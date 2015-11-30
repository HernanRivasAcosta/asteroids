-module(game).
-author('hernanrivasacosta@gmail.com').

-behaviour(gen_server).

%% API
-export([player_connected/2, player_disconnected/1, player_input/2]).
% Supervisors
-export([start_link/1]).
% gen_server callbacks
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

-type tick()         :: integer().
-type player_name()  :: binary().
-type position()     :: {float(), float(), float()}. % x, y, and rotation
-type speed()        :: {float(), float()}. % vx, and vy
-type input()        :: boolean(). % Wether the player is accelerating
-type player_state() :: {position(), tick(), position(), input(), speed()}.
-type connection()   :: pid().
-type player()       :: {player_name(), player_state(), connection()}.

-record(state, {tick          = 0 :: tick(),
                players      = [] :: [],
                events       = [] :: [],
                clock = unfefined :: any()}).

-type state() :: #state{}.
-type error() :: {error, atom() | {atom(), any()}}.

-define(WIDTH, 1000.0).
-define(HEIGHT, 600.0).
-define(TICK_RATE, 40).

%%==============================================================================
%% API
%%==============================================================================
-spec start_link(any()) -> {ok, pid()} | ignore | error().
start_link([]) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec player_connected(connection(), player_name()) -> any().
player_connected(Connection, Name) ->
  gen_server:call(?MODULE, {player_connected, Connection, Name}).

-spec player_disconnected(connection()) -> ok.
player_disconnected(Connection) ->
  gen_server:call(?MODULE, {player_disconnected, Connection}).

-spec player_input(connection(), any()) -> ok.
player_input(Connection, Data) ->
  gen_server:call(?MODULE, {player_input, Connection, Data}).

%%==============================================================================
%% gen_server callbacks
%%==============================================================================
-spec handle_call(any(), any(), state()) -> {reply, any(), state()}.
handle_call({player_connected, Connection, Name}, _From, State) ->
  handle_player_connected(Connection, Name, State);
handle_call({player_disconnected, Connection}, _From, State) ->
  handle_player_disconnected(Connection, State);
handle_call({player_input, Connection, Data}, _From, State) ->
  handle_player_input(Connection, Data, State).

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(tick, State) ->
  {noreply, handle_tick(State)};
handle_info(Msg, State) ->
  _ = lager:notice("Unexpected info message received: ~p on ~p", [Msg, State]),
  {noreply, State}.

% Boilerplate
-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) -> {noreply, State}.
-spec terminate(atom(), state()) -> ok.
terminate(_Reason, _State) -> ok.
-spec code_change(string(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%==============================================================================
%% Handlers
%%==============================================================================
init([]) ->
  {ok, TRef} = timer:send_interval(1000 div ?TICK_RATE, tick),
  {ok, #state{clock = TRef}}.

handle_player_connected(Connection, Name, State = #state{tick = Tick,
                                                         players = Players}) ->
  case has_player(Connection, Players) orelse has_player(Name, Players) of
    true  -> {reply, {error, already_connected}, State};
    false -> _ = lager:notice("Player ~p joining", [Name]),
             PlayerState = get_new_state(Tick),
             NewPlayer = {Name, PlayerState, Connection},
             ok = notify_all(build_player_joined(NewPlayer, Tick), Players),
             Reply = build_joined(PlayerState, Players, Tick),
             {reply, {ok, Reply}, State#state{players = [NewPlayer | Players]}}
  end.

handle_player_disconnected(Connection, State = #state{tick = Tick,
                                                      players = Players}) ->
  case get_player(Connection, Players) of
    unfefined    -> {reply, ok, State};
    {Name, _, _} -> _ = lager:notice("Player ~p leaving", [Name]),
                    NewPlayers = remove_player(Name, Players),
                    ok = notify_all(build_player_left(Name, Tick), NewPlayers),
                    NewTick = case NewPlayers of
                                [] -> 0;
                                _  -> Tick
                              end,
                    {reply, ok, State#state{tick = NewTick,
                                            players = NewPlayers}}
  end.

handle_player_input(Connection, Data, State = #state{events = Events,
                                                     players = Players}) ->
  case proplists:get_value(<<"action">>, Data) of
    <<"shoot">> ->
      Tick = proplists:get_value(<<"tick">>, Data),
      {Position} = proplists:get_value(<<"position">>, Data),
      NewEvent = {bullet, Tick, Position},
      {reply, ok, State#state{events = [NewEvent | Events]}};
    <<"hack">> ->
      Tick = proplists:get_value(<<"tick">>, Data),
      {Position} = proplists:get_value(<<"position">>, Data),
      Accelerating = proplists:get_value(<<"accelerating">>, Data),
      {Name, _, _} = get_player(Connection, Players),
      NewEvent = {hack, Tick, Position, Accelerating, Name},
      {reply, ok, State#state{events = [NewEvent | Events]}};
    _ ->
      {Name, _, _} = get_player(Connection, Players),
      _ = lager:warn("unexpected event ~p from ~p", [Data, Name]),
      {reply, ok, State}
  end.

handle_tick(State = #state{players = []}) ->
  % Do nothing if there are no players
  State;
handle_tick(State = #state{tick = Tick, players = Players, events = Events}) ->
  NewTick = Tick + 1,
  ok = notify_all(build_tick(Tick, Players, Events), Players),
  State#state{tick = NewTick, events = []}.

%%==============================================================================
%% Utils
%%==============================================================================
-spec has_player(connection() | player_name(), [player()]) -> boolean().
has_player(Pid, Players) when is_pid(Pid) ->
  lists:any(fun({_Name, _State, Connection}) -> Pid == Connection end, Players);
has_player(Name, Players) ->
  lists:any(fun({PName, _State, _Connection}) -> Name == PName end, Players).

-spec get_player(connection() | player_name(), [player()]) ->
  connection() | player_name().
get_player(_Player, []) ->
  undefined;
get_player(Pid, [Player = {_, _, Pid} | _T]) ->
  Player;
get_player(Name, [Player = {Name, _, _} | _T]) ->
  Player;
get_player(NameOrPid, [_ | T]) ->
  get_player(NameOrPid, T).

remove_player(Player, Players) ->
  remove_player(Player, Players, []).
remove_player(_Player, [], Acc) ->
  Acc;
remove_player(Player, [{Player, _, _} | T], Acc) ->
  Acc ++ T;
remove_player(Player, [{_, _, Player} | T], Acc) ->
  Acc ++ T;
remove_player(Player, [H | T], Acc) ->
  remove_player(Player, T, [H | Acc]).

get_new_state(Tick) ->
  Position = {random:uniform() * ?WIDTH, random:uniform() * ?HEIGHT, 0.0},
  Speed = {0.0, 0.0},
  {Position, Tick, Position, false, Speed}.

notify_all(_Message, []) ->
  ok;
notify_all(Message, [{_, _, Pid} | T]) ->
  Pid ! {message, Message},
  notify_all(Message, T).

%%==============================================================================
%% Message building
%%==============================================================================
build_joined(PlayerState, Players, Tick) ->
  jiffy:encode({[{tick, Tick},
                 {action, <<"joined">>},
                 {position, build_position(PlayerState)},
                 {players, build_players(Players)}]}).

build_player_joined({Name, PlayerState, _}, Tick) ->
  jiffy:encode({[{tick, Tick},
                 {action, <<"playerJoined">>},
                 {name, Name},
                 {position, build_position(PlayerState)}]}).

build_player_left(Name, Tick) ->
  jiffy:encode({[{tick, Tick},
                 {action, <<"playerLeft">>},
                 {name, Name}]}).

build_tick(Tick, _Players, Events) ->
  jiffy:encode({[{tick, Tick},
                 {action, <<"tick">>},
                 {events, build_events(Events)}]}).

%%==============================================================================
%% Message building utils
%%==============================================================================
build_position({{X, Y, R}, _, _, _, _}) ->
  {[{x, X}, {y, Y}, {r, R}]}.

build_players(Players) ->
  build_players(Players, []).

build_players([], Acc) ->
  Acc;
build_players([{Name, PlayerState, _} | T], Acc) ->
  build_players(T, [{[{name, Name},
                      {position, build_position(PlayerState)}]} | Acc]).

build_events(Events) ->
  build_events(Events, []).
build_events([], Acc) ->
  Acc;
build_events([{bullet = Type, Tick, Position} | T], Acc) ->
  X = proplists:get_value(<<"x">>, Position),
  Y = proplists:get_value(<<"y">>, Position),
  R = proplists:get_value(<<"r">>, Position),
  NewEvent = {[{type, Type},
               {tick, Tick},
               {position, {[{x, X}, {y, Y}, {r, R}]}}]},
  build_events(T, [NewEvent | Acc]);
build_events([{hack = Type, Tick, Position, Accelerating, Name} | T], Acc) ->
  X = proplists:get_value(<<"x">>, Position),
  Y = proplists:get_value(<<"y">>, Position),
  R = proplists:get_value(<<"r">>, Position),
  NewEvent = {[{type, Type},
               {tick, Tick},
               {name, Name},
               {accelerating, Accelerating},
               {position, {[{x, X}, {y, Y}, {r, R}]}}]},
  build_events(T, [NewEvent | Acc]).

%%==============================================================================
%% Simulation
%%==============================================================================