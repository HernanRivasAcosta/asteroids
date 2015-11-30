-module(ws_handler).

-export([init/2]).
-export([websocket_handle/3, websocket_info/3, terminate/3]).

init(Req, Opts) ->
  {cowboy_websocket, Req, Opts}.

websocket_handle({text, <<"connect:", Name/binary>>}, Req, State) ->
  case game:player_connected(self(), Name) of
    {ok, Reply} -> {reply, {text, Reply}, Req, State};
    _Error      -> {stop, Req, State}
  end;
websocket_handle({text, Json}, Req, State) ->
  % Parse the event, if this fails, the client will be disconnected
  {Event} = jiffy:decode(Json),
  % Get the local time of the player (used to calculate ping clientside)
  {value, {_, Time}, NewEvent} = lists:keytake(<<"time">>, 1, Event),
  % Handle the input
  ok = game:player_input(self, NewEvent),
  % The reply only has the client time
  {reply, {text, jiffy:encode({[{action, reply},
                                {time, Time}]})}, Req, State}.

websocket_info({message, Message}, Req, State) ->
  {reply, {text, Message}, Req, State};
websocket_info(Msg, Req, State) ->
  _ = lager:info("unexpected message received ~p", [Msg]),
  {ok, Req, State}.

terminate(Reason, _Req, _State) ->
  _ = lager:info("connection terminated, reason: ~p", [Reason]),
  ok = game:player_disconnected(self()).