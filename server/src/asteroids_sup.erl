-module(asteroids_sup).
-author('hernanrivasacosta@gmail.com').

-behaviour(supervisor).

-export([start_link/0, init/1]).

%%==============================================================================
%% API
%%==============================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Specs = [{game, {game, start_link, [[]]}, permanent, 2000, worker, [game]}],
  {ok, {{one_for_one, 10, 10}, Specs}}.