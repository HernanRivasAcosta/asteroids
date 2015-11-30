-module(asteroids_app).
-author('hernanrivasacosta@gmail.com').

-behaviour(application).

-export([start/2]).
-export([stop/1]).

%%==============================================================================
%% API
%%==============================================================================
start(_Type, _Args) ->
  {ok, Port} = application:get_env(asteroids, port),
  
  Dispatch = {dispatch, cowboy_router:compile(get_dispatch())},
  {ok, _} = cowboy:start_http(http, 100, [{port, Port}], [{env, [Dispatch]}]),

  asteroids_sup:start_link().

stop(_State) ->
  ok.

%%==============================================================================
%% Utils
%%==============================================================================
get_dispatch() ->
  {ok, Path} = application:get_env(asteroids, static_files),
  {ok, Paths} = application:get_env(asteroids, paths),

  [{'_', [{"/ws", ws_handler, []},
          {"/", cowboy_static, {file, Path ++ "/index.html"}} |
          [{P ++ "[...]", cowboy_static, {dir, Path ++ P}} || P <- Paths]]}].