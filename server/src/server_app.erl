-module(server_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  {ok, _Pid} = lobby:start_link(),
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", cowboy_static, {file, "../../../client/index.html"}},
      {"/public/[...]", cowboy_static, {dir, "../../../client"}},
      {"/proto/[...]", cowboy_static, {dir, "../../../proto", [{mimetypes, {<<"text">>, <<"plain">>, []}}]}},
      {"/ws", ws_handler, []},
      {"/profile/:pid", profile_handler, []}
    ]}
  ]),
  {ok, _} = cowboy:start_http(my_http_listener, 100, [{port, 8080}],
    [{env, [{dispatch, Dispatch}]}]
  ),
	server_sup:start_link().

stop(_State) ->
	ok.
