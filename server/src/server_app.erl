-module(server_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  {ok, _Pid} = lobby:start_link(),
  Dispatch = cowboy_router:compile([
    {'_', [
      %%{"/", index_handler, []},
      {"/", index_handler, []},
      {"/ws/:token", ws_handler, []},
      {"/login", login_handler, []},
      {"/profile/:uid", profile_handler, []},
      {"/proto/[...]", cowboy_static, {dir, "../../../proto", [{mimetypes, {<<"text">>, <<"plain">>, []}}]}},
      {"/[...]", cowboy_static, {dir, "../../../react-js"}}
    ]}
  ]),
  {ok, _} = cowboy:start_http(my_http_listener, 100, [{port, 8080}],
    [{env, [{dispatch, Dispatch}]}]
  ),
	server_sup:start_link().

stop(_State) ->
	ok.
