-module(server_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  {ok, _Pid} = lobby:start_link(),
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/public/[...]", cowboy_static, {dir, "../../../client"}},
      %%{"/[...]", cowboy_static, {priv_dir, server, "public"}},
      %%{"/hello", hello_handler, []},
      {"/echo", ws_echo_handler, []}
    ]}
  ]),
  {ok, _} = cowboy:start_http(my_http_listener, 100, [{port, 8080}],
    [{env, [{dispatch, Dispatch}]}]
  ),
	server_sup:start_link().

stop(_State) ->
	ok.
