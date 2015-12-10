-module(hand).
-export([init/0, eval/1]).

init() ->
  ok = erlang:load_nif("priv/server_drv", 0).
eval(_) ->
  "Nif library not loaded".
