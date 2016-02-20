-module(hand).
-export([init/0, eval/1]).

init() ->
  ok = erlang:load_nif("lib/server-0.0.1/priv/server", 0).
eval(_) ->
  "Nif library not loaded".
