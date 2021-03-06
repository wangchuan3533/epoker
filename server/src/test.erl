-module(test).
-include("holdem.hrl").
-include("messages_pb.hrl").
-export([test/0]).

test() ->
  ok = deck:test(),
  ok = io:format("deck test passed~n"),
  ok = game:test(),
  ok = io:format("game test passed~n"),
  ok = hand:test(),
  ok = io:format("hand test passed~n"),
  ok = lobby:test(),
  ok = io:format("lobby test passed~n"),
  ok = player:test(),
  ok = io:format("player test passed~n"),
  ok = table:test(),
  ok = io:format("table test passed~n"),
  ok = test1(),
  ok = io:format("test1 test passed~n").

test1() ->
  Lobby = lobby:new(),
  U1 = #player_db{id = 1, name = 1},
  U2 = #player_db{id = 2, name = 2},
  P1 = player:new({U1, Lobby}),
  P2 = player:new({U2, Lobby}),
  {ok, {TableId, _}} = P1:call(#jointablereq{table_id = -1}),
  {ok, {TableId, _}} = P2:call(#jointablereq{table_id = -1}),
  {ok, {TableId, Table}} = Lobby:call(#p2l_get_table{table_id = TableId}),
  ok = io:format("TableId ~w Table ~w~n", [TableId, Table]),
  [0] = P1:call(#listtablereq{}),
  ok = Table:call(start),
  ok = P1:call(#actionreq{action = 'ACTION_RAISE', amount = 100}),
  ok = P2:call(#actionreq{action = 'ACTION_RAISE', amount = 0}),
  ok = P1:call(#leavegamereq{}),
  ok = P1:call(#leavetablereq{}),
  ok = P2:call(#leavegamereq{}),
  ok = P2:call(#leavetablereq{}),
  ok = P2:stop(),
  ok = P1:stop(),
  ok = Lobby:stop().
