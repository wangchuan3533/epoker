-module(test).
-include("holdem.hrl").
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
  P1 = player:new(Lobby),
  P2 = player:new(Lobby),
  {ok, {TableId, _}} = P1:call(#c2s_join_table{table_id = -1}),
  {ok, {TableId, _}} = P2:call(#c2s_join_table{table_id = -1}),
  {ok, {TableId, Table}} = Lobby:call(#p2l_get_table{table_id = TableId}),
  ok = io:format("TableId ~w Table ~w~n", [TableId, Table]),
  [0] = P1:call(#c2s_list_table{}),
  ok = Table:call(start),
  ok = P1:call(#c2s_action{action = ?ACTION_RAISE, amount = 100}),
  ok = P2:call(#c2s_action{action = ?ACTION_RAISE, amount = 0}),
  ok = P1:call(#c2s_leave_game{}),
  ok = P1:call(#c2s_leave_table{}),
  ok = P2:call(#c2s_leave_game{}),
  ok = P2:call(#c2s_leave_table{}),
  ok = P2:stop(),
  ok = P1:stop(),
  ok = Lobby:stop().
