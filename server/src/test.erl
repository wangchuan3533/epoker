-module(test).
-include("holdem.hrl").
-export([test/0]).

test() ->
  ok = deck:test(),
  ok = game:test(),
  ok = hand:test(),
  ok = lobby:test(),
  ok = player:test(),
  ok = table:test(),
  ok = test1().

test1() ->
  Lobby = lobby:new(),
  P1 = player:new(Lobby),
  P2 = player:new(Lobby),
  TableId = P1:call(#c2s_join_table{table_id = -1}),
  TableId = P2:call(#c2s_join_table{table_id = -1}),
  {ok, {TableId, Table}} = Lobby:call(#p2l_get_table{table_id = TableId}),
  ok = io:format("TableId ~w Table ~w~n", [TableId, Table]),
  [0] = P1:call(#c2s_list_table{}),
  ok = Table:call(start),
  ok = P1:call(#c2s_action{action = ?ACTION_RAISE, amount = 100}),
  ok = P2:call(#c2s_action{action = ?ACTION_RAISE, amount = 100}),
  ok = P1:call(#c2s_leave_game{}),
  ok = P1:call(#c2s_leave_table{}),
  ok = P2:call(#c2s_leave_game{}),
  ok = P2:call(#c2s_leave_table{}),
  ok = P2:stop(),
  ok = P1:stop(),
  ok = Lobby:stop().
