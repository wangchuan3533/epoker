-module(storage).
-include_lib("stdlib/include/qlc.hrl").
-include("holdem.hrl").
-compile(export_all).

init() ->
  mnesia:create_table(player_db, [{attributes, record_info(fields, player_db)}, {disc_copies, [node()]}]).

get(Uid) ->
  case mnesia:transaction(fun() -> mnesia:read(player_db, Uid, read) end) of
    {atomic, [Player]} -> {ok, Player};
    {atomic, []} -> {error, player_not_exist}
  end.

set(Player = #player_db{}) ->
  {atomic, ok} = mnesia:transaction(fun() -> mnesia:write(Player) end),
  ok.

incr_chips(Player = #player_db{chips = Chips}, Diff) ->
  NewPlayer = Player#player_db{chips = Chips + Diff},
  {atomic, ok} = mnesia:transaction(fun() -> mnesia:write(NewPlayer) end),
  NewPlayer.

test() ->
  {atomic, ok} = test_write(),
  {atomic, [Player]} = test_read(),
  ok = io:format("Player 1 is ~p~n", [Player]),
  {atomic, ok} = test_clear().
create_test_players() ->
  mnesia:transaction(fun() -> mnesia:write(#player_db{id = <<"t1">>, password = <<"t1">>, name = <<"t1">>, chips = 100000}) end),
  mnesia:transaction(fun() -> mnesia:write(#player_db{id = <<"t2">>, password = <<"t2">>, name = <<"t2">>, chips = 100000}) end),
  mnesia:transaction(fun() -> mnesia:write(#player_db{id = <<"t3">>, password = <<"t3">>, name = <<"t3">>, chips = 100000}) end),
  mnesia:transaction(fun() -> mnesia:write(#player_db{id = <<"t4">>, password = <<"t4">>, name = <<"t4">>, chips = 100000}) end).

test_write() ->
  mnesia:transaction(fun() -> mnesia:write(#player_db{id = 1, password = 1, name = player1, chips = 100000}) end),
  mnesia:transaction(fun() -> mnesia:write(#player_db{id = 2, password = 1, name = player2, chips = 100000}) end),
  mnesia:transaction(fun() -> mnesia:write(#player_db{id = 3, password = 1, name = player3, chips = 100000}) end),
  mnesia:transaction(fun() -> mnesia:write(#player_db{id = 4, password = 1, name = player4, chips = 100000}) end),
  mnesia:transaction(fun() -> mnesia:write(#player_db{id = 5, password = 1, name = player5, chips = 100000}) end),
  mnesia:transaction(fun() -> mnesia:write(#player_db{id = 6, password = 1, name = player6, chips = 100000}) end),
  mnesia:transaction(fun() -> mnesia:write(#player_db{id = 7, password = 1, name = player7, chips = 100000}) end).
test_read() ->
  mnesia:transaction(fun() -> mnesia:read(player_db, 1, read) end).
test_clear() ->
  mnesia:clear_table(player_db).
test_list() ->
  mnesia:transaction(fun() ->
    Q = qlc:q([U || U <- mnesia:table(player_db)]),
    qlc:e(Q)
  end).
