-module(storage).
-include_lib("stdlib/include/qlc.hrl").
-include("holdem.hrl").
-compile(export_all).

init() ->
  mnesia:create_table(user, [{attributes, record_info(fields, user)}, {disc_copies, [node()]}]).
  
get(Uid) ->
  case mnesia:transaction(fun() -> mnesia:read(user, Uid, read) end) of
    {atomic, [User]} -> {ok, User};
    {atomic, []} -> {error, user_not_exist}
  end.
  
set(User = #user{}) ->
  {atomic, ok} = mnesia:transaction(fun() -> mnesia:write(User) end),
  ok.

test() ->
  {atomic, ok} = test_write(),
  {atomic, [User]} = test_read(),
  ok = io:format("User 1 is ~p~n", [User]),
  {atomic, ok} = test_clear().
  
test_write() ->
  mnesia:transaction(fun() -> mnesia:write(#user{id = 1, name = user1, chips = 100000}) end),
  mnesia:transaction(fun() -> mnesia:write(#user{id = 2, name = user2, chips = 100000}) end),
  mnesia:transaction(fun() -> mnesia:write(#user{id = 3, name = user3, chips = 100000}) end),
  mnesia:transaction(fun() -> mnesia:write(#user{id = 4, name = user4, chips = 100000}) end),
  mnesia:transaction(fun() -> mnesia:write(#user{id = 5, name = user5, chips = 100000}) end),
  mnesia:transaction(fun() -> mnesia:write(#user{id = 6, name = user6, chips = 100000}) end),
  mnesia:transaction(fun() -> mnesia:write(#user{id = 7, name = user7, chips = 100000}) end).
test_read() ->
  mnesia:transaction(fun() -> mnesia:read(user, 1, read) end).
test_clear() ->
  mnesia:clear_table(user).
test_list() ->
  mnesia:transaction(fun() ->
    Q = qlc:q([U || U <- mnesia:table(user)]),
    qlc:e(Q)
  end).