-define(MAX_PLAYERS, 10).
-define(MIN_PLAYERS, 2).

-define(SMALL_BLIND, 100).
-define(BIG_BLIND, 200).
-define(INIT_CHIPS, 10000).
-define(RECORD_TO_TUPLELIST(Rec, Ref), lists:zip(record_info(fields, Rec),tl(tuple_to_list(Ref)))).

-record(deck, {pid}).
-record(table, {pid}).
-record(game, {pid}).
-record(player, {pid}).
-record(lobby, {pid}).
-record(player_db, {id, password, name, chips = ?INIT_CHIPS}).

%% protocols

%% player to table protocols
-record(p2t_join, {player, player_id, name, chips}).
-record(p2t_leave, {player, player_id}).
-record(p2t_action, {player, player_id, action, amount = 0}).

%% player to lobby protocols
-record(p2l_get_table, {table_id = -1}).
-record(p2l_list_tables, {}).

%% table to player protocols
-record(t2p_chat, {player, text}).
-record(t2p_action, {player, action, amount}).
-record(t2p_reward, {reward}).
-record(t2p_state_update, {state}).

%% table to game protocols

%% table to lobby protocols
-record(t2l_table_full, {table_id}).
-record(t2l_table_not_full, {table_id}).
-record(t2l_table_stopped, {table_id}).
