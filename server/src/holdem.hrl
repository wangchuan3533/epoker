-define(MAX_PLAYERS, 10).
-define(MIN_PLAYERS, 2).

-define(ACTION_RAISE, 1).
-define(ACTION_FOLD, 2).

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


%% client to server protocols
-record(c2s_join_table, {table_id = -1}).
-record(c2s_leave_table, {}).
-record(c2s_list_table, {}).
-record(c2s_leave_game, {}).
-record(c2s_action, {action, amount = 0}).

%% protocols

%% player to table protocols
-record(p2t_join, {player}).
-record(p2t_leave, {player}).

%% player to game protocols
-record(p2g_action, {player, action, amount = 0}).

%% player to lobby protocols
-record(p2l_get_table, {table_id = -1}).
-record(p2l_list_tables, {}).

%% table to player protocols

-record(t2p_join, {player}).
-record(t2p_leave, {player}).
-record(t2p_chat, {player, text}).

%% table to game protocols

%% table to lobby protocols
-record(t2l_table_full, {table_id}).
-record(t2l_table_not_full, {table_id}).
-record(t2l_table_stopped, {table_id}).

%% game to player protocols
-record(g2p_started, {game}).
-record(g2p_finished, {game}).
-record(g2p_action, {player, action, amount}).
-record(g2p_reward, {reward}).
-record(g2p_state_update, {state}).

%% game to talbe protocols
-record(g2t_finished, {game}).
