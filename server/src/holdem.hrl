-record(deck, {pid}).
-record(table, {pid}).
-record(game, {pid}).
-record(player, {pid}).
-record(lobby, {pid}).

-define(MAX_PLAYERS, 10).
-define(MIN_PLAYERS, 2).


%% client to server protocols
-record(c2s_join, {table_id}).
-record(c2s_leave, {}).
-record(c2s_list, {}).


%% protocols

%% player to table protocols
-record(p2t_join, {player}).
-record(p2t_leave, {player}).
-record(p2t_start, {}).

%% player to game protocols
-record(p2g_bet, {player, bet}).
-record(p2g_raise, {player, raise}).
-record(p2g_call, {player}).
-record(p2g_check, {player}).
-record(p2g_fold, {player}).
-record(p2g_all_in, {player}).

%% player to lobby protocols
-record(p2l_get_table, {table_id}).
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
-record(g2p_action, {player, action, amount}).
-record(g2p_reward, {reward}).
-record(g2p_state_update, {state}).

%% game to talbe protocols
-record(g2t_finished, {}).
