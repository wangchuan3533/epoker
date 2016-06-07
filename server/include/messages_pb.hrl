-ifndef(MESSAGE_PB_H).
-define(MESSAGE_PB_H, true).
-record(message, {
    type = erlang:error({required, type}),
    data = erlang:error({required, data})
}).
-endif.

-ifndef(JOINTABLEREQ_PB_H).
-define(JOINTABLEREQ_PB_H, true).
-record(jointablereq, {
    table_id = erlang:error({required, table_id})
}).
-endif.

-ifndef(JOINTABLERES_PB_H).
-define(JOINTABLERES_PB_H, true).
-record(jointableres, {
    errno = erlang:error({required, errno}),
    table = erlang:error({required, table})
}).
-endif.

-ifndef(LEAVETABLEREQ_PB_H).
-define(LEAVETABLEREQ_PB_H, true).
-record(leavetablereq, {
    
}).
-endif.

-ifndef(LEAVETABLERES_PB_H).
-define(LEAVETABLERES_PB_H, true).
-record(leavetableres, {
    errno = erlang:error({required, errno})
}).
-endif.

-ifndef(LISTTABLEREQ_PB_H).
-define(LISTTABLEREQ_PB_H, true).
-record(listtablereq, {
    
}).
-endif.

-ifndef(LISTTABLERES_PB_H).
-define(LISTTABLERES_PB_H, true).
-record(listtableres, {
    errno = erlang:error({required, errno}),
    table_ids = []
}).
-endif.

-ifndef(LEAVEGAMEREQ_PB_H).
-define(LEAVEGAMEREQ_PB_H, true).
-record(leavegamereq, {
    
}).
-endif.

-ifndef(LEAVEGAMERES_PB_H).
-define(LEAVEGAMERES_PB_H, true).
-record(leavegameres, {
    errno = erlang:error({required, errno})
}).
-endif.

-ifndef(ACTIONREQ_PB_H).
-define(ACTIONREQ_PB_H, true).
-record(actionreq, {
    action = erlang:error({required, action}),
    amount = erlang:error({required, amount})
}).
-endif.

-ifndef(ACTIONRES_PB_H).
-define(ACTIONRES_PB_H, true).
-record(actionres, {
    errno = erlang:error({required, errno})
}).
-endif.

-ifndef(OTHERJOINTABLENTF_PB_H).
-define(OTHERJOINTABLENTF_PB_H, true).
-record(otherjointablentf, {
    player = erlang:error({required, player})
}).
-endif.

-ifndef(OTHERLEAVETABLENTF_PB_H).
-define(OTHERLEAVETABLENTF_PB_H, true).
-record(otherleavetablentf, {
    player_id = erlang:error({required, player_id})
}).
-endif.

-ifndef(OTHERACTIONNTF_PB_H).
-define(OTHERACTIONNTF_PB_H, true).
-record(otheractionntf, {
    player_id = erlang:error({required, player_id}),
    action = erlang:error({required, action}),
    amount = erlang:error({required, amount})
}).
-endif.

-ifndef(GAMESTARTEDNTF_PB_H).
-define(GAMESTARTEDNTF_PB_H, true).
-record(gamestartedntf, {
    
}).
-endif.

-ifndef(GAMEFINISHEDNTF_PB_H).
-define(GAMEFINISHEDNTF_PB_H, true).
-record(gamefinishedntf, {
    
}).
-endif.

-ifndef(PLAYERPB_PB_H).
-define(PLAYERPB_PB_H, true).
-record(playerpb, {
    id = erlang:error({required, id}),
    name = erlang:error({required, name}),
    chips = erlang:error({required, chips})
}).
-endif.

-ifndef(TABLEPB_PB_H).
-define(TABLEPB_PB_H, true).
-record(tablepb, {
    id = erlang:error({required, id}),
    players = []
}).
-endif.

