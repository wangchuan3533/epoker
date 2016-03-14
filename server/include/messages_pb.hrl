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
    table_id = erlang:error({required, table_id})
}).
-endif.

