lobby:call({p2l_list_tables})
erl -mnesia dir '"/tmp/epoker"' -name 'server@127.0.0.1'
mnesia:create_schema([node()]).
mnesia:start().
mnesia:stop().
halt().
