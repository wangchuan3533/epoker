-module(lobby).
-behaviour(gen_server).

%% API.
-export([start_link/0, call/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
  full_tables,
  not_full_tables,
  max_table_id
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
call(Msg) ->
  gen_server:call(?MODULE, Msg).
%% gen_server behaviour
init([]) ->
	{ok, #state{full_tables = ets:new(full_tables, [set]), not_full_tables = ets:new(not_full_tables, [set]), max_table_id = 0}}.

handle_call(list_tables, _From, State = #state{full_tables = FullTables, not_full_tables = NotFullTables}) ->
	{reply, ets:tab2list(NotFullTables) ++ ets:tab2list(FullTables), State};

handle_call(get_available_table, _From, State = #state{not_full_tables = NotFullTables, max_table_id = MaxTableId}) ->
  case ets:first(NotFullTables) of
    '$end_of_table' ->
      true = ets:insert(NotFullTables, {MaxTableId, table:new()}),
	    {reply, MaxTableId, State#state{max_table_id = MaxTableId + 1}};
    TableId ->
      [_Table] = ets:lookup(NotFullTables, TableId),
	    {reply, TableId, State}
  end;
handle_call({get_table, TableId}, _From, State = #state{full_tables = FullTables, not_full_tables = NotFullTables}) ->
  Ret = case ets:lookup(FullTables, TableId) ++ ets:lookup(NotFullTables, TableId) of
    [{TableId, Table}] ->
      {ok, Table};
    [] ->
      no_such_table;
    _ ->
      more_than_one_table
  end,
	{reply, Ret, State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
