-module(lobby).
-behaviour(gen_server).
-include("holdem.hrl").

%% API.
-export([start_link/0, stop/0, call/1, cast/1, test/0]).
-export([new/0, stop/1, call/2, cast/2]).

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
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
new() ->
	{ok, Pid} = gen_server:start_link(?MODULE, [], []),
  #lobby{pid = Pid}.

call(Msg) ->
  gen_server:call(?MODULE, Msg).
call(Msg, #lobby{pid = Pid}) ->
  gen_server:call(Pid, Msg).

cast(Msg) ->
  gen_server:cast(?MODULE, Msg).
cast(Msg, #lobby{pid = Pid}) ->
  gen_server:cast(Pid, Msg).

stop() ->
  gen_server:stop(?MODULE).
stop(#lobby{pid = Pid}) ->
  gen_server:stop(Pid).

%% gen_server behaviour
init([]) ->
	{ok, #state{full_tables = ets:new(full_tables, [set]), not_full_tables = ets:new(not_full_tables, [set]), max_table_id = 0}}.

handle_call(#p2l_list_tables{}, _From, State = #state{full_tables = FullTables, not_full_tables = NotFullTables}) ->
	{reply, ets:tab2list(NotFullTables) ++ ets:tab2list(FullTables), State};

handle_call(#p2l_get_table{table_id = -1}, _From, State = #state{not_full_tables = NotFullTables, max_table_id = MaxTableId}) ->
  case ets:first(NotFullTables) of
    '$end_of_table' ->
      Entry = {MaxTableId, table:new(MaxTableId)},
      true = ets:insert(NotFullTables, Entry),
	    {reply, {ok, Entry}, State#state{max_table_id = MaxTableId + 1}};
    TableId ->
      [Entry] = ets:lookup(NotFullTables, TableId),
	    {reply, {ok, Entry}, State}
  end;
handle_call(#p2l_get_table{table_id = TableId}, _From, State = #state{full_tables = FullTables, not_full_tables = NotFullTables}) ->
  Ret = case ets:lookup(FullTables, TableId) ++ ets:lookup(NotFullTables, TableId) of
    [Entry] ->
      {ok, Entry};
    [] ->
      no_such_table;
    _ ->
      more_than_one_table
  end,
	{reply, Ret, State};

handle_call(#t2l_table_full{table_id = TableId}, _From, State = #state{full_tables = FullTables, not_full_tables = NotFullTables}) ->
  [Entry] = ets:take(NotFullTables, TableId),
  true = ets:insert(FullTables, Entry),
	{reply, ok, State};
handle_call(#t2l_table_not_full{table_id = TableId}, _From, State = #state{full_tables = FullTables, not_full_tables = NotFullTables}) ->
  [Entry] = ets:take(FullTables, TableId),
  true = ets:insert(NotFullTables, Entry),
	{reply, ok, State};


handle_call(dump, _From, State = #state{full_tables = FullTables, not_full_tables = NotFullTables, max_table_id = MaxTableId}) ->
  {reply, {ets:tab2list(FullTables), ets:tab2list(NotFullTables), MaxTableId}, State};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(#t2l_table_stopped{table_id = TableId}, State = #state{full_tables = FullTables, not_full_tables = NotFullTables}) ->
  true = ets:delete(FullTables, TableId),
  true = ets:delete(NotFullTables, TableId),
	{noreply, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
  io:format("lobby ~w stoped.~n", [self()]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

test() ->
  test_table_full().

test_table_full() ->
  Lobby = lobby:new(),
  {ok, {TableId1, Table1}} = Lobby:call(#p2l_get_table{table_id = -1}),
  {[], [{TableId1, Table1}], 1} = Lobby:call(dump),
  ok = Lobby:call(#t2l_table_full{table_id = TableId1}),
  {[{TableId1, Table1}], [], 1} = Lobby:call(dump),
  ok = Lobby:call(#t2l_table_not_full{table_id = TableId1}),
  {[], [{TableId1, Table1}], 1} = Lobby:call(dump),
  ok = Table1:stop(),
  ok = Lobby:stop(),
  ok.
