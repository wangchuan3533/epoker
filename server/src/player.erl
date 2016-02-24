-module(player).
-behaviour(gen_fsm).
-include("holdem.hrl").

%% API.
-export([new/0, stop/1, call/2]).
-export([dump/1, test/0]).

%% gen_fsm.
-export([init/1]).
-export([in_lobby/2, in_table/2, in_game/2]).
-export([in_lobby/3, in_table/3, in_game/3]).
-export([handle_event/3]).
-export([handle_sync_event/4]).
-export([handle_info/3]).
-export([terminate/3]).
-export([code_change/4]).

-record(state, {
  table = undefined,
  game = undefined,
  chips = 10000
}).

%% API.
new() ->
	{ok, Pid} = gen_fsm:start_link(?MODULE, [], []),
  #player{pid = Pid}.

stop(#player{pid = Pid}) ->
  gen_fsm:stop(Pid).

call(Msg, #player{pid = Pid}) ->
  gen_fsm:sync_send_event(Pid, Msg).

%% gen_fsm.
init([]) ->
	{ok, in_lobby, #state{}}.

in_lobby(_Event, StateData) ->
	{next_state, in_lobby, StateData}.

in_lobby(#{<<"type">> := <<"join">>, <<"tableId">> := TableId}, _From, StateData) when TableId >= 0 ->
  {ok, Table} = lobby:call({get_table, TableId}),
  ok = Table:call(#p2t_join{player = #player{pid = self()}}),
  NewStateData = StateData#state{table = Table},
	{reply, TableId, in_table, NewStateData};
in_lobby(JsonMap = #{<<"type">> := <<"join">>}, From, StateData) ->
  TableId = lobby:call(get_available_table),
  in_lobby(maps:put(<<"tableId">>, TableId, JsonMap), From, StateData);

in_lobby(Event, From, StateData) ->
  handle_sync_event(Event, From, in_lobby, StateData).

in_table(_Event, StateData) ->
	{next_state, in_table, StateData}.

in_table(#{<<"type">> := <<"leave">>}, _From, StateData = #state{table = Table}) ->
  ok = Table:call(#p2t_leave{player = #player{pid = self()}}),
  NewStateData = StateData#state{table = undefined},
	{reply, ok, in_lobby, NewStateData};
in_table({game_started, Game}, _From, StateData) ->
  NewStateData = StateData#state{game = Game},
	{reply, ok, in_game, NewStateData};
in_table(Event, From, StateData) ->
  handle_sync_event(Event, From, in_table, StateData).

in_game(_Event, StateData) ->
	{next_state, in_game, StateData}.
in_game(Event, From, StateData) ->
  handle_sync_event(Event, From, in_game, StateData).

handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.

%% dump
handle_sync_event(dump, _From, StateName, StateData) ->
	{reply, {StateName, StateData}, StateName, StateData};

handle_sync_event(#{<<"type">> := <<"list_tables">>}, _From, StateName, StateData) ->
  TableIds = lists:map(fun({K, _V}) -> K end, lobby:call(list_tables)),
	{reply, TableIds, StateName, StateData};
  
handle_sync_event(_Event, _From, StateName, StateData) ->
	{reply, ignored, StateName, StateData}.

handle_info(_Info, StateName, StateData) ->
	{next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
  io:format("player ~w stoped.~n", [self()]),
	ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

%% test

dump(#player{pid = Pid}) ->
  gen_fsm:sync_send_all_state_event(Pid, dump).

test() ->
  test_join().

test_join() ->
  P = player:new(),
  {in_lobby, #state{table = undefined}} = P:dump(),
  T = table:new(),
  P:join(T),
  {in_table, #state{table = G}} = P:dump(),
  ok = P:leave(),
  G:stop(),
  P:stop().
