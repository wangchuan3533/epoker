-module(game).
-behaviour(gen_fsm).
-include("holdem.hrl").

%% API.
-export([new/0, stop/1]).

%% test
-export([dump/1, test/0]).

%% gen_fsm.
-export([init/1]).
-export([preflop/2]).
-export([preflop/3]).
-export([handle_event/3]).
-export([handle_sync_event/4]).
-export([handle_info/3]).
-export([terminate/3]).
-export([code_change/4]).

-record(state, {
  players = [],
  seats = [],
  pots = [],
  buy_in = 4000,
  small_blind = 100,
  big_blind = 200,
  dealer = 0,
  turn = 0,
  bet = 0
}).

%% API.
-spec new() -> #game{}.
new() ->
  {ok, Pid} = gen_fsm:start_link(?MODULE, [], []),
  #game{pid = Pid}.

stop(#game{pid = Pid}) ->
  gen_fsm:stop(Pid).

%% gen_fsm.

init([]) ->
	{ok, preflop, #state{}}.

preflop(_Event, StateData) ->
	{next_state, preflop, StateData}.

preflop(_Event, _From, StateData) ->
	{reply, ignored, preflop, StateData}.

handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.

%% dump
handle_sync_event(dump, _From, StateName, StateData) ->
	{reply, {StateName, StateData}, StateName, StateData};

handle_sync_event(_Event, _From, StateName, StateData) ->
	{reply, ignored, StateName, StateData}.

handle_info(_Info, StateName, StateData) ->
	{next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
	ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

%% tests
dump(#game{pid = Pid}) ->
  gen_fsm:sync_send_all_state_event(Pid, dump).

test() ->
  test_no_op().

test_no_op() -> ok.
