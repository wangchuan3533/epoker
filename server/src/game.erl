-module(game).
-behaviour(gen_fsm).
-include("holdem.hrl").

%% API.
-export([new/1, stop/1, call/2]).

%% test
-export([dump/1, test/0]).

%% gen_fsm.
-export([init/1]).
-export([preflop/2, flop/2, turn/2, river/2, finished/2]).
-export([preflop/3, flop/3, turn/3, river/3, finished/3]).
-export([handle_event/3]).
-export([handle_sync_event/4]).
-export([handle_info/3]).
-export([terminate/3]).
-export([code_change/4]).

-record(state, {
  seats = undefined,
  table = undefined,
  deck = undefined,
  pots = [],
  buy_in = 4000,
  small_blind = 100,
  big_blind = 200,
  dealer = 0,
  turn = 0,
  bet = 0
}).

-record(seat, {
  cards = [],
  bet = 0,
  talked = false,
  folded = false
}).

%% API.
new(Opt) ->
  {ok, Pid} = gen_fsm:start_link(?MODULE, [Opt], []),
  #game{pid = Pid}.

stop(#game{pid = Pid}) ->
  gen_fsm:stop(Pid).

call(Msg, #game{pid = Pid}) ->
  gen_fsm:sync_send_event(Pid, Msg).

init([{Players, Table}]) ->
  Seats = dict:from_list(lists:map(fun(Player) -> {Player, #seat{}} end, Players)),
	{ok, preflop, #state{seats = Seats, table = Table, deck = deck:new()}}.

preflop(_Event, StateData) ->
	{next_state, preflop, StateData}.

preflop(next, _From, StateData) ->
	{reply, ok, flop, StateData};
preflop(Event, From, StateData) ->
  handle_sync_event(Event, From, preflop, StateData).

flop(_Event, StateData) ->
	{next_state, flop, StateData}.

flop(next, _From, StateData) ->
	{reply, ok, turn, StateData};
flop(Event, From, StateData) ->
  handle_sync_event(Event, From, flop, StateData).

turn(_Event, StateData) ->
	{next_state, turn, StateData}.

turn(next, _From, StateData) ->
	{reply, ok, river, StateData};
turn(Event, From, StateData) ->
  handle_sync_event(Event, From, turn, StateData).

river(_Event, StateData) ->
	{next_state, river, StateData}.

river(next, _From, StateData = #state{table = Table}) ->
  ok = Table:cast(#g2t_finished{}),
	{reply, ok, finished, StateData};
river(Event, From, StateData) ->
  handle_sync_event(Event, From, river, StateData).

finished(_Event, StateData) ->
	{next_state, finished, StateData}.

finished(Event, From, StateData) ->
  handle_sync_event(Event, From, finished, StateData).

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
  io:format("game ~w stoped.~n", [self()]),
	ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

%% tests
dump(#game{pid = Pid}) ->
  gen_fsm:sync_send_all_state_event(Pid, dump).

test() ->
  test_deck().
test_deck() ->
  T = table:new(0),
  P1 = player:new(),
  P2 = player:new(),
  G = game:new({[P1, P2], T}),
  ok = io:format("~w~n", [G:dump()]),
  ok = G:stop(),
  ok = P1:stop(),
  ok = P2:stop(),
  ok = T:stop(),
  ok.
