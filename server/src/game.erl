-module(game).
-behaviour(gen_fsm).
-include("holdem.hrl").

%% API.
-export([new/1, stop/1, call/2, cast/2]).

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
cast(Msg, #game{pid = Pid}) ->
  gen_fsm:send_event(Pid, Msg).
this() ->
  #game{pid = self()}.

init([{Players, Table, Dealer}]) ->
  ok = lists:foreach(fun(Player) ->
    ok = Player:cast(#g2p_started{game = this()})
  end, Players),
  Seats = dict:from_list(lists:map(fun(Player) -> {Player, #seat{}} end, Players)),
	{ok, preflop, #state{seats = Seats, table = Table, dealer = Dealer, deck = deck:new()}}.

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

handle_sync_event(#p2g_action{player = Player, action = Action, amount = Amount}, _From, _StateName, StateData = #state{}) ->
  io:format("player ~w action ~w amount ~w~n", [Player, Action, Amount]),
	{reply, ok, finished, StateData};

%% dump
handle_sync_event(dump, _From, StateName, StateData) ->
	{reply, {StateName, StateData}, StateName, StateData};

handle_sync_event(_Event, _From, StateName, StateData) ->
	{reply, ignored, StateName, StateData}.

handle_info(_Info, StateName, StateData) ->
	{next_state, StateName, StateData}.

terminate(_Reason, _StateName, #state{deck = Deck, table = Table, seats = Seats}) ->
  ok = Deck:stop(),
  ok = dict:fold(fun(Player, Seat, ok) ->
    ok = io:format("player ~w seat cleanup ~w~n", [Player, Seat]),
    Player:cast(#g2p_finished{game = this()})
  end, ok, Seats),
  ok = Table:cast(#g2t_finished{}),
  io:format("game ~w stoped.~n", [this()]),
	ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

%% tests
dump(#game{pid = Pid}) ->
  gen_fsm:sync_send_all_state_event(Pid, dump).

test() ->
  test_deck().
test_deck() ->
  L = lobby:new(),
  T = table:new({0, L}),
  P1 = player:new(L),
  P2 = player:new(L),
  G = game:new({[P1, P2], T, 1}),
  ok = io:format("~w~n", [G:dump()]),
  ok = G:stop(),
  ok = P1:stop(),
  ok = P2:stop(),
  ok = T:stop(),
  ok = L:stop(),
  ok.
