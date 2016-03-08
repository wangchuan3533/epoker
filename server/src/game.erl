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
  not_talked_seats = undefined,
  talked_seats = [],
  table = undefined,
  deck = undefined,
  pots = [],
  bet = 0
}).

-record(seat, {
  player = undefined,
  cards = [],
  bet = 0,
  folded = false,
  chips = 10000
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


%% functions
next(preflop) -> flop;
next(flop) -> turn;
next(turn) -> river;
next(river) -> finished.

%%
init([{Players, Table}]) ->
  ok = lists:foreach(fun(Player) ->
    ok = Player:call(#g2p_started{game = this()})
  end, Players),
  
  Seats = lists:map(fun(Player) -> #seat{player = Player} end, Players),
  [H | T] = Seats,
  Seats1 = lists:append(T, [H]),
  
  %% small blind
  [SmallBlindSeat | T1] = Seats1,
  Chips1 = SmallBlindSeat#seat.chips - ?SMALL_BLIND,
  NewSmallBlindSeat = SmallBlindSeat#seat{chips = Chips1, bet = ?SMALL_BLIND},
  Seats2 = lists:append(T1, [NewSmallBlindSeat]),
  
  %% big blind
  [BigBlindSeat | T2] = Seats2,
  Chips2 = BigBlindSeat#seat.chips - ?BIG_BLIND,
  NewBigBlindSeat = BigBlindSeat#seat{chips = Chips2, bet = ?BIG_BLIND},
  Seats3 = lists:append(T2, [NewBigBlindSeat]),
  
  %% preflop cards
  Deck = deck:new(),
  ok = io:format("seats3 ~w~n", [Seats3]),
  Seats4 = lists:map(fun(Seat) -> Seat#seat{cards = [Deck:call(get), Deck:call(get)]} end, Seats3),
	{ok, preflop, #state{not_talked_seats = Seats4, table = Table, deck = Deck}}.

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

handle_sync_event(#p2g_action{player = Player, action = Action, amount = Amount}, _From, StateName, StateData = #state{not_talked_seats = NotTalkedSeats, talked_seats = TalkedSeats}) ->
  ok = io:format("player ~w action ~w amount ~w~n", [Player, Action, Amount]),
  [NextTalkSeat = #seat{player = NextTalkPlayer} | OtherNotTalkedSeats] = NotTalkedSeats,
  case Player of
    NextTalkPlayer ->
      NewTalkedSeats = [NextTalkSeat | TalkedSeats],
      case OtherNotTalkedSeats of
        [] ->
	        {reply, ok, next(StateName), StateData#state{not_talked_seats = lists:reverse(NewTalkedSeats), talked_seats = []}};
        _NotEmptyList ->
	        {reply, ok, StateName, StateData#state{not_talked_seats = OtherNotTalkedSeats, talked_seats = NewTalkedSeats}}
      end;
    _Other ->
	    {reply, ignored, StateName, StateData}
  end;

%% dump
handle_sync_event(dump, _From, StateName, StateData) ->
	{reply, {StateName, StateData}, StateName, StateData};

handle_sync_event(_Event, _From, StateName, StateData) ->
	{reply, ignored, StateName, StateData}.

handle_info(_Info, StateName, StateData) ->
	{next_state, StateName, StateData}.

terminate(_Reason, _StateName, #state{deck = Deck, table = Table, not_talked_seats = Seats1, talked_seats = Seats2}) ->
  ok = Deck:stop(),
  ok = lists:foreach(fun(#seat{player = Player}) ->
    Player:call(#g2p_finished{game = this()})
  end, lists:append(Seats1, Seats2)),
  ok = Table:call(#g2t_finished{game = this()}),
  ok = io:format("game ~w stoped.~n", [this()]),
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
  P1 = player:new(L),
  P2 = player:new(L),
  TableId = P1:call(#c2s_join_table{}),
  TableId = P2:call(#c2s_join_table{}),
  {ok, {TableId, T}} = L:call(#p2l_get_table{table_id = TableId}),
  ok = T:call(start),
  {playing, {state, _, _, _, _, G}} = T:dump(),
  ok = io:format("~w~n", [G:dump()]),
  ok = G:stop(),
  ok = P1:stop(),
  ok = P2:stop(),
  ok = T:stop(),
  ok = L:stop(),
  ok.
