-module(game).
-behaviour(gen_fsm).
-include("holdem.hrl").
-include("messages_pb.hrl").

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
  not_talked = [],
  talked = [],
  all_ined = [],
  folded = [],
  table = undefined,
  deck = undefined,
  pots = [],
  cards = [],
  bet = 0
}).

-record(seat, {
  player = undefined,
  cards = [],
  bet = 0,
  chips = ?INIT_CHIPS
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

collect_by_bet(Bet, Seats) ->
  Collected = lists:foldl(fun(#seat{bet = Bet1}, Sum) -> Sum + min(Bet, Bet1) end, 0, Seats),
  NewSeats = [Seat#seat{bet = Bet1 - min(Bet, Bet1)} || Seat = #seat{bet = Bet1} <- Seats],
  {Collected, NewSeats}.

collect_all(Pots, {Talked, Folded, AllIned}) ->
  NewAllInedNum = 1 + length(AllIned) - length(Pots),
  if NewAllInedNum == 0 ->
    {Collected1, Talked1} = collect_by_bet(infinity, Talked),
    {Collected2, Folded1} = collect_by_bet(infinity, Folded),
    {Collected3, AllIned1} = collect_by_bet(infinity, AllIned),
    [Pot | OtherPots] = Pots,
    Pots1 = [Pot + Collected1 + Collected2 + Collected3 | OtherPots],
    {Pots1, {Talked1, Folded1, AllIned1}};
  true -> %% NewAllInedNum > 0
    SortedAllIned = lists:keysort(#seat.bet, AllIned),
    NewAllIned = lists:sublist(SortedAllIned, length(Pots), NewAllInedNum),

    [FirstAllIn | _OtherAllIns] = NewAllIned,
    Bet = FirstAllIn#seat.bet,
    {Collected1, Talked1} = collect_by_bet(Bet, Talked),
    {Collected2, Folded1} = collect_by_bet(Bet, Folded),
    {Collected3, AllIned1} = collect_by_bet(Bet, AllIned),
    [Pot | OtherPots] = Pots,
    Pots1 = [0, Pot + Collected1 + Collected2 + Collected3 | OtherPots],
    collect_all(Pots1, {Talked1, Folded1, AllIned1})
  end.

card_num(preflop) -> 0;
card_num(flop) -> 3;
card_num(turn) -> 4;
card_num(river) -> 5.

next(preflop) -> flop;
next(flop) -> turn;
next(turn) -> river;
next(river) -> finished.

next(StateName, StateData = #state{not_talked = NotTalked}) when length(NotTalked) > 0 ->
  {StateName, StateData};

next(StateName, StateData = #state{talked = Talked, folded = Folded, all_ined = AllIned, pots = Pots, cards = Cards, deck = Deck}) when length(Talked) > 1 ->
  case StateName of
    finished -> {finished, StateData};
    river -> showdown(StateName, StateData);
    _ ->
      %% collect pots and go to next round
      {Pots1, {Talked1, Folded1, AllIned1}} = collect_all(Pots, {Talked, Folded, AllIned}),
      NewStateName = next(StateName),
      NewCards = lists:append(Cards, [Deck:call(get) || _ <- lists:seq(1, card_num(NewStateName) - card_num(StateName))]),
      {NewStateName, StateData#state{not_talked = lists:reverse(Talked1), talked = [], folded = Folded1, all_ined = AllIned1, bet = 0, pots = Pots1, cards = NewCards}}
  end;

next(StateName, StateData = #state{talked = Talked, all_ined = AllIned}) when length(Talked) + length(AllIned) > 1 ->
  showdown(StateName, StateData);

next(_StataName, StateData = #state{talked = Talked, folded = Folded, all_ined = AllIned, pots = Pots}) when length(Talked) + length(AllIned) == 1 ->
  {Pots1, {Talked1, Folded1, AllIned1}} = collect_all(Pots, {Talked, Folded, AllIned}),
  {Talked2, AllIned2} = case Talked1 of
    [Winner = #seat{chips = Chips}] ->
      {[Winner#seat{chips = Chips + lists:foldl(fun(Pot, Sum) -> Pot + Sum end, 0, Pots1)}], AllIned1};
    [] ->
      [Winner = #seat{chips = Chips}] = AllIned1,
      {Talked1, [Winner#seat{chips = Chips + lists:foldl(fun(Pot, Sum) -> Pot + Sum end, 0, Pots1)}]}
  end,
  ok = io:format("game finished with results ~p~n", [StateData#state{talked = Talked2, folded = Folded1, all_ined = AllIned2, pots = []}]),
  {finished, StateData#state{talked = Talked2, folded = Folded1, all_ined = AllIned2, pots = []}};

next(_StataName, StateData = #state{talked = [], all_ined = []}) ->
  ok = io:format("no winner ~n"),
  {finished, StateData};

next(StateName, StateData) ->
  {StateName, StateData}.

%% to do give awards
showdown(StateName, StateData = #state{cards = Cards, deck = Deck}) ->
  NewStateName = river,
  NewCards = lists:append(Cards, [Deck:call(get) || _ <- lists:seq(1, card_num(NewStateName) - card_num(StateName))]),
  ok = io:format("showdown from ~p data ~p ~n", [StateName, StateData]),
  {finished, StateData#state{cards = NewCards}}.
%%
init([{Players, Table}]) ->
  ok = io:format("~p~n", [{Players, Table}]),
  ok = lists:foreach(fun(Player) ->
    ok = Player:call(#g2p_started{game = this()})
  end, Players),

  Seats = [#seat{player = Player} || Player <- Players],
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
  Seats4 = [Seat#seat{cards = [Deck:call(get), Deck:call(get)]} || Seat <- Seats3],
  {ok, preflop, #state{not_talked = Seats4, table = Table, deck = Deck, bet = ?BIG_BLIND, pots = [0]}}.

preflop(_Event, StateData) ->
  {next_state, preflop, StateData}.
preflop(Action = #p2g_action{}, _From, StateData) ->
  handle_action(Action, preflop, StateData);
preflop(Event, From, StateData) ->
  handle_sync_event(Event, From, preflop, StateData).

flop(_Event, StateData) ->
  {next_state, flop, StateData}.
flop(Action = #p2g_action{}, _From, StateData) ->
  handle_action(Action, flop, StateData);
flop(Event, From, StateData) ->
  handle_sync_event(Event, From, flop, StateData).

turn(_Event, StateData) ->
  {next_state, turn, StateData}.
turn(Action = #p2g_action{}, _From, StateData) ->
  handle_action(Action, turn, StateData);
turn(Event, From, StateData) ->
  handle_sync_event(Event, From, turn, StateData).

river(_Event, StateData) ->
  {next_state, river, StateData}.
river(Action = #p2g_action{}, _From, StateData) ->
  handle_action(Action, river, StateData);
river(Event, From, StateData) ->
  handle_sync_event(Event, From, river, StateData).

finished(_Event, StateData) ->
  {next_state, finished, StateData}.
finished(Event, From, StateData) ->
  handle_sync_event(Event, From, finished, StateData).

handle_action(#p2g_action{player = Player, action = ?ACTION_FOLD}, StateName, StateData = #state{not_talked = NotTalked, talked = Talked, folded = Folded, all_ined = AllIned}) ->
  ok = io:format("player ~w foled, not_talked: ~w, talked: ~w, folded: ~w, all_ined: ~w~n", [Player, NotTalked, Talked, Folded, AllIned]),
  case lists:keyfind(Player, #seat.player, Folded) of
    #seat{} -> {reply, already_folded, StateName, StateData};
    false -> case lists:keyfind(Player, #seat.player, AllIned) of
      #seat{} -> {reply, already_all_in, StateName, StateData};
      false -> case lists:keyfind(Player, #seat.player, Talked) of
        Seat = #seat{} ->
          NewTalked = lists:delete(Seat, Talked),
          NewFolded = [Seat | Folded],
          {NewStateName, NewStateData} = next(StateName, StateData#state{talked = NewTalked, folded = NewFolded}),
          {reply, ok, NewStateName, NewStateData};
        false -> case lists:keyfind(Player, #seat.player, NotTalked) of
          Seat = #seat{} ->
            NewNotTalked = lists:delete(Seat, NotTalked),
            NewFolded = [Seat | Folded],
            {NewStateName, NewStateData} = next(StateName, StateData#state{not_talked = NewNotTalked, folded = NewFolded}),
            {reply, ok, NewStateName, NewStateData};
          false ->
            {reply, ok, not_in_game, StateData}
        end
      end
    end
  end;

%% raise
handle_action(#p2g_action{player = Player, action = ?ACTION_RAISE, amount = Amount}, StateName, StateData = #state{not_talked = NotTalked, talked = Talked, all_ined = AllIned, bet = Bet}) when Amount > 0 ->
  ok = io:format("player ~w action ~w amount ~w~n", [Player, ?ACTION_RAISE, Amount]),
  [NextTalk = #seat{player = NextTalkPlayer, bet = NextTalkBet, chips = NextTalkChips} | OtherNotTalked] = NotTalked,
  if Player == NextTalkPlayer ->
    NewBet = Bet + Amount,
    BetCost = NewBet - NextTalkBet,
    if NextTalkChips > BetCost ->
      NewNextTalk = NextTalk#seat{chips = NextTalkChips - BetCost, bet = NewBet},
      NewTalked = [NewNextTalk],
      NewNotTalked = lists:append(OtherNotTalked, lists:reverse(Talked)),
      {NewStateName, NewStateData} = next(StateName, StateData#state{talked = NewTalked, not_talked = NewNotTalked, bet = NewBet}),
      {reply, ok, NewStateName, NewStateData};
    NextTalkChips == BetCost -> %% all in
      NewNextTalk = NextTalk#seat{chips = NextTalkChips - BetCost, bet = NewBet},
      NewAllIned = [NewNextTalk | AllIned],
      NewNotTalked = lists:append(OtherNotTalked, lists:reverse(Talked)),
      {NewStateName, NewStateData} = next(StateName, StateData#state{talked = [], not_talked = NewNotTalked, all_ined = NewAllIned, bet = NewBet}),
      {reply, ok, NewStateName, NewStateData};
    true -> %% NextTalkChips < BetCost
      {reply, no_enough_chips, StateName, StateData}
    end;
  true -> %% Player != NextTalkPlayer
    {reply, ignored, StateName, StateData}
  end;

%% check or call
handle_action(#p2g_action{player = Player, action = ?ACTION_RAISE, amount = 0}, StateName, StateData = #state{not_talked = NotTalked, talked = Talked, all_ined = AllIned, bet = Bet}) ->
  ok = io:format("player ~w action ~w amount ~w~n", [Player, ?ACTION_RAISE, 0]),
  [NextTalk = #seat{player = NextTalkPlayer, bet = NextTalkBet, chips = NextTalkChips} | OtherNotTalked] = NotTalked,
  if Player == NextTalkPlayer ->
    BetCost = Bet - NextTalkBet,
    if NextTalkChips > BetCost ->
      NewNextTalk = NextTalk#seat{chips = NextTalkChips - BetCost, bet = Bet},
      NewTalked = [NewNextTalk | Talked],
      {NewStateName, NewStateData} = next(StateName, StateData#state{talked = NewTalked, not_talked = OtherNotTalked, bet = Bet}),
      {reply, ok, NewStateName, NewStateData};
    true -> %% NextTalkChips <= BetCost
      NewNextTalk = NextTalk#seat{chips = 0, bet = NextTalkBet + NextTalkChips},
      NewAllIned = [NewNextTalk | AllIned],
      {NewStateName, NewStateData} = next(StateName, StateData#state{all_ined = NewAllIned, not_talked = OtherNotTalked, bet = Bet}),
      {reply, ok, NewStateName, NewStateData}
    end;
  true -> %% Player != NextTalkPlayer
    {reply, ignored, StateName, StateData}
  end.

handle_event(_Event, StateName, StateData) ->
  {next_state, StateName, StateData}.

%% dump
handle_sync_event(dump, _From, StateName, StateData) ->
  {reply, {StateName, StateData}, StateName, StateData};

handle_sync_event(_Event, _From, StateName, StateData) ->
  {reply, ignored, StateName, StateData}.

handle_info(_Info, StateName, StateData) ->
  {next_state, StateName, StateData}.

terminate(Reason, _StateName, #state{deck = Deck, table = Table, not_talked = NotTalked, talked = Talked, folded = Folded, all_ined = AllIned}) ->
  ok = io:format("game ~p stoped for reaseon ~p~n", [this(), Reason]),
  ok = Deck:stop(),
  ok = lists:foreach(fun(#seat{player = Player}) ->
    Player:call(#g2p_finished{game = this()})
  end, lists:append([NotTalked, Talked, Folded, AllIned])),
  ok = Table:call(#g2t_finished{game = this()}),
  ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.

%% tests
dump(#game{pid = Pid}) ->
  gen_fsm:sync_send_all_state_event(Pid, dump).

test() ->
  ok = test_check(),
  ok = test_fold().
test_check() ->
  L = lobby:new(),

  UD1 = #player_db{id = 1, name = 1},
  PD2 = #player_db{id = 2, name = 2},
  P1 = player:new({UD1, L}),
  P2 = player:new({PD2, L}),
  {ok, {TableId, _}} = P1:call(#jointablereq{}),
  {ok, {TableId, _}} = P2:call(#jointablereq{}),
  {ok, {TableId, T}} = L:call(#p2l_get_table{table_id = TableId}),
  ok = T:call(start),
  {playing, {state, _, _, _, _, G}} = T:dump(),
  {preflop, #state{not_talked = [#seat{player = P1, cards = Cards1, chips = Chips1}, #seat{player = P2, cards = Cards2, chips = Chips2}], talked = []}} = G:dump(),
  ok = io:format("Cards1= ~w, Cards2= ~w~n", [Cards1, Cards2]),
  Chips1 = ?INIT_CHIPS - ?SMALL_BLIND,
  Chips2 = ?INIT_CHIPS - ?BIG_BLIND,

  ok = G:call(#p2g_action{player = P1, action = ?ACTION_RAISE, amount = 0}),
  {preflop, #state{not_talked = [#seat{player = P2, cards = Cards2, chips = Chips2}], talked = [#seat{player = P1, cards = Cards1, chips = Chips2}]}} = G:dump(),
  ok = G:call(#p2g_action{player = P2, action = ?ACTION_RAISE, amount = 0}),
  {flop, #state{not_talked = [#seat{player = P1, cards = Cards1, chips = Chips2}, #seat{player = P2, cards = Cards2, chips = Chips2}], talked = []}} = G:dump(),

  ok = G:call(#p2g_action{player = P1, action = ?ACTION_RAISE, amount = 0}),
  {flop, #state{not_talked = [#seat{player = P2, cards = Cards2, chips = Chips2}], talked = [#seat{player = P1, cards = Cards1, chips = Chips2}]}} = G:dump(),
  ok = G:call(#p2g_action{player = P2, action = ?ACTION_RAISE, amount = 0}),
  {turn, #state{not_talked = [#seat{player = P1, cards = Cards1, chips = Chips2}, #seat{player = P2, cards = Cards2, chips = Chips2}], talked = []}} = G:dump(),

  ok = G:call(#p2g_action{player = P1, action = ?ACTION_RAISE, amount = 0}),
  {turn, #state{not_talked = [#seat{player = P2, cards = Cards2, chips = Chips2}], talked = [#seat{player = P1, cards = Cards1, chips = Chips2}]}} = G:dump(),
  ok = G:call(#p2g_action{player = P2, action = ?ACTION_RAISE, amount = 0}),
  {river, #state{not_talked = [#seat{player = P1, cards = Cards1, chips = Chips2}, #seat{player = P2, cards = Cards2, chips = Chips2}], talked = []}} = G:dump(),

  ok = G:call(#p2g_action{player = P1, action = ?ACTION_RAISE, amount = 0}),
  {river, #state{not_talked = [#seat{player = P2, cards = Cards2, chips = Chips2}], talked = [#seat{player = P1, cards = Cards1, chips = Chips2}]}} = G:dump(),
  ok = G:call(#p2g_action{player = P2, action = ?ACTION_RAISE, amount = 0}),
  {finished, #state{talked = [#seat{player = P2, cards = Cards2, chips = Chips2}, #seat{player = P1, cards = Cards1, chips = Chips2}]}} = G:dump(),

  ignored = G:call(#p2g_action{player = P1, action = ?ACTION_RAISE, amount = 0}),
  {finished, #state{talked = [#seat{player = P2, cards = Cards2, chips = Chips2}, #seat{player = P1, cards = Cards1, chips = Chips2}]}} = G:dump(),
  ignored = G:call(#p2g_action{player = P2, action = ?ACTION_RAISE, amount = 0}),
  {finished, #state{talked = [#seat{player = P2, cards = Cards2, chips = Chips2}, #seat{player = P1, cards = Cards1, chips = Chips2}]}} = G:dump(),

  ok = G:stop(),
  ok = P1:stop(),
  ok = P2:stop(),
  ok = T:stop(),
  ok = L:stop().

test_fold() ->
  L = lobby:new(),
  PD1 = #player_db{id = 1, name = 1},
  PD2 = #player_db{id = 2, name = 2},
  P1 = player:new({PD1, L}),
  P2 = player:new({PD2, L}),
  {ok, {TableId, _}} = P1:call(#jointablereq{}),
  {ok, {TableId, _}} = P2:call(#jointablereq{}),
  {ok, {TableId, T}} = L:call(#p2l_get_table{table_id = TableId}),
  ok = T:call(start),
  {playing, {state, _, _, _, _, G}} = T:dump(),
  Chips1 = ?INIT_CHIPS - ?SMALL_BLIND,
  Chips2 = ?INIT_CHIPS - ?BIG_BLIND,
  ok = G:call(#p2g_action{player = P1, action = ?ACTION_FOLD, amount = 0}),
  {preflop, #state{not_talked = [#seat{player = P2, cards = Cards2, chips = Chips2}], folded = [#seat{player = P1, cards = Cards1, chips = Chips1}]}} = G:dump(),
  ok = G:call(#p2g_action{player = P2, action = ?ACTION_RAISE, amount = 0}),
  {finished, #state{talked = [#seat{player = P2, cards = Cards2, chips = ?INIT_CHIPS + ?SMALL_BLIND}], folded = [#seat{player = P1, cards = Cards1, chips = Chips1}]}} = G:dump(),
  ok = G:stop(),
  ok = P1:stop(),
  ok = P2:stop(),
  ok = T:stop(),
  ok = L:stop().
