-module(table).
-behaviour(gen_fsm).
-include("holdem.hrl").
-include("messages_pb.hrl").

%% API.
-export([new/1, stop/1, call/2, cast/2]).

%% test
-export([dump/1, test/0]).

%% gen_fsm.
-export([init/1, init_game/1]).
-export([waiting/2, preflop/2, flop/2, turn/2, river/2, finished/2]).
-export([waiting/3, preflop/3, flop/3, turn/3, river/3, finished/3]).

-export([handle_event/3]).
-export([handle_sync_event/4]).
-export([handle_info/3]).
-export([terminate/3]).
-export([code_change/4]).

-record(state, {
  id,
  lobby,
  seats = [],
  buyin = 1000,
  timer,

  %% game state
  not_talked = [],
  talked = [],
  all_ined = [],
  folded = [],
  table,
  deck,
  pots = [],
  cards = [],
  bet = 0
}).

-record(seat, {
  player,
  id,
  name,
  chips,
  cards = [],
  bet = 0
}).

%% API.
new(Opts) ->
  {ok, Pid} = gen_fsm:start_link(?MODULE, Opts, []),
  #table{pid = Pid}.

stop(#table{pid = Pid}) ->
  gen_fsm:stop(Pid).

call(Msg, #table{pid = Pid}) ->
  gen_fsm:sync_send_event(Pid, Msg).

cast(Msg, #table{pid = Pid}) ->
  gen_fsm:send_event(Pid, Msg).

this() ->
  #table{pid = self()}.

%% internal functions

card_num(preflop) -> 0;
card_num(flop) -> 3;
card_num(turn) -> 4;
card_num(river) -> 5.

next_stage(preflop) -> flop;
next_stage(flop) -> turn;
next_stage(turn) -> river;
next_stage(river) -> finished.

collect_by_bet(Bet, Seats) ->
  Collected = lists:foldl(fun(#seat{bet = Bet1}, Sum) -> Sum + min(Bet, Bet1) end, 0, Seats),
  NewSeats = [Seat#seat{bet = 0} || Seat = #seat{} <- Seats],
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
    FirstAllIn = lists:nth(AllIned, NewAllInedNum),
    Bet = FirstAllIn#seat.bet,
    {Collected1, Talked1} = collect_by_bet(Bet, Talked),
    {Collected2, Folded1} = collect_by_bet(Bet, Folded),
    {Collected3, AllIned1} = collect_by_bet(Bet, AllIned),
    [Pot | OtherPots] = Pots,
    Pots1 = [0, Pot + Collected1 + Collected2 + Collected3 | OtherPots],
    collect_all(Pots1, {Talked1, Folded1, AllIned1})
  end.

next(StateName, StateData = #state{not_talked = NotTalked}) when length(NotTalked) > 0 ->
  {reply, ok, StateName, StateData};

next(StateName, StateData = #state{talked = Talked, folded = Folded, all_ined = AllIned, pots = Pots, cards = Cards, deck = Deck}) when length(Talked) > 1 ->
  case StateName of
    finished -> {stop, finished, ok, StateData};
    river -> showdown(StateName, StateData);
    _ ->
      %% collect pots and go to next round
      {Pots1, {Talked1, Folded1, AllIned1}} = collect_all(Pots, {Talked, Folded, AllIned}),
      NewStateName = next_stage(StateName),
      NewCards = lists:append(Cards, [Deck:call(get) || _ <- lists:seq(1, card_num(NewStateName) - card_num(StateName))]),
      {reply, ok, NewStateName, StateData#state{not_talked = lists:reverse(Talked1), talked = [], folded = Folded1, all_ined = AllIned1, bet = 0, pots = Pots1, cards = NewCards}}
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
  {stop, finished, ok, StateData#state{talked = Talked2, folded = Folded1, all_ined = AllIned2, pots = []}};

next(_StataName, StateData = #state{talked = [], all_ined = []}) ->
  ok = io:format("no winner ~n"),
  {stop, finished, ok, StateData};

next(StateName, StateData) ->
  {reply, ok, StateName, StateData}.

showdown(StateName, StateData = #state{cards = Cards, deck = Deck}) ->
  NewStateName = river,
  NewCards = lists:append(Cards, [Deck:call(get) || _ <- lists:seq(1, card_num(NewStateName) - card_num(StateName))]),
  ok = io:format("showdown from ~p data ~p ~n", [StateName, StateData]),
  {stop, finished, ok, StateData#state{cards = NewCards}}.

init_game([{Players}]) ->
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
  {ok, preflop, #state{not_talked = Seats4, deck = Deck, bet = ?BIG_BLIND, pots = [0]}}.
%% gen_fsm.


init({Id, Lobby}) ->
  {ok, Timer} = timer:send_interval(1000, self(), tick),
  {ok, waiting, #state{id = Id, lobby = Lobby, timer = Timer}}.

waiting(start, StateData = #state{seats = Seats}) ->
  if length(Seats) < ?MIN_PLAYERS ->
    io:format("not enough players~n"),
    {next_state, waiting, StateData};
  true ->
    %% notify player game started
    ok = lists:foreach(fun(#seat{player = Player}) ->
      ok = Player:call(#g2p_started{game = this()})
    end, Seats),

    %% first is dealer
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
    {next_state, preflop, StateData#state{not_talked = Seats4, deck = Deck, bet = ?BIG_BLIND, pots = [0]}}
  end;
waiting(Event, StateData) ->
  handle_event(Event, waiting, StateData).
waiting(Event, From, StateData) ->
  handle_sync_event(Event, From, waiting, StateData).

preflop(Event, StateData) ->
  handle_event(Event, preflop, StateData).
preflop(Action = #p2g_action{}, _From, StateData) ->
  handle_action(Action, preflop, StateData);
preflop(Event, From, StateData) ->
  handle_sync_event(Event, From, preflop, StateData).

flop(Event, StateData) ->
  handle_event(Event, flop, StateData).
flop(Action = #p2g_action{}, _From, StateData) ->
  handle_action(Action, flop, StateData);
flop(Event, From, StateData) ->
  handle_sync_event(Event, From, flop, StateData).

turn(Event, StateData) ->
  handle_event(Event, turn, StateData).
turn(Action = #p2g_action{}, _From, StateData) ->
  handle_action(Action, turn, StateData);
turn(Event, From, StateData) ->
  handle_sync_event(Event, From, turn, StateData).

river(Event, StateData) ->
  handle_event(Event, river, StateData).
river(Action = #p2g_action{}, _From, StateData) ->
  handle_action(Action, river, StateData);
river(Event, From, StateData) ->
  handle_sync_event(Event, From, river, StateData).

finished(Event, StateData) ->
  handle_event(Event, finished, StateData).
finished(_Event, _From, StateData) ->
  {stop, finished, ok, StateData}.

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
          next(StateName, StateData#state{talked = NewTalked, folded = NewFolded});
        false -> case lists:keyfind(Player, #seat.player, NotTalked) of
          Seat = #seat{} ->
            NewNotTalked = lists:delete(Seat, NotTalked),
            NewFolded = [Seat | Folded],
            next(StateName, StateData#state{not_talked = NewNotTalked, folded = NewFolded});
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
      next(StateName, StateData#state{talked = NewTalked, not_talked = NewNotTalked, bet = NewBet});
    NextTalkChips == BetCost -> %% all in
      NewNextTalk = NextTalk#seat{chips = NextTalkChips - BetCost, bet = NewBet},
      NewAllIned = [NewNextTalk | AllIned],
      NewNotTalked = lists:append(OtherNotTalked, lists:reverse(Talked)),
      next(StateName, StateData#state{talked = [], not_talked = NewNotTalked, all_ined = NewAllIned, bet = NewBet});
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
      next(StateName, StateData#state{talked = NewTalked, not_talked = OtherNotTalked, bet = Bet});
    true -> %% NextTalkChips <= BetCost
      NewNextTalk = NextTalk#seat{chips = 0, bet = NextTalkBet + NextTalkChips},
      NewAllIned = [NewNextTalk | AllIned],
      next(StateName, StateData#state{all_ined = NewAllIned, not_talked = OtherNotTalked, bet = Bet})
    end;
  true -> %% Player != NextTalkPlayer
    {reply, ignored, StateName, StateData}
  end.

handle_event(Event, StateName, StateData) ->
  ok = io:format("event ~p~n", [Event]),
  {next_state, StateName, StateData}.

%% add player
handle_sync_event(#p2t_join{player = Player, id = PlayerId, name = PlayerName, chips = PlayerChips}, _From, StateName, StateData = #state{id = Id, lobby = Lobby, seats = Seats, buyin = BuyIn}) ->
  if PlayerChips < BuyIn ->
    {reply, not_enough_chips, StateName, StateData};
  true ->
    if length(Seats) >= ?MAX_PLAYERS ->
      {reply, full, StateName, StateData};
    true ->
      ok = if length(Seats) == ?MAX_PLAYERS - 1 ->
        ok = Lobby:call(#t2l_table_full{table_id = Id});
      true -> ok
      end,
      %% notify other players
      PlayerPb = #playerpb{id = PlayerId, name = PlayerName, chips = BuyIn},
      OtherJoinTableNtf = #otherjointablentf{player = PlayerPb},
      [ok = P:notice(OtherJoinTableNtf) || #seat{player = P} <- Seats],
      NewSeat = #seat{player = Player, id = PlayerId, name = PlayerName, chips = BuyIn},
      NewSeats = [NewSeat | Seats],
      {reply, {ok, {Id, [#playerpb{id = PlayerId1, name = PlayerName1, chips = PlayerChips1} || #seat{id = PlayerId1, name = PlayerName1, chips = PlayerChips1} <- NewSeats], BuyIn}}, StateName, StateData#state{seats = NewSeats}}
    end
  end;

%% del player
handle_sync_event(#p2t_leave{player = Player}, _From, StateName, StateData = #state{id = Id, lobby = Lobby, seats = Seats}) ->
  ok = if
    length(Seats) == ?MAX_PLAYERS ->
      Lobby:call(#t2l_table_not_full{table_id = Id});
    true ->
      ok
  end,
  case lists:keytake(Player, #seat.player, Seats) of
    {value, #seat{player = Player, id = PlayerId, name = _Name, chips = Chips}, NewSeats} ->
      % notify other players
      OtherLeaveTableNtf = #otherleavetablentf{player_id = PlayerId},
      [ok = P:notice(OtherLeaveTableNtf) || #seat{player = P} <- NewSeats],
      {reply, {ok, Chips}, StateName, StateData#state{seats = NewSeats}};
    false ->
      {reply, not_in_table, StateName, StateData}
  end;

%% dump
handle_sync_event(dump, _From, StateName, StateData) ->
  {reply, {StateName, StateData}, StateName, StateData};

handle_sync_event(_Event, _From, StateName, StateData) ->
  {reply, ignored, StateName, StateData}.

handle_info(tick, waiting, StateData) ->
  waiting(start, StateData);
handle_info(_Info, StateName, StateData) ->
  {next_state, StateName, StateData}.

terminate(Reason, _StateName, #state{id = Id, lobby = Lobby, timer = Timer}) ->
  ok = io:format("table ~p stoped for reason ~p~n", [this(), Reason]),
  ok = Lobby:cast(#t2l_table_stopped{table_id = Id}),
  {ok, cancel} = timer:cancel(Timer),
  ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.

%% tests
dump(#table{pid = Pid}) ->
  gen_fsm:sync_send_all_state_event(Pid, dump).

test() ->
  ok.
