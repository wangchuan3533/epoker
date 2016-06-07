-module(table).
-behaviour(gen_fsm).
-include("holdem.hrl").
-include("messages_pb.hrl").

%% api.
-export([new/1, stop/1, call/2, cast/2]).

%% test
-export([test/0]).

%% gen_fsm.
-export([init/1]).
-export([waiting/2, preflop/2, flop/2, turn/2, river/2]).
-export([waiting/3, preflop/3, flop/3, turn/3, river/3]).

-export([handle_event/3]).
-export([handle_sync_event/4]).
-export([handle_info/3]).
-export([terminate/3]).
-export([code_change/4]).

-record(state, {
  id,
  lobby,
  seats = #{},
  queue = [],
  buyin = 1000,
  timer,

  %% game state
  not_talked = [],
  talked = [],
  all_ined = [],
  folded = [],
  deck,
  pots = [],
  cards = [],
  bet = 0
}).

-record(seat, {
  status,
  player,
  id,
  name,
  chips,
  cards = [],
  bet = 0
}).

%% public api.
new(Opts) ->
  {ok, Pid} = gen_fsm:start_link(?MODULE, Opts, []),
  #table{pid = Pid}.

stop(#table{pid = Pid}) ->
  gen_fsm:stop(Pid).

call(Msg, #table{pid = Pid}) ->
  gen_fsm:sync_send_event(Pid, Msg).

cast(Msg, #table{pid = Pid}) ->
  gen_fsm:send_event(Pid, Msg).

% internal functions
this() ->
  #table{pid = self()}.
  
card_num(preflop) -> 0;
card_num(flop) -> 3;
card_num(turn) -> 4;
card_num(river) -> 5.

next_stage(preflop) -> flop;
next_stage(flop) -> turn;
next_stage(turn) -> river;
next_stage(river) -> showdown.

% collect chips from array of seats
collect_by_bet(Boundary, PlayerIds, Seats) ->
  lists:foldl(fun(PlayerId, {CollectedAcc, SeatsAcc}) ->
    Seat = #seat{bet = Bet} = maps:get(PlayerId, SeatsAcc),
    {CollectedAcc + min(Boundary, Bet), SeatsAcc#{PlayerId := Seat#seat{bet = 0}}}
  end, {0, Seats}, PlayerIds).

% collect all seats before next stage
collect_all(Pots, {Talked, Folded, AllIned}, Seats) ->
  NewAllInedNum = 1 + length(AllIned) - length(Pots),
  Boundary = if NewAllInedNum > 0 ->
    #seat{bet = Bet} = lists:nth(AllIned, NewAllInedNum),
    Bet;
  true -> infinity
  end,
  
  % collect seats's chips to the last pot
  {Collected, NewSeats} = collect_by_bet(Boundary, lists:append([Talked, Folded, AllIned]), Seats),
  [Head | Tail] = Pots,
  Pots1 = [Head + Collected | Tail],
  
  % create new side pot if new player allined
  if NewAllInedNum > 0 ->
    collect_all([0 | Pots], {Talked, Folded, AllIned}, NewSeats);
  true ->
    {Pots1, NewSeats}
  end.

% when there has some one not talked, continue the current stage
next(StateName, StateData = #state{not_talked = NotTalked}) when length(NotTalked) > 0 ->
  {reply, ok, StateName, StateData};

% all the players have talked, and more than one player have talked, so go to next stage
next(StateName, StateData = #state{talked = Talked, folded = Folded, all_ined = AllIned, pots = Pots, cards = Cards, seats = Seats, deck = Deck}) when length(Talked) > 1 ->
  case StateName of
    % river is the last stage, go to showdown
    river -> showdown(StateName, StateData);
    _ ->
      %% collect pots
      {NewPots, NewSeats} = collect_all(Pots, {Talked, Folded, AllIned}, Seats),
      % new cards
      NewStateName = next_stage(StateName),
      AddCardNum = card_num(NewStateName) - card_num(StateName),
      NewCards = lists:append(Cards, [Deck:call(get) || _ <- lists:seq(1, AddCardNum)]),
      {reply, ok, NewStateName, StateData#state{not_talked = lists:reverse(Talked), talked = [], bet = 0, pots = NewPots, cards = NewCards, seats = NewSeats}}
  end;
%% more than one player playing
next(StateName, StateData = #state{talked = Talked, all_ined = AllIned}) when length(Talked) + length(AllIned) > 1 ->
  showdown(StateName, StateData);

% only one player active
next(_StataName, StateData = #state{talked = Talked, folded = Folded, all_ined = AllIned, pots = Pots, seats = Seats}) when length(Talked) + length(AllIned) == 1 ->
  {NewPots, NewSeats} = collect_all(Pots, {Talked, Folded, AllIned}, Seats),
  [WinnerPlayerId] = lists:append(Talked, AllIned),
  TotalWin = lists:sum(NewPots),
  ok = io:format("winner ~p win ~p~n", [WinnerPlayerId, TotalWin]),
  WinnerSeat = #seat{chips = Chips} = maps:get(WinnerPlayerId, Seats),
  NewWinnerSeat = WinnerSeat#seat{chips = Chips + TotalWin},
  NewSeats1 = NewSeats#{WinnerPlayerId := NewWinnerSeat},
  {reply, ok, waiting, StateData#state{talked = [], folded = [], all_ined = [], pots = [], seats = NewSeats1}};
  
% all player folded
next(_StataName, StateData = #state{})->
  ok = io:format("no winner ~n"),
  {reply, finished, ok, StateData}.

% TODO give rewards, reset table
showdown(StateName, StateData = #state{cards = Cards, deck = Deck}) ->
  NewStateName = river,
  NewCards = lists:append(Cards, [Deck:call(get) || _ <- lists:seq(1, card_num(NewStateName) - card_num(StateName))]),
  ok = io:format("showdown from ~p data ~p ~n", [StateName, StateData]),
  {reply, finished, ok, StateData#state{cards = NewCards}}.
  
% fold
handle_action(#p2t_action{id = PlayerId, action = ?ACTION_FOLD}, StateName, StateData = #state{not_talked = NotTalked, folded = Folded, seats = Seats}) ->
  Seat = #seat{status = Status} = maps:get(PlayerId, Seats),
  case Status of
    not_talked ->
      NewSeat = Seat#seat{status = folded},
      next(StateName, StateData#state{not_talked = lists:delete(PlayerId, NotTalked), folded = [PlayerId | Folded], seats = Seats#{PlayerId := NewSeat}});
    _Others ->
      {reply, wrong_status, StateName, StateData}
  end;
  
% check or call
handle_action(#p2t_action{id = PlayerId, action = ?ACTION_RAISE, amount = 0}, StateName, StateData = #state{not_talked = NotTalked, talked = Talked, all_ined = AllIned, seats = Seats, bet = Bet}) ->
  case NotTalked of
    [PlayerId | Tail] ->
      Seat = maps:get(PlayerId, Seats),
      Cost = Bet - Seat#seat.bet,
      if Seat#seat.chips > Cost ->
        NewSeat = Seat#seat{chips = Seat#seat.chips - Cost, status = talked, bet = Bet},
        next(StateName, StateData#state{seats = Seats#{PlayerId := NewSeat}, not_talked = Tail, talked = [PlayerId | Talked]});
      true ->
        NewSeat = Seat#seat{chips = 0, status = talked, bet = Seat#seat.bet + Seat#seat.chips},
        next(StateName, StateData#state{seats = Seats#{PlayerId := NewSeat}, not_talked = Tail, all_ined = [PlayerId | AllIned]})
      end;
    _Others ->
      {reply, ignored, StateName, StateData}
  end;
  
handle_action(#p2t_action{id = PlayerId, action = ?ACTION_RAISE, amount = Amount}, StateName, StateData = #state{not_talked = NotTalked, talked = Talked, all_ined = AllIned, seats = Seats, bet = Bet}) ->
  case NotTalked of
    [PlayerId | Tail] ->
      Seat = maps:get(PlayerId, Seats),
      Cost = Bet + Amount - Seat#seat.bet,
      if Seat#seat.chips > Cost ->
        NewSeat = Seat#seat{chips = Seat#seat.chips - Cost, status = talked, bet = Bet + Amount},
        next(StateName, StateData#state{seats = Seats#{PlayerId := NewSeat}, not_talked = lists:append(Tail, lists:reverse(Talked)), talked = [PlayerId], bet = Bet + Amount});
      Seat#seat.chips == Cost ->
        NewSeat = Seat#seat{chips = Seat#seat.chips - Cost, status = all_ined, bet = Seat#seat.bet + Seat#seat.chips},
        next(StateName, StateData#state{seats = Seats#{PlayerId := NewSeat}, not_talked = lists:append(Tail, lists:reverse(Talked)), talked = [], all_ined = [PlayerId | AllIned], bet = Bet + Amount});
      true ->
        {reply, not_enough_chips, StateName, StateData}
      end;
    _Others ->
      {reply, ignored, StateName, StateData}
  end;
  
handle_action(_Action, StateName, StateData) ->
  {reply, ok, StateName, StateData}.

%% gen_fsm callbacks.
init({Id, Lobby}) ->
  {ok, Timer} = timer:send_interval(1000, self(), tick),
  {ok, waiting, #state{id = Id, lobby = Lobby, timer = Timer}}.

waiting(start, StateData = #state{seats = Seats, queue = Queue}) ->
  if map_size(Seats) < ?MIN_PLAYERS ->
    {next_state, waiting, StateData};
  true ->
    % notify player game started
    ok = maps:fold(fun(_PlayerId, #seat{player = Player}, ok) -> Player:call(#t2p_started{}) end, ok, Seats),
    
    % TODO kick players without enough chips
    
    % DEALER
    [DealerPlayerId | Tail1] = Queue,
    Queue1 = lists:append(Tail1, [DealerPlayerId]),
    
    % small blind
    [SmallBlindPlayerId | Tail2] = Queue1,
    Queue2 = lists:append(Tail2, [SmallBlindPlayerId]),
    
    % big blind
    [BigBlindPlayerId | Tail3] = Queue2,
    Queue3 = lists:append(Tail3, [BigBlindPlayerId]),
    
    % blinds
    SmallBlindSeat = maps:get(SmallBlindPlayerId, Seats),
    NewSmallBlindSeat = SmallBlindSeat#seat{chips = SmallBlindSeat#seat.chips - ?SMALL_BLIND, bet = ?SMALL_BLIND},
    BigBlindSeat = maps:get(BigBlindPlayerId, Seats),
    NewBigBlindSeat = BigBlindSeat#seat{chips = BigBlindSeat#seat.chips - ?BIG_BLIND, bet = ?BIG_BLIND},
    Seats1 = Seats#{SmallBlindPlayerId := NewSmallBlindSeat, BigBlindPlayerId := NewBigBlindSeat},
    
    Deck = deck:new(),
    Seats2 = maps:map(fun(_PlayerId, Seat) -> Seat#seat{cards = [Deck:call(get), Deck:call(get)]} end, Seats1),
    {next_state, preflop, StateData#state{seats = Seats2, queue = Queue1, not_talked = Queue3, talked = [], all_ined = [], folded = [], deck = Deck, bet = ?BIG_BLIND, pots = [0]}}
  end;

waiting(Event, StateData) ->
  handle_event(Event, waiting, StateData).
waiting(Event, From, StateData) ->
  handle_sync_event(Event, From, waiting, StateData).

preflop(Event, StateData) ->
  handle_event(Event, preflop, StateData).
preflop(Action = #p2t_action{}, _From, StateData) ->
  handle_action(Action, preflop, StateData);
preflop(Event, From, StateData) ->
  handle_sync_event(Event, From, preflop, StateData).

flop(Event, StateData) ->
  handle_event(Event, flop, StateData).
flop(Action = #p2t_action{}, _From, StateData) ->
  handle_action(Action, flop, StateData);
flop(Event, From, StateData) ->
  handle_sync_event(Event, From, flop, StateData).

turn(Event, StateData) ->
  handle_event(Event, turn, StateData).
turn(Action = #p2t_action{}, _From, StateData) ->
  handle_action(Action, turn, StateData);
turn(Event, From, StateData) ->
  handle_sync_event(Event, From, turn, StateData).

river(Event, StateData) ->
  handle_event(Event, river, StateData).
river(Action = #p2t_action{}, _From, StateData) ->
  handle_action(Action, river, StateData);
river(Event, From, StateData) ->
  handle_sync_event(Event, From, river, StateData).

handle_event(Event, StateName, StateData) ->
  ok = io:format("event ~p~n", [Event]),
  {next_state, StateName, StateData}.

%% player join
handle_sync_event(#p2t_join{player = Player, id = PlayerId, name = PlayerName, chips = PlayerChips}, _From, StateName, StateData = #state{id = Id, lobby = Lobby, seats = Seats, buyin = BuyIn, queue = Queue}) ->
  if PlayerChips < BuyIn ->
    {reply, not_enough_chips, StateName, StateData};
  true ->
    if map_size(Seats) >= ?MAX_PLAYERS ->
      {reply, full, StateName, StateData};
    true ->
      % if the player exists
      case maps:find(PlayerId, Seats) of
        {ok, _Seat} ->
          {reply, player_exists, StateName, StateData};
        % player not exists
        error ->
          NewSeat = #seat{player = Player, id = PlayerId, name = PlayerName, chips = BuyIn},
          NewSeats = Seats#{PlayerId => NewSeat},
          NewQueue = lists:append(Queue, [PlayerId]),
          
          % notify other players
          PlayerPb = #playerpb{id = PlayerId, name = PlayerName, chips = BuyIn},
          OtherJoinTableNtf = #otherjointablentf{player = PlayerPb},
          ok = maps:fold(fun(_OtherPlayerId, #seat{player = OtherPlayer}, ok) -> OtherPlayer:notice(OtherJoinTableNtf) end, ok, Seats),
          
          % if the table is full, notify the lobby
          ok = if map_size(NewSeats) == ?MAX_PLAYERS ->
            Lobby:call(#t2l_table_full{table_id = Id});
          true -> ok
          end,
          
          % reply
          {reply, {ok, {Id, [#playerpb{id = PlayerId1, name = PlayerName1, chips = PlayerChips1} || #seat{id = PlayerId1, name = PlayerName1, chips = PlayerChips1} <- maps:values(NewSeats)], BuyIn}}, StateName, StateData#state{seats = NewSeats, queue = NewQueue}}
      end
    end
  end;

%% player leave
handle_sync_event(#p2t_leave{id = PlayerId}, _From, StateName, StateData = #state{id = Id, lobby = Lobby, seats = Seats, queue = Queue}) ->
  case maps:find(PlayerId, Seats) of
    {ok, #seat{id = PlayerId, name = _Name, chips = Chips}} ->
      NewSeats = maps:remove(PlayerId, Seats),
      NewQueue = lists:delete(PlayerId, Queue),
      
      % notify other players
      OtherLeaveTableNtf = #otherleavetablentf{player_id = PlayerId},
      ok = maps:fold(fun(_OtherPlayerId, #seat{player = OtherPlayer}, ok) -> OtherPlayer:notice(OtherLeaveTableNtf) end, ok, NewSeats),
      
      % if the table if full before player leave, now it's not full, notify the lobby
      ok = if map_size(Seats) == ?MAX_PLAYERS ->
          Lobby:call(#t2l_table_not_full{table_id = Id});
      true -> ok
      end,
      
      % reply
      {reply, {ok, Chips}, StateName, StateData#state{seats = NewSeats, queue = NewQueue}};
    % player not exists in the table
    error ->
      {reply, player_not_exists, StateName, StateData}
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
test() ->
  ok.
