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
  %% tableId
  table_id,
  %% lobby ref
  lobby,
  seats = dict:new(),
  
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
  player_id,
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

% broadcast
broadcast(Msg, Seats) ->
  dict:fold(fun(_PlayerId, #seat{player = Player}, ok) -> Player:cast(Msg) end, ok, Seats).
  
batchChangeStatus(PlayerIds, Status, Seats) ->
  lists:foldl(fun(PlayerId, SeatsAcc) ->
    Seat = dict:fetch(PlayerId, SeatsAcc),
    dict:store(PlayerId, Seat#seat{status = Status}, SeatsAcc)
  end, Seats, PlayerIds).

% collect chips from array of seats
collect_by_bet(Boundary, PlayerIds, Seats) ->
  lists:foldl(fun(PlayerId, {CollectedAcc, SeatsAcc}) ->
    Seat = #seat{bet = Bet} = dict:fetch(PlayerId, SeatsAcc),
    {CollectedAcc + min(Boundary, Bet), dict:store(PlayerId, Seat#seat{bet = 0}, SeatsAcc)}
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
      
      % next stage
      NewStateName = next_stage(StateName),
      AddCardNum = card_num(NewStateName) - card_num(StateName),
      NewCards = lists:append(Cards, [Deck:call(get) || _ <- lists:seq(1, AddCardNum)]),
      
      %% collect pots
      {NewPots, Seats1} = collect_all(Pots, {Talked, Folded, AllIned}, Seats),
      
      % update seat status
      Seats2 = batchChangeStatus(Talked, not_talked, Seats1),
      
      {reply, ok, NewStateName, StateData#state{not_talked = lists:reverse(Talked), talked = [], bet = 0, pots = NewPots, cards = NewCards, seats = Seats2}}
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
  WinnerSeat = #seat{chips = Chips} = dict:fetch(WinnerPlayerId, Seats),
  NewWinnerSeat = WinnerSeat#seat{chips = Chips + TotalWin},
  NewSeats1 = dict:store(WinnerPlayerId, NewWinnerSeat, NewSeats),
  {reply, ok, waiting, StateData#state{talked = [], folded = [], all_ined = [], pots = [], seats = NewSeats1}};
  
% all player folded
next(_StataName, StateData)->
  ok = io:format("no winner ~n"),
  {reply, finished, ok, StateData}.

% TODO give rewards, reset table
showdown(StateName, StateData = #state{cards = Cards, deck = Deck}) ->
  NewStateName = river,
  NewCards = lists:append(Cards, [Deck:call(get) || _ <- lists:seq(1, card_num(NewStateName) - card_num(StateName))]),
  ok = io:format("showdown from ~p data ~p ~n", [StateName, StateData]),
  {reply, finished, ok, StateData#state{cards = NewCards}}.
  
% fold
handle_action(#p2t_action{player_id = PlayerId, action = 'ACTION_FOLD'}, StateName, StateData = #state{not_talked = NotTalked, folded = Folded, seats = Seats}) ->
  Seat = #seat{status = Status} = dict:fetch(PlayerId, Seats),
  case Status of
    not_talked ->
      NewSeat = Seat#seat{status = folded},
      next(StateName, StateData#state{not_talked = lists:delete(PlayerId, NotTalked), folded = [PlayerId | Folded], seats = dict:store(PlayerId, NewSeat, Seats)});
    % TODO
    folded ->
      {reply, ok, StateName, StateData}
  end;
  
% check or call
handle_action(#p2t_action{player_id = PlayerId, action = 'ACTION_RAISE', amount = 0}, StateName, StateData = #state{not_talked = NotTalked, talked = Talked, all_ined = AllIned, seats = Seats, bet = Bet}) ->
  case NotTalked of
    [PlayerId | Tail] ->
      Seat = dict:fetch(PlayerId, Seats),
      Cost = Bet - Seat#seat.bet,
      if Seat#seat.chips > Cost ->
        NewSeat = Seat#seat{chips = Seat#seat.chips - Cost, status = talked, bet = Bet},
        next(StateName, StateData#state{seats = dict:store(PlayerId, NewSeat, Seats), not_talked = Tail, talked = [PlayerId | Talked]});
      true ->
        NewSeat = Seat#seat{chips = 0, status = talked, bet = Seat#seat.bet + Seat#seat.chips},
        next(StateName, StateData#state{seats = dict:store(PlayerId, NewSeat, Seats), not_talked = Tail, all_ined = [PlayerId | AllIned]})
      end;
    _Others ->
      {reply, ignored, StateName, StateData}
  end;
  
handle_action(#p2t_action{player_id = PlayerId, action = 'ACTION_RAISE', amount = Amount}, StateName, StateData = #state{not_talked = NotTalked, talked = Talked, all_ined = AllIned, seats = Seats, bet = Bet}) ->
  case NotTalked of
    [PlayerId | Tail] ->
      Seat = dict:fetch(PlayerId, Seats),
      Cost = Bet + Amount - Seat#seat.bet,
      if Seat#seat.chips > Cost ->
        NewSeat = Seat#seat{chips = Seat#seat.chips - Cost, status = talked, bet = Bet + Amount},
        next(StateName, StateData#state{seats = dict:store(PlayerId, NewSeat, Seats), not_talked = lists:append(Tail, lists:reverse(Talked)), talked = [PlayerId], bet = Bet + Amount});
      Seat#seat.chips == Cost ->
        NewSeat = Seat#seat{chips = Seat#seat.chips - Cost, status = all_ined, bet = Seat#seat.bet + Seat#seat.chips},
        next(StateName, StateData#state{seats = dict:store(PlayerId, NewSeat, Seats), not_talked = lists:append(Tail, lists:reverse(Talked)), talked = [], all_ined = [PlayerId | AllIned], bet = Bet + Amount});
      true ->
        {reply, not_enough_chips, StateName, StateData}
      end;
    _Others ->
      {reply, ignored, StateName, StateData}
  end;
  
handle_action(_Action, StateName, StateData) ->
  {reply, ok, StateName, StateData}.

%% gen_fsm callbacks.
init({TableId, Lobby}) ->
  {ok, Timer} = timer:apply_interval(500, gen_fsm, send_event, [self(), tick]),
  {ok, waiting, #state{table_id = TableId, lobby = Lobby, timer = Timer}}.

waiting(tick, StateData = #state{seats = Seats, queue = Queue}) ->
  case dict:size(Seats) < ?MIN_PLAYERS of
    true ->
      {next_state, waiting, StateData};
    false ->
      % notify player game started
      ok = broadcast(#gamestartedntf{}, Seats),
      
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
      SmallBlindSeat = dict:fetch(SmallBlindPlayerId, Seats),
      NewSmallBlindSeat = SmallBlindSeat#seat{chips = SmallBlindSeat#seat.chips - ?SMALL_BLIND, bet = ?SMALL_BLIND},
      Seats1 = dict:store(SmallBlindPlayerId, NewSmallBlindSeat, Seats),
      
      BigBlindSeat = dict:fetch(BigBlindPlayerId, Seats1),
      NewBigBlindSeat = BigBlindSeat#seat{chips = BigBlindSeat#seat.chips - ?BIG_BLIND, bet = ?BIG_BLIND},
      Seats2 = dict:store(BigBlindPlayerId, NewBigBlindSeat, Seats1),
      
      Deck = deck:new(),
      Seats3 = dict:map(fun(_PlayerId, Seat) -> Seat#seat{status = not_talked, cards = [Deck:call(get), Deck:call(get)]} end, Seats2),
      
      {next_state, preflop, StateData#state{seats = Seats3, queue = Queue1, not_talked = Queue3, talked = [], all_ined = [], folded = [], deck = Deck, bet = ?BIG_BLIND, pots = [0]}}
  end;

waiting(Event, StateData) ->
  handle_event(Event, waiting, StateData).
waiting(Event, From, StateData) ->
  handle_sync_event(Event, From, waiting, StateData).

preflop(Event, StateData) ->
  handle_event(Event, preflop, StateData).
preflop(Event, From, StateData) ->
  handle_sync_event(Event, From, preflop, StateData).

flop(Event, StateData) ->
  handle_event(Event, flop, StateData).
flop(Event, From, StateData) ->
  handle_sync_event(Event, From, flop, StateData).

turn(Event, StateData) ->
  handle_event(Event, turn, StateData).
turn(Event, From, StateData) ->
  handle_sync_event(Event, From, turn, StateData).

river(Event, StateData) ->
  handle_event(Event, river, StateData).
river(Event, From, StateData) ->
  handle_sync_event(Event, From, river, StateData).

handle_event(_Event, StateName, StateData) ->
  %% ok = io:format("event ~p~n", [Event]),
  {next_state, StateName, StateData}.

%% player join
handle_sync_event(#p2t_join{player = Player, player_id = PlayerId, name = PlayerName, chips = PlayerChips}, _From, StateName, StateData = #state{table_id = TableId, lobby = Lobby, seats = Seats, buyin = BuyIn, queue = Queue}) ->
  if PlayerChips < BuyIn ->
    {reply, not_enough_chips, StateName, StateData};
  true ->
    case dict:size(Seats) >= ?MAX_PLAYERS of
      true ->
        {reply, full, StateName, StateData};
      false ->
        % if the player exists
        case dict:find(PlayerId, Seats) of
          {ok, _Seat} ->
            {reply, player_exists, StateName, StateData};
          % player not exists
          error ->
            NewSeat = #seat{player = Player, player_id = PlayerId, name = PlayerName, chips = BuyIn},
            NewSeats = dict:store(PlayerId, NewSeat, Seats),
            NewQueue = lists:append(Queue, [PlayerId]),
            
            % notify other players
            PlayerPb = #playerpb{id = PlayerId, name = PlayerName, chips = BuyIn},
            OtherJoinTableNtf = #otherjointablentf{player = PlayerPb},
            ok = broadcast(OtherJoinTableNtf, Seats),
            
            % if the table is full, notify the lobby
            ok = case dict:size(NewSeats) == ?MAX_PLAYERS of
              true -> Lobby:call(#t2l_table_full{table_id = TableId});
              false -> ok
            end,
            
            % reply
            {reply, {ok, {TableId, [#playerpb{id = PlayerId1, name = PlayerName1, chips = PlayerChips1} || {PlayerId1, #seat{player_id = PlayerId1, name = PlayerName1, chips = PlayerChips1}} <- dict:to_list(NewSeats)], BuyIn}}, StateName, StateData#state{seats = NewSeats, queue = NewQueue}}
        end
    end
  end;

%% player leave
handle_sync_event(#p2t_leave{player_id = PlayerId}, _From, StateName, StateData = #state{table_id = TableId, lobby = Lobby, seats = Seats, queue = Queue}) ->
  case dict:find(PlayerId, Seats) of
    {ok, #seat{player_id = PlayerId, name = _Name, chips = Chips}} ->
      NewSeats = dict:erase(PlayerId, Seats),
      NewQueue = lists:delete(PlayerId, Queue),
      
      % notify other players
      OtherLeaveTableNtf = #otherleavetablentf{player_id = PlayerId},
      ok = broadcast(OtherLeaveTableNtf, NewSeats),
      
      % if the table if full before player leave, now it's not full, notify the lobby
      ok = case dict:size(Seats) == ?MAX_PLAYERS of
        true -> Lobby:call(#t2l_table_not_full{table_id = TableId});
        false -> ok
      end,
      
      % reply
      {reply, {ok, Chips}, StateName, StateData#state{seats = NewSeats, queue = NewQueue}};
    % player not exists in the table
    error ->
      {reply, player_not_exists, StateName, StateData}
  end;
  
% player action
handle_sync_event(Msg = #p2t_action{player_id = PlayerId, action = Action, amount = Amount}, _From, StateName, StateData = #state{seats = Seats}) ->
  ok = broadcast(#otheractionntf{player_id = PlayerId, action = Action, amount = Amount}, Seats),
  handle_action(Msg, StateName, StateData);

%% dump
handle_sync_event(dump, _From, StateName, StateData) ->
  {reply, {StateName, StateData}, StateName, StateData};

handle_sync_event(_Event, _From, StateName, StateData) ->
  {reply, ignored, StateName, StateData}.

handle_info(_Info, StateName, StateData) ->
  {next_state, StateName, StateData}.

terminate(Reason, _StateName, #state{table_id = TableId, lobby = Lobby, timer = Timer}) ->
  ok = io:format("table ~p stoped for reason ~p~n", [this(), Reason]),
  ok = Lobby:cast(#t2l_table_stopped{table_id = TableId}),
  {ok, cancel} = timer:cancel(Timer),
  ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.

%% tests
test() ->
  ok.
