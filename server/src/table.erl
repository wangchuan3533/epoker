-module(table).
-behaviour(gen_fsm).
-include("holdem.hrl").
-include("messages_pb.hrl").

%% API.
-export([new/1, stop/1, call/2, cast/2]).

%% test
-export([dump/1, test/0]).

%% gen_fsm.
-export([init/1]).
-export([waiting/2, playing/2]).
-export([waiting/3, playing/3]).
-export([handle_event/3]).
-export([handle_sync_event/4]).
-export([handle_info/3]).
-export([terminate/3]).
-export([code_change/4]).

-record(state, {
  id,
  lobby,
  waiting_players = [],
  playing_players = [],
  game = undefined,
  buyin = 1000,
  timer
}).


%% API.
new(Opts) ->
  {ok, Pid} = gen_fsm:start_link(?MODULE, [Opts], []),
  #table{pid = Pid}.

stop(#table{pid = Pid}) ->
  gen_fsm:stop(Pid).

call(Msg, #table{pid = Pid}) ->
  gen_fsm:sync_send_event(Pid, Msg).

cast(Msg, #table{pid = Pid}) ->
  gen_fsm:send_event(Pid, Msg).

this() ->
  #table{pid = self()}.

%% gen_fsm.

init([{Id, Lobby}]) ->
  {ok, Timer} = timer:send_interval(1000, self(), tick),
  {ok, waiting, #state{id = Id, lobby = Lobby, timer = Timer}}.

waiting(_Event, StateData) ->
  {next_state, waiting, StateData}.

waiting(Event, From, StateData) ->
  handle_sync_event(Event, From, waiting, StateData).

playing(_Event, StateData) ->
  {next_state, playing, StateData}.

playing(#g2t_finished{game = Game}, _From, StateData = #state{waiting_players = WaitingPlayers, playing_players = PlayingPlayers, game = Game}) ->
  {reply, ok, waiting, StateData#state{waiting_players = WaitingPlayers ++ PlayingPlayers, playing_players = [], game = undefined}};
playing(Event, From, StateData) ->
  handle_sync_event(Event, From, playing, StateData).
handle_event(Event, StateName, StateData) ->
  ok = io:format("event ~p~n", [Event]),
  {next_state, StateName, StateData}.

%% add player
handle_sync_event(#p2t_join{player = Player, id = PlayerId, name = PlayerName, chips = PlayerChips}, _From, StateName, StateData = #state{id = Id, lobby = Lobby, playing_players = PlayingPlayers, waiting_players = WaitingPlayers, buyin = BuyIn}) ->
  if PlayerChips < BuyIn ->
    {reply, not_enough_chips, StateName, StateData};
  true ->
    if length(PlayingPlayers) + length(WaitingPlayers) >= ?MAX_PLAYERS ->
      {reply, full, StateName, StateData};
    true ->
      ok = if length(PlayingPlayers) + length(WaitingPlayers) == ?MAX_PLAYERS - 1 ->
        ok = Lobby:call(#t2l_table_full{table_id = Id});
      true -> ok
      end,
      %% notify other players
      PlayerPb = #playerpb{id = PlayerId, name = PlayerName, chips = BuyIn},
      OtherJoinTableNtf = #otherjointablentf{player = PlayerPb},
      Message = #message{type = 'OTHER_JOIN_TABLE_NTF', data = OtherJoinTableNtf},
      [ok = P:notice(Message) || #player_in_table{player = P} <- lists:append(PlayingPlayers, WaitingPlayers)],
      PlayerInTable = #player_in_table{player = Player, pb = PlayerPb},
      WaitingPlayers1 = [PlayerInTable | WaitingPlayers],
      {reply, {ok, {Id, [PlayerPb1 ||#player_in_table{pb = PlayerPb1} <- lists:append(PlayingPlayers, WaitingPlayers1)], BuyIn}}, StateName, StateData#state{waiting_players = WaitingPlayers1}}
    end
  end;

%% del player
handle_sync_event(#p2t_leave{player = Player}, _From, StateName, StateData = #state{id = Id, lobby = Lobby, playing_players = PlayingPlayers, waiting_players = WaitingPlayers}) ->
  ok = if
    length(PlayingPlayers) + length(WaitingPlayers) == ?MAX_PLAYERS ->
      Lobby:call(#t2l_table_not_full{table_id = Id});
    true ->
      ok
  end,
  case lists:keytake(Player, #player_in_table.player, PlayingPlayers) of
    {value, #player_in_table{player = Player, pb = #playerpb{id = _Id, name = _Name, chips = Chips}}, PlayingPlayers1} ->
      {reply, {ok, Chips}, StateName, StateData#state{playing_players = PlayingPlayers1}};
    false ->
      case lists:keytake(Player, #player_in_table.player, WaitingPlayers) of
        {value, #player_in_table{player = Player, pb = #playerpb{id = _Id, name = _Name, chips = Chips}}, WaitingPlayers1} ->
          {reply, {ok, Chips}, StateName, StateData#state{waiting_players = WaitingPlayers1}};
        false ->
          {reply, not_in_table, StateName, StateData}
      end
  end;

%% dump
handle_sync_event(dump, _From, StateName, StateData) ->
  {reply, {StateName, StateData}, StateName, StateData};

handle_sync_event(_Event, _From, StateName, StateData) ->
  {reply, ignored, StateName, StateData}.

handle_info(tick, waiting, StateData = #state{waiting_players = WaitingPlayers}) ->
  if length(WaitingPlayers) >= ?MIN_PLAYERS ->
    ok = io:format("before start ~p~n", [WaitingPlayers]),
    Game = game:new({[Player || #player_in_table{player = Player} <- WaitingPlayers], this()}),
    ok = io:format("after start ~n"),
    {next_state, playing, StateData#state{waiting_players = [], playing_players = WaitingPlayers, game = Game}};
  true ->
    io:format("not enough players~n"),
    {next_state, waiting, StateData}
  end;

handle_info(Info, StateName, StateData) ->
  ok = io:format("info ~p~n", [Info]),
  {next_state, StateName, StateData}.

terminate(Reason, _StateName, #state{id = Id, lobby = Lobby}) ->
  ok = io:format("table ~p stoped for reason ~p~n", [this(), Reason]),
  ok = Lobby:cast(#t2l_table_stopped{table_id = Id}),
  ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.

%% tests
dump(#table{pid = Pid}) ->
  gen_fsm:sync_send_all_state_event(Pid, dump).

test() ->
  test_join_leave(),
  test_game_start().

%% test
test_join_leave() ->
  Lobby = lobby:new(),
  Table = table:new({0, Lobby}),
  U1 = #player_db{id = 1, name = 1},
  U2 = #player_db{id = 2, name = 2},
  A = player:new({U1, Lobby}),
  B = player:new({U2, Lobby}),
  {waiting, #state{waiting_players = [], playing_players = [], game = undefined}} = Table:dump(),
  {ok, {0, _}} = Table:call(#p2t_join{player = A}),
  {waiting, #state{waiting_players = [A], playing_players = [], game = undefined}} = Table:dump(),
  {ok, {0, _}} = Table:call(#p2t_join{player = B}),
  {waiting, #state{waiting_players = [B, A], playing_players = [], game = undefined}} = Table:dump(),
  ok = Table:call(#p2t_leave{player = B}),
  {waiting, #state{waiting_players = [A], playing_players = [], game = undefined}} = Table:dump(),
  ok = Table:call(#p2t_leave{player = A}),
  {waiting, #state{waiting_players = [], playing_players = [], game = undefined}} = Table:dump(),
  ok = B:stop(),
  ok = A:stop(),
  Table:stop(),
  Lobby:stop().

test_game_start() ->
  Lobby = lobby:new(),
  {ok, {TableId, Table}} = Lobby:call(#p2l_get_table{}),

  U1 = #player_db{id = 1, name = 1},
  U2 = #player_db{id = 2, name = 2},
  U3 = #player_db{id = 3, name = 3},

  A = player:new({U1, Lobby}),
  B = player:new({U2, Lobby}),
  C = player:new({U3, Lobby}),
  {ok, {TableId, _}} = A:call(#jointablereq{table_id = TableId}),
  {ok, {TableId, _}} = B:call(#jointablereq{table_id = TableId}),
  ok = Table:call(start),
  {playing, #state{waiting_players = [], playing_players = [B, A], game = Game = #game{}}} = Table:dump(),
  {ok, {TableId, _}} = C:call(#jointablereq{table_id = 0}),
  {preflop, _StateData} = Game:dump(),
  {playing, #state{waiting_players = [C], playing_players = [B, A]}} = Table:dump(),
  ok = Game:stop(),
  {waiting, #state{waiting_players = [C, B, A], playing_players = []}} = Table:dump(),
  ok = C:stop(),
  ok = B:stop(),
  ok = A:stop(),
  Table:stop(),
  Lobby:stop().
