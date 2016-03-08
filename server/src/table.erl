-module(table).
-behaviour(gen_fsm).
-include("holdem.hrl").

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
  game = undefined
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
	{ok, waiting, #state{id = Id, lobby = Lobby}}.

waiting(_Event, StateData) ->
	{next_state, waiting, StateData}.

waiting(start, _From, StateData = #state{waiting_players = WaitingPlayers}) ->
  if length(WaitingPlayers) >= ?MIN_PLAYERS ->
    Game = game:new({WaitingPlayers, this()}),
    {reply, ok, playing, StateData#state{waiting_players = [], playing_players = WaitingPlayers, game = Game}};
  true ->
    {reply, ok, waiting, StateData}
  end;
waiting(Event, From, StateData) ->
  handle_sync_event(Event, From, waiting, StateData).

playing(_Event, StateData) ->
	{next_state, playing, StateData}.

playing(#g2t_finished{game = Game}, _From, StateData = #state{waiting_players = WaitingPlayers, playing_players = PlayingPlayers, game = Game}) ->
	{reply, ok, waiting, StateData#state{waiting_players = WaitingPlayers ++ PlayingPlayers, playing_players = [], game = undefined}};
playing(Event, From, StateData) ->
  handle_sync_event(Event, From, playing, StateData).

handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.

%% add player
handle_sync_event(#p2t_join{player = Player}, _From, StateName, StateData = #state{id = Id, lobby = Lobby, playing_players = PlayingPlayers, waiting_players = WaitingPlayers}) ->
  if
    length(PlayingPlayers) + length(WaitingPlayers) >= ?MAX_PLAYERS ->
      {reply, full, StateName, StateData};
    length(PlayingPlayers) + length(WaitingPlayers) == ?MAX_PLAYERS - 1->
      ok = Lobby:call(#t2l_table_full{table_id = Id}),
      {reply, ok, StateName, StateData#state{waiting_players = [Player | WaitingPlayers]}};
    true ->
      {reply, ok, StateName, StateData#state{waiting_players = [Player | WaitingPlayers]}}
  end;

%% del player
handle_sync_event(#p2t_leave{player = Player}, _From, StateName, StateData = #state{id = Id, lobby = Lobby, playing_players = PlayingPlayers, waiting_players = WaitingPlayers}) ->
  ok = if
    length(PlayingPlayers) + length(WaitingPlayers) == ?MAX_PLAYERS ->
      Lobby:call(#t2l_table_not_full{table_id = Id});
    true ->
      ok
  end,
  {reply, ok, StateName, StateData#state{playing_players = lists:delete(Player, PlayingPlayers), waiting_players = lists:delete(Player, WaitingPlayers)}};

%% dump
handle_sync_event(dump, _From, StateName, StateData) ->
	{reply, {StateName, StateData}, StateName, StateData};

handle_sync_event(_Event, _From, StateName, StateData) ->
	{reply, ignored, StateName, StateData}.

handle_info(_Info, StateName, StateData) ->
	{next_state, StateName, StateData}.

terminate(_Reason, _StateName, #state{id = Id, lobby = Lobby}) ->
  ok = Lobby:cast(#t2l_table_stopped{table_id = Id}),
  ok = io:format("table ~w stoped.~n", [this()]),
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
  A = player:new(Lobby),
  B = player:new(Lobby),
  {waiting, #state{waiting_players = [], playing_players = [], game = undefined}} = Table:dump(),
  ok = Table:call(#p2t_join{player = A}),
  {waiting, #state{waiting_players = [A], playing_players = [], game = undefined}} = Table:dump(),
  ok = Table:call(#p2t_join{player = B}),
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
  A = player:new(Lobby),
  B = player:new(Lobby),
  C = player:new(Lobby),
  TableId = A:call(#c2s_join_table{table_id = TableId}),
  TableId = B:call(#c2s_join_table{table_id = TableId}),
  ok = Table:call(start),
  {playing, #state{waiting_players = [], playing_players = [B, A], game = Game = #game{}}} = Table:dump(),
  TableId = C:call(#c2s_join_table{table_id = 0}),
  {preflop, _StateData} = Game:dump(),
  {playing, #state{waiting_players = [C], playing_players = [B, A]}} = Table:dump(),
  ok = Game:call(next),
  {flop, _StateData} = Game:dump(),
  {playing, #state{waiting_players = [C], playing_players = [B, A]}} = Table:dump(),
  ok = Game:call(next),
  {turn, _StateData} = Game:dump(),
  {playing, #state{waiting_players = [C], playing_players = [B, A]}} = Table:dump(),
  ok = Game:call(next),
  {river, _StateData} = Game:dump(),
  {playing, #state{waiting_players = [C], playing_players = [B, A]}} = Table:dump(),
  ok = Game:call(next),
  {finished, _StateData} = Game:dump(),
  ok = Game:stop(),
  {waiting, #state{waiting_players = [C, B, A], playing_players = []}} = Table:dump(),
  ok = C:stop(),
  ok = B:stop(),
  ok = A:stop(),
  Table:stop(),
  Lobby:stop().
