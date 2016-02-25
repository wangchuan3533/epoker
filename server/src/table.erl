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
  waiting_players = [],
  playing_players = [],
  game = undefined
}).

%% API.
new(Id) ->
  {ok, Pid} = gen_fsm:start_link(?MODULE, [Id], []),
  #table{pid = Pid}.

stop(#table{pid = Pid}) ->
  gen_fsm:stop(Pid).

call(Msg, #table{pid = Pid}) ->
  gen_fsm:sync_send_event(Pid, Msg).

cast(Msg, #table{pid = Pid}) ->
  gen_fsm:send_event(Pid, Msg).


%% gen_fsm.

init([Id]) when is_integer(Id) ->
	{ok, waiting, #state{id = Id}}.

waiting(_Event, StateData) ->
	{next_state, waiting, StateData}.

waiting(#p2t_start{}, _From, StateData = #state{waiting_players = WaitingPlayers}) ->
  if length(WaitingPlayers) >= ?MIN_PLAYERS ->
    Game = game:new({WaitingPlayers, #table{pid = self()}}),
    lists:foreach(fun(Player) -> Player:call({game_started, Game}) end, WaitingPlayers),
    {reply, ok, playing, StateData#state{waiting_players = [], playing_players = WaitingPlayers, game = Game}};
  true ->
    {reply, ok, waiting, StateData}
  end;
waiting(Event, From, StateData) ->
  handle_sync_event(Event, From, waiting, StateData).

playing(#g2t_finished{}, StateData = #state{waiting_players = WaitingPlayers, playing_players = PlayingPlayers, game = Game}) ->
  ok = Game:stop(),
	{next_state, waiting, StateData#state{waiting_players = WaitingPlayers ++ PlayingPlayers, playing_players = [], game = undefined}};
playing(_Event, StateData) ->
	{next_state, playing, StateData}.

playing(Event, From, StateData) ->
  handle_sync_event(Event, From, playing, StateData).

handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.

%% add player
handle_sync_event(#p2t_join{player = Player}, _From, StateName, StateData = #state{id = Id, playing_players = PlayingPlayers, waiting_players = WaitingPlayers}) ->
  if
    length(PlayingPlayers) + length(WaitingPlayers) >= ?MAX_PLAYERS ->
      {reply, full, StateName, StateData};
    length(PlayingPlayers) + length(WaitingPlayers) == ?MAX_PLAYERS - 1->
      ok = lobby:call(#t2l_table_full{table_id = Id}),
      {reply, ok, StateName, StateData#state{waiting_players = [Player | WaitingPlayers]}};
    true ->
      {reply, ok, StateName, StateData#state{waiting_players = [Player | WaitingPlayers]}}
  end;

%% del player
handle_sync_event(#p2t_leave{player = Player}, _From, StateName, StateData = #state{id = Id, playing_players = PlayingPlayers, waiting_players = WaitingPlayers}) ->
  ok = if
    length(PlayingPlayers) + length(WaitingPlayers) == ?MAX_PLAYERS ->
      lobby:call(#t2l_table_not_full{table_id = Id});
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

terminate(_Reason, _StateName, #state{id = Id}) ->
  ok = lobby:cast(#t2l_table_stopped{table_id = Id}),
  ok = io:format("table ~w stoped.~n", [self()]),
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
  Table = table:new(0),
  A = player:new(),
  B = player:new(),
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
  Table:stop().

test_game_start() ->
  Table = table:new(0),
  A = player:new(),
  B = player:new(),
  C = player:new(),
  ok = Table:call(#p2t_join{player = A}),
  ok = Table:call(#p2t_join{player = B}),
  ok = Table:call(#p2t_start{}),
  {playing, #state{waiting_players = [], playing_players = [B, A], game = Game = #game{}}} = Table:dump(),
  ok = Table:call(#p2t_join{player = C}),
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
  {waiting, #state{waiting_players = [C, B, A], playing_players = []}} = Table:dump(),
  ok = C:stop(),
  ok = B:stop(),
  ok = A:stop(),
  Table:stop().
