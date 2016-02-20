-module(table).
-behaviour(gen_fsm).
-include("holdem.hrl").

%% API.
-export([new/0, stop/1, add_player/2, del_player/2, start_game/1]).

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
  waiting_players = [],
  playing_players = [],
  game = undefined
}).

%% API.
-spec new() -> #table{}.
new() ->
  {ok, Pid} = gen_fsm:start_link(?MODULE, [], []),
  #table{pid = Pid}.

stop(#table{pid = Pid}) ->
  gen_fsm:stop(Pid).

add_player(Player, #table{pid = Pid}) ->
  gen_fsm:sync_send_all_state_event(Pid, {add, Player}).

del_player(Player, #table{pid = Pid}) ->
  gen_fsm:sync_send_all_state_event(Pid, {del, Player}).

start_game(#table{pid = Pid}) ->
  gen_fsm:sync_send_event(Pid, start_game).

%% gen_fsm.

init([]) ->
	{ok, waiting, #state{}}.

waiting(_Event, StateData) ->
	{next_state, waiting, StateData}.

waiting(start_game, _From, StateData = #state{waiting_players = WaitingPlayers}) ->
  if length(WaitingPlayers) >= ?MIN_PLAYERS ->
    {reply, ok, playing, StateData#state{waiting_players = [], playing_players = WaitingPlayers}};
  true ->
    {reply, ok, waiting, StateData}
  end;
waiting(_Event, _From, StateData) ->
	{reply, ignored, waiting, StateData}.
  
playing(_Event, StateData) ->
	{next_state, playing, StateData}.
playing(_Event, _From, StateData) ->
	{reply, ignored, playing, StateData}.

handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.

%% add player
handle_sync_event({add, Player}, _From, StateName, StateData = #state{playing_players = PlayingPlayers, waiting_players = WaitingPlayers}) when length(PlayingPlayers) + length(WaitingPlayers) < ?MAX_PLAYERS ->
  {reply, ok, StateName, StateData#state{waiting_players = [Player | WaitingPlayers]}};
handle_sync_event({add, _Player}, _From, StateName, StateData) ->
  {reply, full, StateName, StateData};

%% del player
handle_sync_event({del, Player}, _From, StateName, StateData = #state{playing_players = PlayingPlayers, waiting_players = WaitingPlayers}) ->
  {reply, ok, StateName, StateData#state{playing_players = lists:delete(Player, PlayingPlayers), waiting_players = lists:delete(Player, WaitingPlayers)}};

%% dump
handle_sync_event(dump, _From, StateName, StateData) ->
	{reply, {StateName, StateData}, StateName, StateData};

handle_sync_event(_Event, _From, StateName, StateData) ->
	{reply, ignored, StateName, StateData}.

handle_info(_Info, StateName, StateData) ->
	{next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
	ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.


%% tests
dump(#table{pid = Pid}) ->
  gen_fsm:sync_send_all_state_event(Pid, dump).

test() ->
  test_add_players().

%% test
test_add_players() ->
  Table = table:new(),
  {waiting, #state{waiting_players = [], playing_players = [], game = undefined}} = Table:dump(),
  ok = Table:add_player(a),
  {waiting, #state{waiting_players = [a], playing_players = [], game = undefined}} = Table:dump(),
  ok = Table:add_player(b),
  {waiting, #state{waiting_players = [b, a], playing_players = [], game = undefined}} = Table:dump(),
  ok = Table:start_game(),
  {playing, #state{waiting_players = [], playing_players = [b, a]}} = Table:dump(),
  ok = Table:add_player(c),
  {playing, #state{waiting_players = [c], playing_players = [b, a]}} = Table:dump(),
  ok = Table:add_player(d),
  {playing, #state{waiting_players = [d, c], playing_players = [b, a]}} = Table:dump(),
  ok = Table:add_player(e),
  {playing, #state{waiting_players = [e, d, c], playing_players = [b, a]}} = Table:dump(),
  ok = Table:add_player(f),
  {playing, #state{waiting_players = [f, e, d, c], playing_players = [b, a]}} = Table:dump(),
  ok = Table:add_player(g),
  {playing, #state{waiting_players = [g, f, e, d, c], playing_players = [b, a]}} = Table:dump(),
  ok = Table:add_player(h),
  {playing, #state{waiting_players = [h, g, f, e, d, c], playing_players = [b, a]}} = Table:dump(),
  ok = Table:add_player(i),
  {playing, #state{waiting_players = [i, h, g, f, e, d, c], playing_players = [b, a]}} = Table:dump(),
  ok = Table:add_player(j),
  {playing, #state{waiting_players = [j, i, h, g, f, e, d, c], playing_players = [b, a]}} = Table:dump(),
  full = Table:add_player(k),
  {playing, #state{waiting_players = [j, i, h, g, f, e, d, c], playing_players = [b, a]}} = Table:dump(),
  Table:stop().
