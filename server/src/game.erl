-module(game).
-behaviour(gen_fsm).
-include("holdem.hrl").

%% API.
-export([new/0, stop/1, add_player/2, del_player/2]).

%% test
-export([dump/1, test/0, test2/0]).

%% gen_fsm.
-export([init/1]).
-export([waiting/2, preflop/2]).
-export([waiting/3, preflop/3]).
-export([handle_event/3]).
-export([handle_sync_event/4]).
-export([handle_info/3]).
-export([terminate/3]).
-export([code_change/4]).

-record(state, {
  waiting_players = [],
  playing_players = []
}).

%% test
test() ->
  Game = game:new(),
  {waiting, #state{waiting_players = [], playing_players = []}} = Game:dump(),
  ok = Game:add_player(a),
  {waiting, #state{waiting_players = [a], playing_players = []}} = Game:dump(),
  ok = Game:add_player(b),
  {preflop, #state{waiting_players = [], playing_players = [b, a]}} = Game:dump(),
  ok = Game:add_player(c),
  {preflop, #state{waiting_players = [c], playing_players = [b, a]}} = Game:dump(),
  ok = Game:add_player(d),
  {preflop, #state{waiting_players = [d, c], playing_players = [b, a]}} = Game:dump(),
  ok = Game:add_player(e),
  {preflop, #state{waiting_players = [e, d, c], playing_players = [b, a]}} = Game:dump(),
  ok = Game:add_player(f),
  {preflop, #state{waiting_players = [f, e, d, c], playing_players = [b, a]}} = Game:dump(),
  ok = Game:add_player(g),
  {preflop, #state{waiting_players = [g, f, e, d, c], playing_players = [b, a]}} = Game:dump(),
  ok = Game:add_player(h),
  {preflop, #state{waiting_players = [h, g, f, e, d, c], playing_players = [b, a]}} = Game:dump(),
  ok = Game:add_player(i),
  {preflop, #state{waiting_players = [i, h, g, f, e, d, c], playing_players = [b, a]}} = Game:dump(),
  ok = Game:add_player(j),
  {preflop, #state{waiting_players = [j, i, h, g, f, e, d, c], playing_players = [b, a]}} = Game:dump(),
  full = Game:add_player(k),
  {preflop, #state{waiting_players = [j, i, h, g, f, e, d, c], playing_players = [b, a]}} = Game:dump(),
  Game:stop().

test2() ->
  P1 = player:new(),
  P2 = player:new(),
  P3 = player:new(),
  G = game:new(),
  P1:join(G),
  {waiting, #state{waiting_players = [P1], playing_players = []}} = G:dump(),
  {playing, {state, G}} = P1:dump(),
  P2:join(G),
  {preflop, #state{waiting_players = [], playing_players = [P2, P1]}} = G:dump(),
  {playing, {state, G}} = P2:dump(),
  P3:join(G),
  {preflop, #state{waiting_players = [P3], playing_players = [P2, P1]}} = G:dump(),
  {playing, {state, G}} = P3:dump(),
  ok = P3:leave(),
  ok = P2:leave(),
  ok = P1:leave(),
  G:stop(),
  P3:stop(),
  P2:stop(),
  P1:stop().

%% API.
-spec new() -> #game{}.
new() ->
  {ok, Pid} = gen_fsm:start_link(?MODULE, [], []),
  #game{pid = Pid}.

stop(#game{pid = Pid}) ->
  gen_fsm:stop(Pid).

add_player(Player, #game{pid = Pid}) ->
  gen_fsm:sync_send_all_state_event(Pid, {add, Player}).

del_player(Player, #game{pid = Pid}) ->
  gen_fsm:sync_send_all_state_event(Pid, {del, Player}).

dump(#game{pid = Pid}) ->
  gen_fsm:sync_send_all_state_event(Pid, dump).

%% gen_fsm.

init([]) ->
	{ok, waiting, #state{}}.

waiting(_Event, StateData) ->
	{next_state, waiting, StateData}.

waiting(_Event, _From, StateData) ->
	{reply, ignored, waiting, StateData}.

preflop(_Event, StateData) ->
	{next_state, preflop, StateData}.

preflop(_Event, _From, StateData) ->
	{reply, ignored, preflop, StateData}.

handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.

%% add player
handle_sync_event({add, Player}, _From, StateName, StateData = #state{waiting_players = WaitingPlayers}) when StateName == waiting, length(WaitingPlayers) + 1 == ?MIN_PLAYERS ->
  %% preflop
  {reply, ok, preflop, StateData#state{waiting_players = [], playing_players = [Player | WaitingPlayers]}};
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
