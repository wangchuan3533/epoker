-module(player).
-behaviour(gen_fsm).
-include("holdem.hrl").

%% API.
-export([new/0, stop/1, join/2, leave/1]).
-export([dump/1]).

%% gen_fsm.
-export([init/1]).
-export([lobby/2, playing/2]).
-export([lobby/3, playing/3]).
-export([handle_event/3]).
-export([handle_sync_event/4]).
-export([handle_info/3]).
-export([terminate/3]).
-export([code_change/4]).

-record(state, {
  game = undefined
}).

%% API.
new() ->
	{ok, Pid} = gen_fsm:start_link(?MODULE, [], []),
  #player{pid = Pid}.

stop(#player{pid = Pid}) ->
  gen_fsm:stop(Pid).

join(Game, #player{pid = Pid}) ->
  gen_fsm:sync_send_event(Pid, {join, Game}).
  
leave(#player{pid = Pid}) ->
  gen_fsm:sync_send_event(Pid, leave).
  
dump(#player{pid = Pid}) ->
  gen_fsm:sync_send_all_state_event(Pid, dump).

%% gen_fsm.
init([]) ->
	{ok, lobby, #state{}}.

lobby(_Event, StateData) ->
	{next_state, lobby, StateData}.

lobby({join, Game = #game{}}, _From, StateData) ->
  NewStateData = StateData#state{game = Game},
  ok = Game:add_player(#player{pid = self()}),
	{reply, ok, playing, NewStateData};
  
lobby(_Event, _From, StateData) ->
	{reply, ignored, lobby, StateData}.
  
playing(_Event, StateData) ->
	{next_state, playing, StateData}.

playing(leave, _From, StateData = #state{game = Game}) ->
  ok = Game:del_player(#player{pid = self()}),
  NewStateData = StateData#state{game = undefined},
	{reply, ok, lobby, NewStateData};
playing(_Event, _From, StateData) ->
	{reply, ignored, playing, StateData}.

handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.

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
