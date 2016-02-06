-module(seat).
-behaviour(gen_fsm).

%% API.
-export([start_link/0]).

%% gen_fsm.
-export([init/1]).
-export([empty/2]).
-export([empty/3]).
-export([handle_event/3]).
-export([handle_sync_event/4]).
-export([handle_info/3]).
-export([terminate/3]).
-export([code_change/4]).

-record(state, {
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_fsm:start_link(?MODULE, [], []).

%% gen_fsm.

init([]) ->
	{ok, empty, #state{}}.

empty(_Event, StateData) ->
	{next_state, empty, StateData}.

handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.

empty(_Event, _From, StateData) ->
	{reply, ignored, empty, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
	{reply, ignored, StateName, StateData}.

handle_info(_Info, StateName, StateData) ->
	{next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
	ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.
