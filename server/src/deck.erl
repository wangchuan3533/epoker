-module(deck).
-behaviour(gen_fsm).

%% API.
-export([start_link/0]).

%% gen_fsm.
-export([init/1]).
-export([available/2]).
-export([available/3]).
-export([finished/2]).
-export([finished/3]).
-export([handle_event/3]).
-export([handle_sync_event/4]).
-export([handle_info/3]).
-export([terminate/3]).
-export([code_change/4]).

-record(state, {
  cards = lists:seq(0, 5)
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_fsm.

init([]) ->
	{ok, available, #state{}}.

available(_Event, StateData) ->
	{next_state, available, StateData}.

available(get, _From, StateData = #state{cards = Cards}) ->
  Index = random:uniform(length(Cards)),
  Card = lists:nth(Index, Cards),
  NewCards = lists:delete(Card, Cards),
  NewStateData = StateData#state{cards = NewCards},
  case NewCards of
    [] ->
	    {reply, finished, finished, NewStateData};
    _ ->
	    {reply, Card, available, NewStateData}
  end.
  
finished(_Event, StateData) ->
	{next_state, finished, StateData}.

finished(_Event, _From, StateData = #state{cards = []}) ->
	{reply, finished, finished, StateData}.
  
handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
	{reply, ignored, StateName, StateData}.

handle_info(_Info, StateName, StateData) ->
	{next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
	ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.
