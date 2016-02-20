-module(deck).
-behaviour(gen_fsm).
-include("holdem.hrl").

%% API.
-export([new/0, stop/1, get_card/1]).

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
  cards = lists:seq(0, 51)
}).

%% API.
new() ->
  {ok, Pid} = gen_fsm:start_link(?MODULE, [], []),
  #deck{pid = Pid}.

stop(#deck{pid = Pid}) ->
  gen_fsm:stop(Pid).

get_card(#deck{pid = Pid}) ->
  gen_fsm:sync_send_event(Pid, get).

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
