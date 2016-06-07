-module(player).
-behaviour(gen_fsm).
-include("holdem.hrl").
-include("messages_pb.hrl").

%% API.
-export([new/1, stop/1, call/2, cast/2, notice/2]).
-export([test/0]).

%% gen_fsm.
-export([init/1]).
-export([in_lobby/2, in_table/2, in_game/2]).
-export([in_lobby/3, in_table/3, in_game/3]).
-export([handle_event/3]).
-export([handle_sync_event/4]).
-export([handle_info/3]).
-export([terminate/3]).
-export([code_change/4]).

-record(state, {
  ws,
  lobby,
  table,
  player_db,
  id,
  name,
  chips
}).

%% API.
new(Opts) ->
  {ok, Pid} = gen_fsm:start_link(?MODULE, [Opts], []),
  #player{pid = Pid}.

stop(#player{pid = Pid}) ->
  gen_fsm:stop(Pid).

call(Msg, #player{pid = Pid}) ->
  gen_fsm:sync_send_event(Pid, Msg).
cast(Msg, #player{pid = Pid}) ->
  gen_fsm:send_event(Pid, Msg).
notice(Msg, #player{pid = Pid}) ->
  gen_fsm:send_event(Pid, {notice, Msg}).
this() ->
  #player{pid = self()}.

%% inner funcs
leave_table(StateName, StateData = #state{table = Table, chips = Chips, id = Id}) ->
  case StateName of
    in_game ->
      ok = Table:cast(#p2t_action{player = this(), action = ?ACTION_FOLD}),
      {ok, LeftChips} = Table:call(#p2t_leave{player = this(), id = Id}),
      {reply, ok, in_lobby, StateData#state{table = undefined, chips = Chips + LeftChips}};
    in_table ->
      {ok, LeftChips} = Table:call(#p2t_leave{player = this(), id = Id}),
      {reply, ok, in_lobby, StateData#state{table = undefined, chips = Chips + LeftChips}};
    in_lobby ->
      {reply, ok, in_lobby, StateData}
  end.

%% gen_fsm.
init([{PlayerDb = #player_db{id = Id, name = Name, chips = Chips}, Lobby, Ws}]) ->
  {ok, in_lobby, #state{lobby = Lobby, player_db = PlayerDb, id = Id, name = Name, chips = Chips, ws = Ws}}.

in_lobby(Event, StateData) ->
  handle_event(Event, in_lobby, StateData).

in_lobby(#jointablereq{table_id = Tid}, _From, StateData = #state{lobby = Lobby, id = Id, name = Name, chips = Chips}) ->
  {ok, {_TableId, Table}} = Lobby:call(#p2l_get_table{table_id = Tid}),
  case Table:call(#p2t_join{player = this(), id = Id, name = Name, chips = Chips}) of
    {ok, {TableId, OtherPlayers, BuyIn}} ->
      NewStateData = StateData#state{table = Table, chips = Chips - BuyIn},
      Ret = #jointableres{errno = 0, table = #tablepb{id = TableId, players = OtherPlayers}},
      {reply, {ok, Ret}, in_table, NewStateData};
    Other ->
      {reply, Other, StateData}
  end;

in_lobby(Event, From, StateData) ->
  handle_sync_event(Event, From, in_lobby, StateData).

in_table(Event, StateData) ->
  handle_event(Event, in_table, StateData).

in_table(#t2p_started{}, _From, StateData) ->
  ok = io:format("t2p_started~n"),
  {reply, ok, in_game, StateData};
in_table(Event, From, StateData) ->
  handle_sync_event(Event, From, in_table, StateData).

in_game(Event, StateData) ->
  handle_event(Event, in_game, StateData).

in_game(#actionreq{action = Action, amount = Amount}, _From, StateData = #state{table = Table, id = Id}) ->
  Ret = Table:call(#p2t_action{player = this(), id= Id, action = Action, amount = Amount}),
  {reply, Ret, in_game, StateData};
in_game(#t2p_finished{}, _From, StateData) ->
  {reply, ok, in_table, StateData};
in_game(Event, From, StateData) ->
  handle_sync_event(Event, From, in_game, StateData).

handle_event({notice, Msg}, StateName, StateData = #state{ws = Ws}) ->
  Ws ! {notice, Msg},
  {next_state, StateName, StateData};
handle_event(_Event, StateName, StateData) ->
  {next_state, StateName, StateData}.

%% dump
handle_sync_event(dump, _From, StateName, StateData) ->
  {reply, {StateName, StateData}, StateName, StateData};

handle_sync_event(#leavetablereq{}, _From, StateName, StateData) ->
  leave_table(StateName, StateData);

handle_sync_event(#listtablereq{}, _From, StateName, StateData = #state{lobby = Lobby}) ->
  TableIds = lists:map(fun({K, _V}) -> K end, Lobby:call(#p2l_list_tables{})),
  {reply, TableIds, StateName, StateData};

handle_sync_event(_Event, _From, StateName, StateData) ->
  {reply, ignored, StateName, StateData}.

handle_info(_Info, StateName, StateData) ->
  {next_state, StateName, StateData}.

terminate(Reason, StateName, StateData = #state{player_db = PlayerDb}) ->
  io:format("player ~p stoped for reason ~p~n", [this(), Reason]),
  {reply, ok, in_lobby, #state{chips = Chips}} = leave_table(StateName, StateData),
  %% save back to db
  ok = storage:set(PlayerDb#player_db{chips = Chips}).

code_change(_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.

%% test
test() ->
  ok.
