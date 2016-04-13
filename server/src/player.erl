-module(player).
-behaviour(gen_fsm).
-include("holdem.hrl").
-include("messages_pb.hrl").

%% API.
-export([new/1, stop/1, call/2, cast/2, notice/2]).
-export([dump/1, test/0]).

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
  game,
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

in_table(#leavetablereq{}, _From, StateData = #state{table = Table}) ->
  ok = Table:call(#p2t_leave{player = this()}),
  NewStateData = StateData#state{table = undefined},
  {reply, ok, in_lobby, NewStateData};
in_table(#g2p_started{game = Game}, _From, StateData) ->
  NewStateData = StateData#state{game = Game},
  {reply, ok, in_game, NewStateData};
in_table(Event, From, StateData) ->
  handle_sync_event(Event, From, in_table, StateData).

in_game(Event, StateData) ->
  handle_event(Event, in_game, StateData).

in_game(#actionreq{action = Action, amount = Amount}, _From, StateData = #state{game = Game}) ->
  Ret = Game:call(#p2g_action{player = this(), action = Action, amount = Amount}),
  {reply, Ret, in_game, StateData};
in_game(#leavetablereq{}, _From, StateData = #state{game = Game}) ->
  Game:call(#p2g_action{player = this(), action = ?ACTION_FOLD}),
  NewStateData = StateData#state{game = undefined},
  {reply, ok, in_table, NewStateData};
in_game(#g2p_finished{game = Game}, _From, StateData = #state{game = Game}) ->
  NewStateData = StateData#state{game = undefined},
  {reply, ok, in_table, NewStateData};
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

handle_sync_event(#listtablereq{}, _From, StateName, StateData = #state{lobby = Lobby}) ->
  TableIds = lists:map(fun({K, _V}) -> K end, Lobby:call(#p2l_list_tables{})),
  {reply, TableIds, StateName, StateData};

handle_sync_event(_Event, _From, StateName, StateData) ->
  {reply, ignored, StateName, StateData}.

handle_info(_Info, StateName, StateData) ->
  {next_state, StateName, StateData}.

terminate(Reason, StateName, #state{table = Table, game = Game, player_db = PlayerDb, chips = Chips}) ->
  io:format("player ~p stoped for reason ~p~n", [this(), Reason]),
  LeftChips = case StateName of
    in_game ->
      ok = Game:call(#p2g_action{player = this(), action = ?ACTION_FOLD}),
      {ok, Chips1} = Table:call(#p2t_leave{player = this()}),
      Chips1;
    in_table ->
      {ok, Chips1} = Table:call(#p2t_leave{player = this()}),
      Chips1;
    in_lobby ->
      0
  end,

  %% save back to db
  ok = storage:set(PlayerDb#player_db{chips = Chips + LeftChips}).

code_change(_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.

%% test

dump(#player{pid = Pid}) ->
  gen_fsm:sync_send_all_state_event(Pid, dump).

test() ->
  test_join().

test_join() ->
  L = lobby:new(),
  U = #player_db{id = 1, name = 1},
  P = player:new({U, L}),
  {in_lobby, #state{table = undefined}} = P:dump(),
  P:call(#jointablereq{table_id = -1}),
  {in_table, #state{table = T}} = P:dump(),
  ok = P:call(#leavetablereq{}),
  T:stop(),
  P:stop().
