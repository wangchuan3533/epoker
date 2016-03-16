-module(ws_handler).
-behaviour(cowboy_websocket_handler).
-include("holdem.hrl").
-include("messages_pb.hrl").

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-record(state, {
  player
}).

init(_, _, _) ->
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_, Req, _Opts) ->
	Req2 = cowboy_req:compact(Req),
	{ok, Req2, #state{player = player:new(lobby)}}.

websocket_handle({text, Text}, Req, State = #state{player = Player}) ->
  ok = io:format("received text message ~s~n", [Text]),
  Resp = try jiffy:decode(Text, [return_maps]) of
    Msg -> iolist_to_binary(jiffy:encode(handle_json_message(Player, Msg)))
  catch
    throw:{error,_} -> "not json"
  end,
	{reply, {text, Resp}, Req, State};
websocket_handle({binary, Data}, Req, State = #state{player = Player}) ->
  ReqMsg = messages_pb:decode_message(Data),
  ResMsg = handle_pb_message(Player, ReqMsg),
	{reply, {binary, messages_pb:encode(ResMsg)}, Req, State};
websocket_handle(_Frame, Req, State) ->
	{ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
  {reply, {text, Msg}, Req, State};

websocket_info(_Info, Req, State) ->
	{ok, Req, State}.

websocket_terminate(_Reason, _Req, #state{player = Player}) ->
  Player:stop(),
	ok.

handle_pb_message(Player, Msg) ->
  case Msg of
    #message{type = 'JOIN_TABLE_REQ', data = Data} ->
      #jointablereq{table_id = TableId} = messages_pb:decode_jointablereq(Data),
      {ok, {TableId1, Players}} = Player:call(#c2s_join_table{table_id = TableId}),
      PlayersPb = [#playerpb{id = 0, name = "hello", chips = 100, head_img = "img"} || _ <- Players],
      TablePb = #tablepb{id = TableId1, players = PlayersPb},
      Res = #jointableres{errno = 0, table = TablePb},
      #message{type = 'JOIN_TABLE_RES', data = messages_pb:encode(Res)};
  
    #message{type = 'LEAVE_TABLE_REQ', data = Data} ->
      #leavetablereq{} = messages_pb:decode_leavetablereq(Data),
      ok = Player:call(#c2s_leave_table{}),
      Res = #leavetableres{errno = 0},
      #message{type = 'LEAVE_TABLE_RES', data = messages_pb:encode(Res)};

    #message{type = 'LIST_TABLE_REQ', data = Data} ->
      #listtablereq{} = messages_pb:decode_listtablereq(Data),
      TableIds = Player:call(#c2s_list_table{}),
      Res = #listtableres{errno = 0, table_ids = TableIds},
      #message{type = 'LIST_TABLE_RES', data = messages_pb:encode(Res)};

    #message{type = 'LEAVE_GAME_REQ', data = Data} ->
      #leavegamereq{} = messages_pb:decode_leavegamereq(Data),
      %%ok = Player:call(#c2s_leave_game{}),
      ignored = Player:call(#c2s_leave_game{}),
      Res = #leavegameres{errno = 0},
      #message{type = 'LEAVE_GAME_RES', data = messages_pb:encode(Res)};

    #message{type = 'ACTION_REQ', data = Data} ->
      #actionreq{action = Action, amount = Amount} = messages_pb:decode_actionreq(Data),
      %ok = Player:call(#c2s_action{action = Action, amount = Amount}),
      ignored = Player:call(#c2s_action{action = Action, amount = Amount}),
      Res = #actionres{errno = 0},
      #message{type = 'ACTION_RES', data = messages_pb:encode(Res)};
    _Other ->
      <<"not recognized protocol">>
  end.

handle_json_message(Player, Msg) ->
  case Msg of
    #{<<"type">> := <<"join">>} ->
      Player:call(#c2s_join_table{table_id = maps:get(<<"tableId">>, Msg, -1)});
    #{<<"type">> := <<"quit">>} ->
      Player:call(#c2s_leave_game{});
    #{<<"type">> := <<"leave">>} ->
      Player:call(#c2s_leave_table{});
    #{<<"type">> := <<"list">>} ->
      Player:call(#c2s_list_table{});
    #{<<"type">> := <<"action">>} ->
      Player:call(#c2s_action{action = maps:get(<<"action">>, Msg, 1), amount = maps:get(<<"amount">>, Msg, 0)});
    _Other ->
      <<"not recognized protocol">>
  end.
