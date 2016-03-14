-module(ws_echo_handler).
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
    Msg -> iolist_to_binary(jiffy:encode(handle_message(Player, Msg)))
  catch
    throw:{error,_} -> "not json"
  end,
	{reply, {text, Resp}, Req, State};
websocket_handle({binary, Data}, Req, State) ->
  Message = messages_pb:decode_message(Data),
  ok = case Message of
    #message{type = 'JOIN_TABLE_REQ', data = PbData} ->
      JoinTableReq = messages_pb:decode_jointablereq(PbData),
      ok = io:format("~p~n", [JoinTableReq]);
    _ -> ok
  end,
	{reply, {binary, Data}, Req, State};
websocket_handle(_Frame, Req, State) ->
	{ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
  {reply, {text, Msg}, Req, State};

websocket_info(_Info, Req, State) ->
	{ok, Req, State}.

websocket_terminate(_Reason, _Req, #state{player = Player}) ->
  Player:stop(),
	ok.

handle_message(Player, Msg) ->
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
