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

websocket_init(_Type, Req, _Opts) ->
  {Token, Req2} = cowboy_req:binding(token, Req),
  case storage:get(Token) of
    {ok, PlayerDb} ->
      Req3 = cowboy_req:compact(Req2),
      {ok, Req3, #state{player = player:new({PlayerDb, lobby, self()})}};
    {error, Reason} ->
      ok = io:format("auth failed reason ~p~n", [Reason]),
      {shutdown, Req2}
  end.

websocket_handle({text, Text}, Req, State = #state{player = Player}) ->
  ok = io:format("received text message ~s~n", [Text]),
  Resp = try jiffy:decode(Text, [return_maps]) of
    Msg -> iolist_to_binary(jiffy:encode(handle_json_message(Player, Msg)))
  catch
    throw:{error,_} -> "not json"
  end,
  {reply, {text, Resp}, Req, State};
websocket_handle({binary, Data}, Req, State = #state{player = Player}) ->
  Msg = protocol:decode(Data),
  {ok, Res} = Player:call(Msg),
  ok = io:format("~p~n", [{debug, Res}]),
  {reply, {binary, protocol:encode(Res)}, Req, State};
websocket_handle(_Frame, Req, State) ->
  {ok, Req, State}.

websocket_info({notice, Message}, Req, State) ->
  {reply, {binary, messages_pb:encode(Message)}, Req, State};
websocket_info(_Info, Req, State) ->
  {ok, Req, State}.

websocket_terminate(_Reason, _Req, #state{player = Player}) ->
  ok = Player:stop().
%websocket_terminate(_Reason, _Req, _State) ->
%  ok.

handle_json_message(Player, Msg) ->
  case Msg of
    #{<<"type">> := <<"join">>} ->
      Player:call(#jointablereq{table_id = maps:get(<<"tableId">>, Msg, -1)});
    #{<<"type">> := <<"quit">>} ->
      Player:call(#leavegamereq{});
    #{<<"type">> := <<"leave">>} ->
      Player:call(#leavetablereq{});
    #{<<"type">> := <<"list">>} ->
      Player:call(#listtablereq{});
    #{<<"type">> := <<"action">>} ->
      Player:call(#actionreq{action = maps:get(<<"action">>, Msg, 1), amount = maps:get(<<"amount">>, Msg, 0)});
    _Other ->
      <<"not recognized protocol">>
  end.
