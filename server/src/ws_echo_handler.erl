-module(ws_echo_handler).
-behaviour(cowboy_websocket_handler).
-include("holdem.hrl").

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
	{ok, Req2, #state{player = player:new()}}.

websocket_handle({text, Text}, Req, State = #state{player = Player}) ->
  ok = io:format("received text message ~s~n", [Text]),
  Msg = jiffy:decode(Text, [return_maps]),
  Resp = iolist_to_binary(jiffy:encode(handle_message(Player, Msg))),
	{reply, {text, Resp}, Req, State};
websocket_handle({binary, Data}, Req, State) ->
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
      Player:call(#c2s_join{table_id = maps:get(<<"tableId">>, Msg, -1)});
    #{<<"type">> := <<"leave">>} ->
      Player:call(#c2s_leave{});
    #{<<"type">> := <<"list">>} ->
      Player:call(#c2s_list{})
  end.
