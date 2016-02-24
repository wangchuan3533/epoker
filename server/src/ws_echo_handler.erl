-module(ws_echo_handler).
-behaviour(cowboy_websocket_handler).

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
  ok = io:format("received text message ~w~n", [Text]),
  {struct, L} = mochijson2:decode(Text),
  Resp = iolist_to_binary(mochijson2:encode(Player:call(maps:from_list(L)))),
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
