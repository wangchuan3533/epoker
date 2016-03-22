-module(index_handler).
-behaviour(cowboy_http_handler).
-include("holdem.hrl").

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).


init(_, Req, _Opts) ->
  {ok, Req, #state{}}.

handle(Req, State=#state{}) ->
  Filename = "../../../react-js/index.html",
  Filesize = filelib:file_size(Filename),
  F = fun(Socket, Transport) ->
    Transport:sendfile(Socket, Filename)
  end,
  Req2 = cowboy_req:set_resp_body_fun(Filesize, F, Req),
  {ok, Req3} = cowboy_req:reply(200, Req2),
  {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
  ok.
