-module(login_handler).
-behaviour(cowboy_http_handler).
-include("holdem.hrl").


-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).
generate_uid() -> binary:list_to_bin(uuid:uuid_to_string(uuid:get_v4())).

init(_, Req, _Opts) ->
	{ok, Req, #state{}}.

handle(Req, State=#state{}) ->
  {Method, Req2} = cowboy_req:method(Req),
  {ok, ContentType, Req3} = cowboy_req:parse_header(<<"content-type">>, Req2),
  {ok, Body, Req4} = cowboy_req:body(Req3),
  case {Method, ContentType} of
    {<<"POST">>, {<<"application">>, <<"json">>, _}} ->
      {JsonReqList} = jiffy:decode(Body),
      case lists:keyfind(<<"register">>, 1, JsonReqList) of
      {<<"register">>, true} ->
        Uid = generate_uid(),
        PlayerDb = #player_db{id = Uid, name = Uid, password = Uid, chips = ?INIT_CHIPS},
        ok = storage:set(PlayerDb),
        Ret = jiffy:encode({?RECORD_TO_TUPLELIST(player_db, PlayerDb)}),
        {ok, Req5} = cowboy_req:reply(200, cowboy_req:set_resp_body(Ret, Req4)),
        {ok, Req5, State};
      _Other -> %% not register
        {<<"uid">>, Uid} = lists:keyfind(<<"uid">>, 1, JsonReqList),
        {<<"password">>, Password} = lists:keyfind(<<"password">>, 1, JsonReqList),
        %ok = io:format("~p~n", [Password]),
        {ok, Req5} = case storage:get(Uid) of
          {ok, #player_db{id = Uid, password = Password}} ->
            cowboy_req:reply(200, cowboy_req:set_resp_body(binary:list_to_bin(io_lib:format("{\"token\":\"~s\"}", [Uid])), Req4));
          {ok, _WrongPlayer} ->
            cowboy_req:reply(403, cowboy_req:set_resp_body(<<"{\"errmsg\":\"wrong password\"}">>, Req4));
          {error, _Reason} ->
            cowboy_req:reply(403, cowboy_req:set_resp_body(<<"{\"errmsg\":\"user not exist\"}">>, Req4))
        end,
        {ok, Req5, State}
      end;
    _Other ->
      {ok, Req5} = cowboy_req:reply(403, cowboy_req:set_resp_body(<<"{\"errmsg\":\"invalid format\"}">>, Req4)),
      {ok, Req5, State}
  end.

terminate(_Reason, _Req, _State) ->
	ok.
