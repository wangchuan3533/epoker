-module(index_handler).
-behaviour(cowboy_http_handler).
-include("holdem.hrl").

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

generate_session_id() -> uuid:uuid_to_string(uuid:get_v4()).

init(_, Req, _Opts) ->
  {ok, Req, #state{}}.

handle(Req, State=#state{}) ->
  {SessionId, Req2} = cowboy_req:cookie(<<"sessionid">>, Req, <<"undefined">>),
  Req3 = if SessionId == <<"undefined">> ->
    SessionId1 = generate_session_id(),
    User = #user{id = SessionId1, name = SessionId1, chips = ?INIT_CHIPS},
    ok = storage:set(User),
    cowboy_req:set_resp_cookie(<<"sessionid">>, SessionId1, [], Req2);
  true ->
    case storage:get(binary:bin_to_list(SessionId)) of
      {ok, User} ->
        io:format("welcome back ~p~n", [User]);
      {error, Reason} ->
        io:format("auth failed reason ~p~n", [Reason])
    end,
    Req2
  end,
  %%{ok, Req4} = cowboy_req:reply(303, [{<<"location">>, <<"/public/index.html">>}], Req3),
  Filename = "../../../client/index.html",
  Filesize = filelib:file_size(Filename),
  F = fun(Socket, Transport) ->
    Transport:sendfile(Socket, Filename)
  end,
  Req4 = cowboy_req:set_resp_body_fun(Filesize, F, Req3),
  {ok, Req5} = cowboy_req:reply(200, Req4),
  {ok, Req5, State}.

terminate(_Reason, _Req, _State) ->
  ok.
