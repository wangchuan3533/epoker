-module(profile_handler).
-include("holdem.hrl").

-export([init/3]).
-export([content_types_provided/2]).
-export([get_json/2]).

init(_, _Req, _Opts) ->
	{upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
	{[{{<<"application">>, <<"json">>, '*'}, get_json}], Req, State}.

get_json(Req, State) ->
  {Uid, Req2} = cowboy_req:binding(uid, Req),
  {ok, User} = storage:get(Uid),
  Ret = jiffy:encode({?RECORD_TO_TUPLELIST(user, User)}),
	{Ret, Req2, State}.
