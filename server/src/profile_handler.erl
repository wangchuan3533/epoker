-module(profile_handler).

-export([init/3]).
-export([content_types_provided/2]).
-export([get_html/2]).

init(_, _Req, _Opts) ->
	{upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
  {B, _} = cowboy_req:binding(pid, Req),
  Pid = list_to_integer(binary_to_list(B)),
  ok = io:format("~p~n", [Pid]),
	{[{{<<"text">>, <<"html">>, '*'}, get_html}], Req, State}.

get_html(Req, State) ->
	{<<"<html><body>This is REST!</body></html>">>, Req, State}.
