%% vim:set et tabstop=4 shiftwidth=4 nu nowrap fileencoding=utf-8:
%%
%% Handler returns the service version. Just to have a landing site. Also
%% this API maybe used to verify that the service is alive.
%%
-module(index_handler).


-export([init/3, handle/2, terminate/3]).


init(_Transport, Req, []) ->
    io:write("1"),
    {ok, Req, undefined}.


handle(Req, State) ->
    {Method, Req1} = cowboy_req:method(Req),
    {ok, Req1} = handle(Method, Req, State),
    {ok, Req1, State}.

handle(<<"GET">>, Req, _State) ->
    cowboy_req:reply(200, [
        {<<"Content-Type">>, <<"application/json">>}
    ], <<"{\"version\":\"0.0.1\"}">>, Req);

handle(_Other, Req, _State) ->
    cowboy_req:reply(405, [], <<"Method not allowed">>, Req).


terminate(_Reason, _Request, _State) ->
    ok.

