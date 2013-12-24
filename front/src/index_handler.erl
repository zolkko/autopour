%% vim:set et tabstop=4 shiftwidth=4 nu nowrap fileencoding=utf-8:
%%
%% Handler returns the service version. Just to have a landing site. Also
%% this API maybe used to verify that the service is alive.
%%
-module(index_handler).

-export([init/3, handle/2, terminate/3]).


init(_Transport, Request, Opts) ->
    lager:info("initializing index_handler module"),
    case lists:keyfind(redis, 1, Opts) of
        {redis, RedisClient} ->
            {ok, Request, RedisClient};
        _ ->
            {ok, Request, no_state}
    end.


handle(Req, State) ->
    lager:info("index request received"),
    {Method, Req1} = cowboy_req:method(Req),
    {ok, Req1} = handle(Method, Req, State),
    {ok, Req1, State}.

handle(<<"GET">>, Req, State) ->
    lager:info("index_handler module is returning API version"),
    case State of
        no_state ->
            lager:warning("redis client connection expected"),
            cowboy_req:reply(500, [
                {<<"Content-Type">>, <<"text/plain">>}
            ], <<"Server Error">>, Req);
        RedisClient ->
            RespStart = <<"{\"version\":\"">>,
            RespVersion = front_store:version(RedisClient),
            RespEnd = <<"\"}">>,
            cowboy_req:reply(200, [
                {<<"Content-Type">>, <<"application/json">>}
            ], <<RespStart/binary, RespVersion/binary, RespEnd/binary>>, Req)
    end;

handle(Method, Req, _State) ->
    lager:warning("method ~s is not supported by index_handler module", [Method]),
    cowboy_req:reply(405, [], <<"Method not allowed">>, Req).


terminate(_Reason, _Request, _State) ->
    lager:info("terminating index_handler"),
    ok.

