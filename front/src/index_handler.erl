%% vim:set et tabstop=4 shiftwidth=4 nu nowrap fileencoding=utf-8:
%%
%% Handler returns the service version. Just to have a landing site. Also
%% this API maybe used to verify that the service is alive.
%%
-module(index_handler).

-export([init/3, handle/2, terminate/3]).


init(_Transport, Request, Opts) ->
    lager:info("initializing index_handler module"),
    case apply(eredis, start_link, proplists:get_value(redis, Opts)) of
        {ok, RedisClient} ->
            lager:info("connection to redis database has been established"),
            {ok, Request, RedisClient};
        _ ->
            lager:warning("failed to connect to redis database"),
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
            cowboy_req:reply(500, [
                {<<"Content-Type">>, <<"text/plain; charset=utf-8">>}
            ], <<"Internal Server Error">>, Req);
        RedisClient ->
            Version = front_storage:version(RedisClient),
            JsonData = jiffy:encode({[{version,  Version}]}),
            cowboy_req:reply(200, [
                {<<"Content-Type">>, <<"application/json; charset=utf-8">>}
            ], JsonData, Req)
    end;

handle(Method, Req, _State) ->
    lager:warning("method ~s is not supported by index_handler module", [Method]),
    cowboy_req:reply(405, [], <<"Method not allowed">>, Req).


terminate(_Reason, _Request, State) ->
    lager:info("terminating index_handler"),
    case State of
        no_state ->
            lager:info("no need to stop redis connection");
        RedisClient ->
            lager:info("stopping redis connection"),
            eredis:stop(RedisClient)
    end,
    ok.
