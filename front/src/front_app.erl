%% vim:set et tabstop=4 shiftwidth=4 nu nowrap fileencoding=utf-8:
%%
-module(front_app).


-behaviour(application).


%% Application callbacks
-export([start/2, stop/1]).


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    lager:info("connecting to redis server"),
    {ok, RedisClient} = eredis:start_link(),
    lager:info("connection to redis server has been established"),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", index_handler, [
                {redis, RedisClient}
            ]},
            {"/sensor/[:sensor_id]", sensor_handler, [RedisClient]}
        ]}
    ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8281}], [
        {env, [{dispatch, Dispatch}]}
    ]),
    front_sup:start_link().


stop(_State) ->
    lager:info("Finalizing application"),
    ok.

