%% vim:set et tabstop=4 shiftwidth=4 nu nowrap fileencoding=utf-8:
%%
-module(front_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).


%% ===================================================================
%% Application callbacks
%% ===================================================================
start(_StartType, StartArgs) ->
    RedisConnectionOptions = front_storage:redis_connection_options(StartArgs),
    lager:info("redis server ~p will be used", [RedisConnectionOptions]),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", index_handler, [
                RedisConnectionOptions
            ]},
            {"/sensor/[:sensor_id]", sensor_handler, [
                RedisConnectionOptions
            ]}
        ]}
    ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8281}], [
        {env, [{dispatch, Dispatch}]}
    ]),
    front_sup:start_link().


stop(_State) ->
    lager:info("Finalizing application"),
    ok.
