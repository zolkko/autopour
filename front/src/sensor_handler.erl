%% vim:set et tabstop=4 shiftwidth=4 nu nowrap fileencoding=utf-8:
%%
-module(sensor_handler).


-export([init/3]).
-export([rest_init/2]).
-export([rest_terminate/2]).
-export([content_types_provided/2]).
-export([get_sensor_json/2]).
-export([post_sensor_json/2]).


init(_Transport, _Req, _State) ->
    {upgrade, protocol, cowboy_rest}.


rest_init(Request, Opts) ->
    lager:info("initializing sensor REST service"),
    case apply(eredis, start_link, proplists:get_value(redis, Opts)) of
        {ok, RedisClient} ->
            lager:info("connection to redis database has been established in sensor REST service"),
            {ok, Request, RedisClient};
        _ ->
            lager:warning("failed to connect to redis database in sensor REST service"),
            {ok, Request, no_state}
    end.


%% ==================================================
%% It is called when REST handler gets terminating
%% ==================================================
rest_terminate(_Req, State) when State == no_state ->
    lager:info("terminating sensor REST service"),
    ok;

rest_terminate(_Req, RedisConnection) ->
    lager:info("terminating sensor REST service and redis connection"),
    eredis:stop(RedisConnection),
    ok.

%% ==================================================
%%
%% ==================================================
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"PUT">>], Req, State}.


content_types_accepted(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, post_sensor_stats}
    ], Req, State}.


content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, get_sensor_json}
    ], Req, State}.


%% ==================================================
%% Tests whether resource exists or not
%% ==================================================
resource_exists(Request, State) ->
    lager:info("resource_exists called"),

    case cowboy_req:binding(sid, Request) of
        {undefined, Req} ->
            {true, Req, index};
        {SensorId, Req} ->
            %% TODO: validate sensor identifier. If sensor does not exists
            %% then {false, Req2, SensorId}
            {true, Req, SensorId}
    end.


%% ==================================================
%% Returns sensor data as json object
%% ==================================================
get_sensor_json(Req, State) ->
    {SensorId, _} = cowboy_req:binding(sensor_id, Req),
    lager:info("return data for sensor ~p", [SensorId]),
    {<<"">>, Req, State}.


%% ==================================================
%% Updates sensor statistic or register a new sensor
%% node in the database.
%% ==================================================
post_sensor_json(Request, State) ->
    lager:info("post_sensor_json has been called"),
    case cowboy_req:binding(sensor_id,  Request) of
        {SensorId, Req} ->
            {true, Req, State};
        {_, Req} ->
            {false, Req, State}
    end.
