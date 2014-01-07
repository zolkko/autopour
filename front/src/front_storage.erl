%% vim:set et tabstop=4 shiftwidth=4 nu nowrap fileencoding=utf-8:
%%

-module(front_storage).

-export([version/1, version/2]).
-export([redis_connection_options/1]).
-export([sensor/2, sensor/3]).

-define(VERSION_KEY, "version").
-define(VERSION_DEFAULT, <<"0.0.1-dev">>).


%% ===================================================================
%% Returns current application version. Just for testing purpose only
%% ===================================================================

version(Client) ->
    case eredis:q(Client, ["GET", ?VERSION_KEY]) of
        {ok, Version} ->
            Version;
        _ ->
            lager:warning("there is no VERSION key in the redis storage"),
            version(Client, ?VERSION_DEFAULT),
            ?VERSION_DEFAULT
    end.


%% ===================================================================
%% Makes an attempt to write application version into database
%% ===================================================================
version(Client, Version) ->
    case eredis:q(Client, ["SET", ?VERSION_KEY, ?VERSION_DEFAULT]) of
        {ok, _} ->
            Version;
        _ ->
            ?VERSION_DEFAULT
    end.


sensor_key(SensorId) ->
    << <<"sensor-">>/binary, SensorId/binary >>.


%% ===================================================================
%% Returns a sensor description
%% ===================================================================
sensor(Client, SensorId) ->
    case eredis:q(Client, ["GET", sensor_key(SensorId)]) of
        {ok, SensorData} ->
            {ts, 0};
        _ ->
            not_found
    end.


sensor(Client, SensorId, SensorData) ->
    case eredis:q(Client, ["SET", sensor_key(SensorId), <<"">>]) of
        {ok, _} ->
            true;
        _ ->
            false
    end.


%% ===================================================================
%% Returns connection options for Redis database
%% ===================================================================
redis_connection_options(Opts) ->
    case proplists:get_value(redis, Opts) of
        [Host, Port, Database, Timeout] ->
            {redis, [Host, Port, Database, Timeout]};
        _ ->
            {redis, ["localhost", 6380, 0, ""]}
    end.
