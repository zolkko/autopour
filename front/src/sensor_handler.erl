%% vim:set et tabstop=4 shiftwidth=4 nu nowrap fileencoding=utf-8:
%%
-module(sensor_handler).


-export([init/3]).
-export([content_types_provided/2]).
-export([get_sensor_json/2]).
-export([post_sensor_json/2]).


init(_Transport, _Req, _State) ->
    {upgrade, protocol, cowboy_rest}.


allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"PUT">>], Req, State}.


content_types_accepted(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, post_sensor_stats}
    ], Req, State}.


content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, get_sensor_json}
    ]}.


%% ==================================================
%% Tests whether resource exists or not
%% ==================================================
resource_exists(Request, State) ->
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
    {<<"">>, Req, State}.


%% ==================================================
%% Updates sensor statistic or register a new sensor
%% node in the database.
%% ==================================================
post_sensor_json(Request, State) ->
    case cowboy_req:binding(sensor_id,  Request) of
        {SensorId, Req} ->
            {true, Req, State};
        {_, Req} ->
            {false, Req, State}
    end.

