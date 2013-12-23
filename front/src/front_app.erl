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
    lager:info("Application start routine has been executed"),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", index_handler, []}
        ]}
    ]),
    lager:info("Application routes have been compiled"),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8281}], [
        {env, [{dispatch, Dispatch}]}
    ]),
    lager:info("HTTP server has been started"),
    front_sup:start_link().


stop(_State) ->
    lager:info("Finalizing application"),
    ok.

