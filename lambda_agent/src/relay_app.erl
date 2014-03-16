-module(relay_app).
-behaviour(application).

-define(LISTEN_PORT, 8081).

-export([start/2,
         stop/1]).

%%  API
start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
                {'_', [
                        {"/", toppage_handler, []}
                        ]}
                ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, ?LISTEN_PORT}], [
                {env, [{dispatch, Dispatch}]}
                ]),
    relay_sup:start_link().

stop(_State) ->
    ok.
