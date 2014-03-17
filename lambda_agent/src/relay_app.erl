-module(relay_app).
-behaviour(application).

-define(LISTEN_PORT, 8081).

-export([start/2,
         stop/1]).

%%  API
start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
                {'_', [
                        {"/", top_page_handler, []}
                        ]}
                ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, ?LISTEN_PORT}], [
                {env, [{dispatch, Dispatch}]}
                ]),
    {ok, S1,
     [P1, PyState1, E1]} =
            relay_agent:start(worker, [<<"Agent1">>, undefined]),
    {ok, S2, [P2, PyState2, E2]} =
            relay_agent:start(worker, [<<"Agent2">>, PyState1]),
    relay_sup:start_link().

stop(_State) ->
    ok.
