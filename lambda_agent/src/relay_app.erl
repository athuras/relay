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
     [P1, PyPid, R1]} =
            relay_agent:start(worker, [<<"Agent1">>, undefined]),
    Workers = make_workers([<<"Agent2">>, <<"Agent3">>,
                            <<"Agent4">>, <<"Agent5">>],
                           PyPid),
    Routers = [R1|lists:map(fun({ok, _, [_, _, R]}) -> R end, Workers)],
    spider:spin(Routers),
    relay_sup:start_link().

stop(_State) ->
    ok.

make_workers(Names, PyPid) ->
    [relay_agent:start(worker, [N, PyPid]) || N <- Names].
