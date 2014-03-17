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
     [_P1, PyPid, R1]} =
            relay_agent:start(worker, [<<"Agent1">>, undefined]),
    Workers = make_workers([<<"Agent2">>, <<"Agent3">>,
                            <<"Agent4">>, <<"Agent5">>],
                           PyPid),
    Routers = [R1|lists:map(fun({ok, _, [_, _, R]}) -> R end, Workers)],
    SMs = [S1|lists:map(fun({ok, SM, _}) -> SM end, Workers)],
    batch_set_taus(SMs),
    spider:spin(Routers),
    relay_sup:start_link().

stop(_State) ->
    ok.

make_workers(Names, PyPid) ->
    [relay_agent:start(worker, [N, PyPid]) || N <- Names].

%%  Hard-Coded for Demo
batch_set_taus(SMs) ->
    Updates = [
            [{2, [3, 20, 6, 1]}, {3, [100, -50, 0.75, 1]}],  %% A
            [{4, [3, 20, 6, 1]}, {3, [6, 13, 3, 1]}, {2, [30, 5, 1, 1]}],  %% B
            [{4, [30, 5, 1, 1]}, {3, [2, 30, 3, 1]}, {1, [285, -78, 0.39, 1]}],  % C
            [{4, [100, -50, 0.75, 1]}, {1, [6, 13, 3, 1]}, {2, [332, -45, 0.22, 1]}],  % D
            [{4, [332, -45, 0.22, 1]}, {2, [285, -78, 0.39, 1]}, {3, [2, 30, 3, 1]}]  % E
            ],
    lists:foreach(fun({SM, Tau_list}) ->
                lists:foreach(fun({X, [Shape, Loc, Scale, Gain]}) ->
                    gen_event:notify(SM, {set_delay_param, X, [Shape, Loc, Scale, Gain]})
                    end, Tau_list) end,
                lists:zip(SMs, Updates)).
