-module(relay_agent).
-compile([export_all]).
%%  API
-export([
    start_link/0,
    get_info/1,
    test_inc/1
    ]).

%%  API
start_link() ->
    {ok, Pid} = gen_event:start_link(),
    gen_event:add_handler(Pid, ingress_accumulator, []),
    gen_event:add_handler(Pid, relay_store, []),
    %gen_event:add_handler(Pid, prediction_engine, []),
    %gen_event:add_handler(Pid, btg_planner, []),
    {ok, Pid}.

get_info(Pid) ->
    gen_event:call(Pid, ingress_accumulator, render_queues).


load_queues(Pid) ->
    lists:foreach(fun(X) -> gen_event:notify(Pid, test_inc(2)) end,
                  lists:seq(1, 10)).
test_inc(Idx) ->
    {incoming, Idx, current_time(), 1}.

current_time() ->
    element(1, now()) * 10000 + element(2, now()).
