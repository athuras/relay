-module(testing).
-compile([export_all]).

load_queues(Pid) -> load_queues(Pid, 25).
load_queues(Pid, N) when is_integer(N) ->
    lists:foreach(fun(_) ->
                gen_event:notify(Pid, test_incoming()) end,
                  lists:seq(1, N)).

load_upstream(Pid, N) when is_integer(N) ->
    lists:foreach(fun(_) ->
                gen_event:notify(Pid, test_incoming_upstream()) end,
                  lists:seq(1, N)).

test_incoming() ->
    {incoming, random:uniform(4), state_manager:clock(), 1}.
test_incoming_upstream() ->
    {incoming_upstream, random:uniform(4), state_manager:clock(), random:uniform()}.


new_agent(Name) ->
    {ok, SM} = state_manager:start_link(worker, []),
    {ok, PM, [Py]} = python_manager:start_link(worker, []),
    {ok, E} = echo:start_link(),
    register(Name, SM),
    ok = load_queues(SM, 20),
    ok = load_upstream(SM, 20),
    test_local_prediction({PM, SM, E}),
    {SM, PM, E}.


killpy(P) ->
    gen_event:notify(P, kill_python).

get_tables(SM) ->
    {ok, {BTG, BT}} = state_manager:get_tables(SM),
    {BTG, BT}.

test_local_prediction({SM, PM, E}) ->
    gen_event:call(PM, predict, {gen_local_prediction, SM, E}).

test_plan({SM, PM, E}) ->
    gen_event:call(PM, plan, {gen_plan, SM, E}).

get_plan({SM, PM, E}) ->
    gen_event:call(SM, relay_store, get_current_plan).

get_prediction({SM, PM, E}) ->
    gen_event:call(SM, relay_store, get_prediction).

request_state({SM, PM, E}) ->
    gen_event:call(SM, relay_store, request_state).
