-module(testing).
-compile([export_all]).

new_state_manager() -> state_manager:start_link().

new_python_manager() -> python_manager:start_link().

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


do_stuff() ->
    {ok, SM} = new_state_manager(),
    {ok, PM} = new_python_manager(),
    {ok, E} = echo:start_link(),
    ok = load_queues(SM, 100),
    ok = load_upstream(SM, 100),
    {ok, SM, PM, E}.

killpy(P) ->
    gen_event:notify(P, kill_python).

get_tables(SM) ->
    state_manager:get_tables(SM).

test_local_prediction(PM, SM, E) ->
    gen_event:call(PM, predict, {gen_local_prediction, SM, E}).

test_plan(PM, SM, E) ->
    gen_event:call(PM, plan, {gen_plan, SM, E}).
