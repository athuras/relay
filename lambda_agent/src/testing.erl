-module(testing).
-compile([export_all]).

new_state_manager() ->
    state_manager:start_link().

load_queues(Pid) -> load_queues(Pid, 25).
load_queues(Pid, N) when is_integer(N) ->
    lists:foreach(fun(_) -> gen_event:notify(Pid, test_incoming(random:uniform(4))) end,
                  lists:seq(1, N)).

test_incoming(N) ->
    {incoming, N, current_time(), 1}.

current_time() ->
    element(1, now()) * 10000 + element(2, now()).

do_stuff() ->
    {ok, R} = new_state_manager(),
    ok = load_queues(R, 15),
    R.
