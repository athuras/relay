-module(plan).
-compile([export_all]).
%%  gen_event
-export([
    code_change/3,
    handle_call/2,
    handle_event/2,
    handle_info/2,
    init/1,
    remote_gen_plan/1,
    terminate/2
    ]).
-export([notify_behaviour/2]).

-record(state, {py_pid, stuff}).

init(PyPid) ->
    {ok, #state{py_pid=PyPid}}.

%%  Events  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_event(kill_python, State) ->
    {python:stop(State#state.py_pid), State};
handle_event(_Event, State) ->
    {ok, State}.

%%  Calls  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call({gen_plan, Store, E}, State=#state{py_pid=P}) ->
    Pid = spawn(?MODULE, remote_gen_plan, [{Store, P, E}]),
    {ok, Pid, State};

handle_call(_, State) ->
    {ok, ok, State}.

%%  Misc  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_info(_, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%  Private Methods  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

remote_gen_plan({Store, P, Listener}) ->
    {ok, {BTG_Tid, B_Tid}} = gen_event:call(Store, relay_store, get_tables),
    B_id = gen_event:call(Store, relay_store, get_current_state),
    {[Start], F} = gen_event:call(Store, relay_store, get_prediction),
    Args = prepare_plan_args(BTG_Tid, B_Tid, B_id, Start, F),
    {Plan, Times, First} = try python:call(P, graph_select, optimize_path, Args)
             catch error:{python, ExClass, ExArgs, Stack} ->
                Listener ! {error, ExClass, ExArgs, Stack},
            {[0], [0], 0}
            end,
    Next_B = lists:nth(1, Plan),
    timer:apply_after(First * 1000, ?MODULE, notify_behaviour, [Next_B, Store]),
    gen_event:notify(Store, {new_plan, {Plan, Times}}),

    exit(normal).
    %gen_event:notify(Controller, {new_plan, done, self()}).

notify_behaviour(Next_B, Store) ->
    gen_event:notify(Store, {set_behaviour, Next_B}).


-spec table_to_list(ets:tid()) -> list().
table_to_list(Tid) ->
    ets:foldl(fun(X, Acc) -> [X|Acc] end, [], Tid).

-spec prepare_plan_args(ets:tid(), ets:tid(), integer(), integer(), list()) -> list().
prepare_plan_args(BTG_Tid, B_Tid, Current_Bid, Start, F) ->
    BTG_Edges = table_to_list(BTG_Tid),
    B_Table = table_to_list(B_Tid),
    B = [lists:sum(X) || X <- lookup_behaviour(B_Tid, Current_Bid)],
    {S2, Predict} = case F of
        [] -> {state_manager:clock(),
               gen_random_prediction(length(B), 100)};
            _ -> {Start, F}
    end,
    Paths = lists:foldl(fun(X, Y) ->
                    gen_paths(BTG_Edges, Current_Bid, X) ++ Y
            end, [], lists:seq(2, 3)),
    [Paths, B_Table, BTG_Edges, Start, Predict, 1.0, 15].

-spec gen_paths(list(), integer(), integer()) -> list().
gen_paths(Edges, Start, Depth) ->
    %%  Make a {Node -> [Adjacents]} dict.
    X = lists:map(fun({{A, B}, _}) -> {A, B} end, Edges),
    D = lists:foldl(fun({A, B}, Acc) -> orddict:append(A, B, Acc) end,
                    orddict:new(),
                    X),
    pathfinder(D, [Start], Depth).

-spec pathfinder(dict(), list(), integer()) -> list().
pathfinder(_, Path, 0) ->
    list_to_tuple(lists:reverse(Path));
pathfinder(D, Path, N) ->
    Branches = get_neighbours(lists:nth(1, Path), D),
    Paths = lists:map(fun(X) -> [X] ++ Path end, Branches),
    lists:flatten(
        lists:foldl(fun(X, Y) -> [pathfinder(D, X, N-1)] ++ Y end, [], Paths)
        ).

get_neighbours(Start, D) -> orddict:fetch(Start, D).

-spec gen_random_prediction(integer(), integer()) -> list().
gen_random_prediction(Ports, N) ->
    [[random:uniform() || _ <- lists:seq(1, N)]
        || _ <- lists:seq(1, Ports)].

-spec gen_zeros(integer(), integer()) -> list().
gen_zeros(Ports, N) ->
    [lol_matrices:zeros(N) || _ <- lists:seq(1, Ports)].

-spec lookup_behaviour(ets:tid(), integer()) -> list().
lookup_behaviour(Tid, Bid) ->
    L = ets:lookup(Tid, Bid),
    case L == [] of
        true -> [];
        false -> {_, B} = lists:nth(1, L), B
    end.
