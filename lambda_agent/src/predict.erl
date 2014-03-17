-module(predict).

%%  gen_event
-export([
    init/1,
    handle_event/2,
    handle_call/2,
    handle_info/2,
    code_change/3,
    remote_gen_local_prediction/1,
    terminate/2
    ]).

-record(state, {py_pid, listener}).

init(PyPid) ->
    {ok, #state{py_pid=PyPid}}.

%%  Events %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_event(kill_python, State) ->
    {python:stop(State#state.py_pid), State};
handle_event(_Event, State) ->
    {ok, State}.


%%  Calls %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call(test, State=#state{py_pid=P}) ->
    {ok, python:call(P, test, test, [lists:seq(1, 10)]), State};

handle_call({gen_local_prediction, Store, L}, State=#state{py_pid=P}) ->
    Pid = spawn(?MODULE, remote_gen_local_prediction, [{Store, P, 60, L}]),
    %% Some Stuff
    {ok, Pid, State};

handle_call({gen_params, StorePid}, State) ->
    _Store = gen_event:call(StorePid, relay_store, prediction_info),
    %% Some Stuff
    {ok, these_are_the_new_params, State};

handle_call(_, State) ->
    {ok, ok, State}.


%%  Misc  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_info(_, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, State) ->
    python:stop(State#state.py_pid).


%%  Private Methods %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remote_gen_local_prediction({Store, P, T, Listener}) ->
    Delay_Params = gen_event:call(Store, relay_store, get_delay_params),
    Remote_Egress = gen_event:call(Store, accumulator, get_remote_egress),
    {Time, Prediction} =
        try python:call(P, predictions, gen_local_prediction,
                        [Remote_Egress, Delay_Params,
                         state_manager:clock(), T])
        catch error:{python, ExClass, ExArgs, Stack} ->
            Listener ! {error, ExClass, ExArgs, Stack},
            {error, ExClass, ExArgs, Stack}
    end,
    gen_event:notify(Store, {new_prediction, {Time, Prediction}}),
    exit(normal).
