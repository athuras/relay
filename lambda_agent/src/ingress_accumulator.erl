-module(ingress_accumulator).
-behaviour(gen_event).

-export([
    init/1,
    handle_event/2,
    handle_call/2,
    handle_info/2,
    code_change/3,
    terminate/2
    ]).

-record(state, {queues, maxlen=256}).

init([]) -> init([4]);
init([NumQueues]) ->
    Qs = orddict:from_list(
            lists:map(fun(X) -> {X, queue:new()} end,
                      lists:seq(0, NumQueues - 1))
            ),
    {ok, #state{queues=Qs}}.

handle_event({incoming, Index, Time, Weight}, State) ->
    NewDict = update_queue(State#state.queues,
                           {Index, Time, Weight},
                           State#state.maxlen),
    NewState = State#state{queues=NewDict},
    {ok, NewState};
handle_event(_, State) ->
    {ok, State}.

handle_call(render_queues, State) ->
    R = render_queues(State#state.queues),
    {ok, R, State};
handle_call(_, State) ->
    {ok, ok, State}.

handle_info(_, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.


%%  Private Methods
shorten_queue(Q, 0) -> Q;
shorten_queue(Q, N) ->
    {_, Q2} = queue:out(Q),
    shorten_queue(Q2, N-1).

%%  -> [{queue_index, [{time, weight}]}]
render_queues(DictOfQueues) ->
    lists:map(fun({Id, Q}) -> {Id, queue:to_list(Q)} end,
              orddict:to_list(DictOfQueues)
             ).

update_queue(DictOfQueues, {Index, Time, Weight}, MaxLength) ->
    Q = queue:in({Time, Weight},
                 orddict:fetch(Index, DictOfQueues)
                ),
    N = MaxLength - queue:len(Q),
    Q2 = case N < 0 of
        true -> shorten_queue(Q, -N);
        false -> Q
    end,
    orddict:update(Index, fun(_) -> Q2 end, DictOfQueues).
