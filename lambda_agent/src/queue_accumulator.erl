%%  Encapsulates the Queue state for inbound and outbound objects.
-module(queue_accumulator).
-behaviour(gen_event).

-export([
    init/1,
    handle_event/2,
    handle_call/2,
    handle_info/2,
    code_change/3,
    terminate/2
    ]).

-record(state, {current_behaviour,
                ingress,
                egress,
                maxlen=300,
                ports=4}).

init(Initial_Behaviour) ->
    NumQueues = length(Initial_Behaviour),
    Ingress = [queue:new() || _ <- lists:seq(1, NumQueues)],
    Egress = [queue:new() || _ <- lists:seq(1, NumQueues)],
    {ok, #state{ingress=Ingress, egress=Egress,
                ports=NumQueues, current_behaviour=Initial_Behaviour}}.

%%  Event Handlers  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Add the event to the proper ingress queue,
%%  then add the expected events to the egress queues.
handle_event({incoming, Index, Time, Weight}, State) ->
    NewIngress = update_queue(State#state.ingress,
                              {Index, Time, Weight},
                              State#state.maxlen),
    X = calc_egress(Index, Weight, State#state.current_behaviour),
    Items = lists:map(fun({I, W}) -> {I, Time, W} end,
                      lists:zip(lists:seq(1, State#state.ports), X)),
    NewEgress = update_queues(State#state.egress, Items, State#state.maxlen),
    {ok, State#state{ingress=NewIngress, egress=NewEgress}};

%%  Update the current behaviour
handle_event({new_behaviour, B_mat}, State) ->
    {ok, State#state{current_behaviour=B_mat}};

handle_event(_, State) ->
    {ok, State}.


%%  Calls %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call(all_ingress, State) ->
    {ok, render_queues(State#state.ingress), State};

handle_call({get_ingress, N}, State) ->
    case N > 0 andalso N =< State#state.ports of
        true -> {ok, render_queue(lists:nth(N, State#state.ingress)), State};
        false -> {ok, {error, no_queue}, State}
    end;

handle_call({get_egress, N}, State) ->
    case N > 0 andalso N =< State#state.ports of
        true -> {ok, render_queue(lists:nth(N, State#state.egress)), State};
        false -> {ok, {error, no_queue}, State}
    end;

handle_call(_, State) ->
    {ok, ok, State}.


%%  Misc  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_info(_, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%  Private  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec shorten_queue(queue(), integer()) -> queue().
shorten_queue(Q, 0) -> Q;
shorten_queue(Q, N) ->
    {_, Q2} = queue:out(Q),
    shorten_queue(Q2, N-1).

-spec render_queue(queue()) -> list().
render_queue(Q) -> queue:to_list(Q).

-spec render_queues(list()) -> list().
render_queues(ListOfQueues) ->
    [queue:to_list(X) || X <- ListOfQueues].

-spec calc_egress(integer(), float(), list()) -> list().
calc_egress(Index, Weight, B) ->
    X = set_nth(Index, lol_matrices:zeros(length(B)), Weight),
    lol_matrices:vec_dot_mat(X, B).

-spec update_queue(list(), {integer(), term(), float()}, integer()) -> list().
update_queue(Qs, {_, _, W}, _) when W == 0 -> Qs;
update_queue(ListOfQueues, {Index, Time, Weight}, MaxLength) ->
    Q = queue:in({Time, Weight}, lists:nth(Index, ListOfQueues)),
    N = MaxLength - queue:len(Q),
    Q2 = case N < 0 of
        true -> shorten_queue(Q, -N);
        false -> Q
    end,
    set_nth(Index, ListOfQueues, Q2).

-spec update_queues(list(), list(), integer()) -> list().
update_queues(Qs, [], _) -> Qs;
update_queues(Qs, [Item|T], MaxLength) ->
    update_queues(update_queue(Qs, Item, MaxLength), T, MaxLength).

-spec set_nth(integer(), list(), term()) -> list().
set_nth(1, [_|L], New) -> [New|L];
set_nth(I, [H|T], New) -> [H|set_nth(I - 1, T, New)].
