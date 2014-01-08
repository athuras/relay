-module(netmed_handler).
-behaviour(gen_event).
-export([
	init/1,
	handle_event/2,
	handle_call/2,
	handle_info/2,
	code_change/3,
	terminate/2
	]).

-record(state, {graph=netmed_graph:default_graph()}).

init([]) ->
	{ok, #state{}}.


handle_event({subscribe, Pid}, State) ->
	netmed_graph:subscribe(State#state.graph, Pid),
	{ok, State};

handle_event({unsubscribe, Pid}, State) ->
	netmed_graph:unsubscribe(State#state.graph, Pid),
	{ok, State};

handle_event({broadcast, Message}, State) ->
	NLP = netmed_graph:nonlocal_peers(State#state.graph),
	ok = lists:foreach(fun(X) -> gen_event:notify(X, {relay, Message}) end, NLP),
	{ok, State};

handle_event({relay, Peers, Message}, State) when is_list(Peers) ->
	{batch_notify(Peers, Message), State};

handle_event({relay, Peer, Message}, State) -> {gen_event:notify(Peer, Message), State};

handle_event({incoming, _Data}, State) ->
	LocalServices = netmed_graph:local_services(State#state.graph),
	{batch_notify(LocalServices, {incoming, _Data}), State};

handle_event({gossip, Data, Fun, Marked}, State) when is_function(Fun) ->
	io:format("~p Received Message: ~p,~p~n", [self(), Data, Fun]),
	Updated = sets:add_element(self(), Marked),
	Peers = unmarked_peers(Updated, State),
	{NewData, NewState} = Fun(Data, State),
	Message = {gossip, NewData, Fun, Updated},
	{batch_notify(Peers, Message), NewState};

handle_event(_, State) ->
	io:format("~p Received Message~n", [self()]),
	{ok, State}.


handle_call(get_state, State) ->
	{ok, State#state.graph, State};

handle_call(_, State) ->
	{ok, ok, State}.


handle_info(_, State) -> {ok, State}.


code_change(_OldVsn, State, _Extra) -> {ok, State}.


terminate(_Reason, _State) -> ok.


%%	@doc Return list of peers not yet marked, doesn't restrict to downstream.
unmarked_peers(Marked, State) ->
	G = State#state.graph,
	P = netmed_graph:nonlocal_peers(G),
	IsUnmarked = fun(X) -> not sets:is_element(X, Marked) end,
	lists:filter(IsUnmarked, P).

%%	@doc Sends Message to Each Pid in Procs, traps errors...presumably.
batch_notify(Procs, Message) when is_list(Procs) ->
	lists:foreach(fun(X) -> gen_event:notify(X, Message) end, Procs).
