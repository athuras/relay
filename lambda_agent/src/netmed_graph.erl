-module(netmed_graph).
-export([
	add_edge/3,
	add_edge/4,
	default_graph/0,
	default_graph/1,
	nonlocal_peers/1,
	subscribe/2,
	subscribe/3,
	subscribe/4,
	unsubscribe/2,
	unsubscribe/3
	]).

%%	Interface Functions  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%	@doc Returns a graph with a single vertex corresponding to self()
default_graph() -> default_graph(self()).

%%	@doc Returns a graph with a single vertex corresponding to Pid, it is given
%%	the special label 'local'.
default_graph(Pid) ->
	G = digraph:new(),
	digraph:add_vertex(G, Pid, local),
	G.

%%	@doc Adds an edge emanating from Center to Pid.
subscribe(G, Pid) -> subscribe(G, Pid, self(), []).
subscribe(G, Pid, Center) -> subscribe(G, Pid, Center, []).
subscribe(G, Pid, Center, Label) ->
	add_edge(G, Center, Pid, Label),
	ok.

%% @doc Deletes the edge incident on Center from Pid. More Generally, deletes each edge in the simple
%%		path from Center to Pid. This only works because there SHOULD ONLY EVER BE ONE PATH TO DELETE!
unsubscribe(G, Pid) -> unsubscribe(G, Pid, self()).
unsubscribe(G, Pid, Center) ->
	E = digraph:get_path(G, Pid, Center),
	Edges = lists:map(fun(X) -> ['$e'|X] end, E),
	digraph:del_edges(G, Edges).

%%	@doc remote peer vertices.
nonlocal_peers(G) ->
	Pred = fun(X) -> X =/= local end,
	lists:map(fun({V, _}) -> V end,
		label_filter(vertex_data(G), Pred)).

%%	Graph Utility Functions  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	mostly wrappers (from refactor)

add_edge(G, A, B) -> graph_utils:add_edge(G, A, B).
add_edge(G, A, B, Label) -> graph_utils:add_edge(G, A, B, Label).

vertex_data(G) -> graph_utils:vertex_data(G).

label_filter(V, Predicate) -> graph_utils:label_filter(V, Predicate).
