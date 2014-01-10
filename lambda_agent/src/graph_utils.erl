%% A little more abstraction over the digraph module.

-module(graph_utils).
-export([add_edge/3,
         add_edge/4,
         vertex_data/1,
         label_filter/2]).

%% @doc Add an edge to the graph, inserting a new vertex if necessary
add_edge(G, A, B) -> add_edge(G, A, B, []).
add_edge(G, A, B, Label) ->
    case digraph:add_edge(G, A, B, Label) of
        {error, {bad_vertex, V}} ->
            digraph:add_vertex(G, V),
            add_edge(G, A, B, Label);
        {error, {bad_edge, _}} -> throw(bad_edge);
        _ -> ok
    end.

%% @doc Return [{digraph:vertex(), Label}] for the graph G
vertex_data(G) -> vtx_iter(G, digraph:vertices(G), []).

vtx_iter(_, [], Acc) -> Acc;
vtx_iter(G, [V|Vertices], Acc) ->
    vtx_iter(G, Vertices, [digraph:vertex(G, V)|Acc]).

%% @doc Applies Predicate to the label() in a vertex tuple
label_filter(V, Predicate) ->
    lists:filter(fun({_, L}) -> Predicate(L) end, V).
