%% A little more abstraction over the digraph module.

-module(graph_utils).
-export([combine/4,
         add_edge/3,
         add_edge/4,
         get_edge_data/2,
         vertex_combine/3,
         edge_combine/3,
         vertex_data/1,
         label_filter/2]).

%% @doc Imagine a world where you could perform combinatory operations on graphs!
%%      Welcome to this world. Also, B must be a proper subset of A... Sorry.
%% @spec combine(G, B, V_op, E_op) -> digraph().
%%              V_op(G.vertex(), B.vertex()) -> vertex(),
%%              E_op(G.edge(), B.edge()) -> edge()
%% @end
combine(A, B, V_func, E_func) when is_function(V_func) and is_function(E_func) ->
    Common_vertices = vertex_combine(A, B, V_func),
    Common_edges = edge_combine(A, B, E_func),
    G = digraph:new(),
    lists:foreach(fun({V, Label}) -> digraph:add_vertex(G, V, Label) end, Common_vertices),
    lists:foreach(fun({V1, V2, Label}) -> digraph:add_edge(G, V1, V2, Label) end, Common_edges),
    G.

%% @doc Generates an edgelist common to both graphs A, and B.
%%      The labels of the resulting edge_lists have labels that are the result
%%      of calling F with the labels from edges originating from A and B respectively
%% @spec edge_combine(A, B, F) -> [{V1, V2, Label}] | {error, Reason}
%% @end
edge_combine(A, B, F) when is_function(F) ->
    Left_edges = lists:map(
            fun(E) ->
                {V1, V2, _} = get_edge_data(A, E),
                {V1, V2}
            end,
            digraph:edges(A)
        ),
    Right_edges = lists:map(
            fun(E) ->
                {V1, V2, _} = get_edge_data(B, E),
                {V1, V2} end,
            digraph:edges(B)
        ),
    Common_edge_pairs = sets:intersection(
            sets:from_list(Left_edges),
            sets:from_list(Right_edges)
        ),
    %% Get a list of {V1, V2, Label} triples, for each graph: A, B
    Edge_meta = lists:map(
            fun(G) ->
                sets:fold(
                    fun({V1, V2}, Acc) -> [edge_lookup(G, V1, V2)|Acc] end, [],
                    Common_edge_pairs
                )
            end,
            [A, B]
        ),
    %% Combine the two lists into [{V1, V2, F(L1, L2)}]
    lists:zipwith(
        fun({X, Y, L1}, {_, _, L2}) ->
            {X, Y, F(L1, L2)}
        end,
        Edge_meta
    ).

%% @doc Generates a Vertex list common to both graphs A, and B.
%%      The labels of the resulting vertices are the result of calling F
%%      with the labels from vertices originating from A, and B respectively
%% @spec vertex_combine(A, B, F) -> [digraph:vertex()] | {error, Reason}.
%% @end
vertex_combine(A, B, F) when is_function(F) ->
    Vertex_sets = lists:map(
            fun(X) -> sets:from_list(digraph:vertices(X)) end,
            [A, B]),
    Common = sets:intersection(Vertex_sets),
    sets:fold(
        fun(V, Acc) ->
            {_, L1} = digraph:vertex(A, V),
            {_, L2} = digraph:vertex(B, V),
            [{V, F(L1, L2)} | Acc]
        end, [],
        Common).

%% @doc Return the invariant information about a particular edge, essentially
%%      a wrapper on digraph:edge/2
%% @spec get_edge_data(G, E) -> {digraph:vertex(), digraph:vertex(), Label} | {error, Reason}
%% @end
get_edge_data(G, Edge) ->
    {_, V1, V2, Label} = digraph:edge(G, Edge),
    {V1, V2, Label}.

%% @doc Add an edge to the graph, inserting a new unlabelled vertex if necessary
%% @end
add_edge(G, A, B) -> add_edge(G, A, B, []).
add_edge(G, A, B, Label) ->
    case digraph:add_edge(G, A, B, Label) of
        {error, {bad_vertex, V}} ->
            digraph:add_vertex(G, V),
            add_edge(G, A, B, Label);
        {error, {bad_edge, _}} -> throw(bad_edge);
        _ -> ok
    end.

%% @doc Given two vertices: VA,  VB; return an edge that traverses from A to B.
%%      uses ordsets due to small expected size of the Graphs.
%% @spec edge_lookup(G, A, B) -> {A, B, Label} | {error, no_edge} | {error, Reason}
%% @end
edge_lookup(G, VA, VB) ->
    Get_data = fun(X) ->
            {_, V1, V2, Label} = digraph:edge(G, X),
            {V1, V2, Label}
        end,
    A_out = lists:map(Get_data, digraph:out_edges(G, VA)),
    B_in = lists:map(Get_data, digraph:in_edges(G, VB)),

    %% Get a list of the edges [{VA, VB, Label}]
    Result = ordsets:to_list(
            ordsets:intersection(
                lists:map(fun(X) -> ordsets:from_list(X) end,
                          [A_out, B_in])
                )
            ),
    %% Take the first (arbitrary) if it exists.
    case Result of
        [] -> {error, no_edge};
        _  -> lists:nth(1, Result)
    end.

%% @doc Return [{digraph:vertex(), Label}] for the graph G
%% @end
vertex_data(G) -> vtx_iter(G, digraph:vertices(G), []).

vtx_iter(_, [], Acc) -> Acc;
vtx_iter(G, [V|Vertices], Acc) ->
    vtx_iter(G, Vertices, [digraph:vertex(G, V)|Acc]).

%% @doc Applies Predicate to the label() in a vertex tuple
%% @end
label_filter(V, Predicate) ->
    lists:filter(fun({_, L}) -> Predicate(L) end, V).
