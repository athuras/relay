%% A bunch of functions that are usefull for building graphs (specifically fromt ext files).

-module(graph_builder).
-export([
    from_file/1,
    graph_from_edge_list/1,
    rotate_behaviour/1
    ]).

%% @doc Generates a digraph from the edge-list contained in Path.
%% Each line in Path should read as "int() int()[ float()]".
%% refer to template_graphs/4_intersection.txt for an example
%% @spec from_file(Path) -> digraph() | {error, Reason}
%% @end
from_file(Path) ->
    {ok, Bin} = file:read_file(Path),
    Edges = parse_edges_from_data(Bin),
    graph_from_edge_list(Edges).

parse_edges_from_data(Bin) when is_binary(Bin) ->
    parse_edges_from_data(binary_to_list(Bin));
parse_edges_from_data(Str) ->
    Edge_data = string:tokens(Str, "\n"),
    lists:map(fun(X) -> make_edge(string:tokens(X, " ")) end, Edge_data).

make_edge(L) when is_list(L) ->
    case length(L) of
        2 -> {A, B} = list_to_tuple(L),
            {list_to_integer(A), list_to_integer(B), []};
        3 -> {A, B, P} = list_to_tuple(L),
            {list_to_integer(A), list_to_integer(B), list_to_float(P)}
    end.

%% @doc
%% Generates a graph given a set of {V1, V2, Label} tuples, inserting appropriate
%% vertices as it goes.
%% @spec graph_from_edge_list(Edge_List) -> digraph() | {error, Reason}
%% @end
graph_from_edge_list(Edges) ->
    G = digraph:new(),
    lists:foreach(fun({A, B, L}) -> graph_utils:add_edge(G, A, B, L) end, Edges),
    G.

%% @doc
%% Generates a 'symmetrically rotated' graph using an existing graph as a template.
%% Requires a 'normalized' graph (where nodes are zero-indexed integers).
%% @spec rotate_behaviour(G) -> digraph:digraph() | {error, Reason}
%% @end
rotate_behaviour(G) ->
    Max = lists:max(digraph:vertices(G)) + 1,
    rotate_behaviour(G, Max).

rotate_behaviour(G, N) ->
    Original_Edges = digraph:edges(G),
    Rotated_Edges = lists:map(
            fun(Edge_id) ->
                    {_, A, B, Label} = digraph:edge(G, Edge_id),
                    sym_increment_edge({A, B, Label}, 1, N)
            end,
            Original_Edges),
    graph_from_edge_list(Rotated_Edges).

sym_increment_edge({A, B, Label}, K, N) ->
    {(A + K) rem N, (B + K) rem N, Label}.
