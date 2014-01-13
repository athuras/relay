-module(graph_builder).
-export([
    from_file/1,
    graph_from_edge_list/1
    ]).

%% @doc Generates a digraph from the edge-list contained in Path.
%% Each line in Path should read as "int() int()[ float()]".
%% refer to template_graphs/4_intersection.txt for an example
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

graph_from_edge_list(Edges) ->
    G = digraph:new(),
    lists:foreach(fun({A, B, L}) -> graph_utils:add_edge(G, A, B, L) end, Edges),
    G.
