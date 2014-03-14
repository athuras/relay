%% Template graph functions for setting up BTG, Control, and symmetric graphs.

-module(graph_builder).
-export([
    rotate_behaviour/1,
    base_4/0,
    b4_1/0,
    b4_2/0,
    b4_3/0,
    b4_4/0,
    new_btg/0
    ]).

%% @doc Generates the default symmetric 4-way behaviour transition graph
%%      yes, this is hardcoded.
%%      Edges are labelled with the 'delay' associated with the transistion
%%      such as Red, and Yellow time.
%%      Nodes are behaviour graphs.
%% @end
new_btg() ->
    Red_time = 3, % Half the time taken by a red light
    Yellow_time = 2,
    Adv_green_delay = 1.5,
    B1_1 = b4_1(),
    B1_2 = rotate_behaviour(B1_1),
    B2_1 = b4_2(),
    B2_2 = rotate_behaviour(B2_1),
    B3_1 = b4_3(),
    [B3_2, B3_3, B3_4] = lists:map(
            fun(X) -> rotate_behaviour(B3_1, X) end,
            [1, 2, 3]),
    Red = b4_4(),
    %% And now to build the graph ...
    EdgeList = [
            {B1_1, Red},
            {B1_2, Red},
            {B2_1, B1_1},{B2_1, Red},
            {B2_2, Red}, {B2_2, B1_2},
            {B3_1, Red}, {B3_1, B1_1},
            {B3_2, Red}, {B3_2, B1_2},
            {B3_3, Red}, {B3_3, B1_1},{B3_3, B2_2},
            {B3_4, Red}, {B3_4, B2_1},{B3_4, B1_2},
            {Red, B1_1}, {Red, B1_2}, {Red, B2_1}, {Red, B2_2},
            {Red, B3_1}, {Red, B3_2}, {Red, B3_3}, {Red, B3_4}
        ],
    Edge_list_delays = lists:map(
            fun({A, B}) ->
                Delay = if B == Red -> Yellow_time
                        ;  A == Red -> Red_time
                        ;  true -> Adv_green_delay
                        end,
                {A, B, Delay}
            end,
            EdgeList
        ),
    graph_from_edge_list(Edge_list_delays).


%% Generates a graph given a set of {V1, V2, Label} tuples, inserting appropriate
%% vertices as it goes.
-spec graph_from_edge_list(list()) -> digraph()
                                    | {error, digraph:add_edge_err_rsn()}.
graph_from_edge_list(Edges) ->
    G = digraph:new(),
    lists:foreach(fun({A, B, L}) -> graph_utils:add_edge(G, A, B, L) end, Edges),
    G.

%% Generates a 'symmetrically rotated' graph using an existing graph as a template.
%% Requires a 'normalized' graph (where nodes are zero-indexed integers).
-spec rotate_behaviour(digraph(), integer()) -> digraph()
                                                      | {error, digraph:add_edge_err_rsn()}.
rotate_behaviour(G) -> rotate_behaviour(G, 1).
rotate_behaviour(G, K) ->
    Max = lists:max(digraph:vertices(G)) + 1,
    rotate_behaviour(G, K, Max).

rotate_behaviour(G, K, N) ->
    Original_Edges = digraph:edges(G),
    Rotated_Edges = lists:map(
            fun(Edge_id) ->
                    {_, A, B, Label} = digraph:edge(G, Edge_id),
                    sym_increment_edge({A, B, Label}, K, N)
            end,
            Original_Edges),
    graph_from_edge_list(Rotated_Edges).

sym_increment_edge({A, B, Label}, K, N) ->
    {(A + K) rem N, (B + K) rem N, Label}.

%% Behold! Hardcoded graphs!
%% Tuples are {Entering_node, Exit_node, Coefficient}.

% Describes an entirely transitive 4-node graph.
% 0 -> North Entrance, 1 -> East, 2 -> South, 3 -> West
base_4() ->
    graph_from_edge_list(base_4_edgelist).

% Behavior I. "North-South Green"
% Two rotations for coverage.
b4_1() ->
    graph_from_edge_list(b4_1_edgelist).

% Behaviour II. "North-South Adv. Green"
% Two rotations for coverage
b4_2() ->
    graph_from_edge_list(b4_2_edgelist).

%% Behaviour III. "North Adv. Green, Through"
%% 4 rotations for coverage
b4_3() ->
    graph_from_edge_list(b4_3_edgelist).

%% Behaviour IV. "Red", decrease RHT by 1/2
%% 1 Rotation only
b4_4() ->
    graph_from_edge_list(b4_4_edgelist).

%% Edge Lists
base_4_edgelist() ->
    [{0, 1, 0.1}, {0, 2, 0.7}, {0, 3, 0.2},
     {1, 0, 0.2}, {1, 2, 0.1}, {1, 3, 0.7},
     {2, 0, 0.7}, {2, 1, 0.2}, {2, 3, 0.1},
     {3, 0, 0.1}, {3, 1, 0.7}, {3, 2, 0.2}].

b4_1_edgelist() ->
    [{0, 2, 1.0}, {0, 3, 1.0},
     {1, 0, 1.0}, {2, 0, 1.0},
     {2, 1, 1.0}, {3, 2, 1.0}].

b4_2_edgelist() ->
    [{0, 1, 1.0}, {0, 3, 1.0},
     {1, 0, 1.0}, {2, 1, 1.0},
     {2, 3, 1.0}, {3, 2, 1.0}].

b4_3_edgelist() ->
    [{0, 1, 1.0}, {0, 2, 1.0},
     {0, 3, 1.0}, {1, 0, 1.0},
     {2, 1, 1.0}, {3, 2, 1.0}].

b4_4_edgelist() ->
    [{0, 3, 0.5}, {1, 0, 0.5},
     {2, 1, 0.5}, {3, 2, 0.5}].
