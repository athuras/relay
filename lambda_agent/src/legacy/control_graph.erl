%% The Behaviour Transition Graph, and associated functions.

-module(control_graph).
-export([
    new/0,
    subgraph_multiply/2
    ]).

%% @doc Generate a new symmetric 4-way behaviour transition graph.
%% @spec new() -> digraph:graph() | {error, Reason}
%% @end
new() ->
    graph_builder:new_btg().

%% @doc Takes two graphs, and returns the edgewise multiplication.
%%      Because I'm lazy, Small MUST be a proper subset of Big.
%% @spec subgraph_multiply(Big, Small) -> digraph:graph() | {error, Reason}
%% @end
subgraph_multiply(Big, Small) ->
    graph_utils:combine(Big, Small,
                        fun(_, Y) -> Y end,
                        fun(X, Y) -> X * Y end).
