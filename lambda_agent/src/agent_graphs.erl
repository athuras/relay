-module(agent_graphs).
-export([new_behaviour_table/0,
         new_base_graph/0,
         new_btg/0]).

%%  Hardcoded, cause thats how we roll...
%%  Also simplified ... matrix-as-list-of-lists (LOL Matrices).
%%  [{behaviour_id -> behaviour_mask}].
new_behaviour_table() ->
    C = circular(),
    B = lists:map(fun(X) -> lol_matrices:add(X, C) end,
                  [nst(), ewt(), ns_adv(), ew_adv(), red()]),
    lists:zip(lists:seq(0, 4), B).

%%  4-inlets, 4-outlets
new_base_graph() ->
    [[0.0, 0.1, 0.7, 0.2],
     [0.2, 0.0, 0.1, 0.7],
     [0.7, 0.2, 0.0, 0.1],
     [0.1, 0.7, 0.2, 0.0]].

%%  The numbering is based on the list in new_behaviour_table!!! Don't change it!
new_btg() ->
    Red_Time = 2,
    Adv_green_delay = 1,
    [{{0, 4}, Red_Time}, {{1, 4}, Red_Time},
    {{2, 4}, Red_Time}, {{3, 4}, Red_Time},
    {{4, 0}, Red_Time}, {{4, 1}, Red_Time},
    {{4, 2}, Red_Time}, {{4, 3}, Red_Time},
    {{2, 0}, Adv_green_delay}, {{3, 1}, Adv_green_delay}].

circular() ->
    [[0, 0, 0, 1],
     [1, 0, 0, 0],
     [0, 1, 0, 0],
     [0, 0, 1, 0]].

nst() ->
    [[0, 0, 1, 0],
     [0, 0, 0, 0],
     [1, 0, 0, 0],
     [0, 0, 0, 0]].

ewt() ->
    [[0, 0, 0, 0],
     [0, 0, 0, 1],
     [0, 0, 0, 0],
     [0, 1, 0, 0]].

ns_adv() ->
    [[0, 0, 0, 0],
     [1, 0, 0, 0],
     [0, 0, 0, 1],
     [0, 0, 0, 0]].

ew_adv() ->
    [[0, 0, 0, 0],
     [0, 0, 1, 0],
     [0, 0, 0, 0],
     [1, 0, 0, 0]].

red() ->
    [[0, 0, 0, 0],
     [0, 0, 0, 0],
     [0, 0, 0, 0],
     [0, 0, 0, 0]].

%% Ignore one-directional behaviours for now...
%% TODO: This.
