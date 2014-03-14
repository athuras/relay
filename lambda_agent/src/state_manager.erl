%%  Maintains and updates the Agent's internal state
%%  queue_accumulator:
%%      Ingress and Egress queues
%%  relay_store:
%%      All other state, including planning and BTG stuff.

-module(state_manager).

%%  API
-export([
    start_link/0
    ]).

%%  API
start_link() ->
    {ok, Pid} = gen_event:start_link(),
    gen_event:add_handler(Pid, queue_accumulator, agent_graphs:default_behaviour()),
    gen_event:add_handler(Pid, relay_store, []),
    %gen_event:add_handler(Pid, prediction_engine, []),
    %gen_event:add_handler(Pid, btg_planner, []),
    {ok, Pid}.
