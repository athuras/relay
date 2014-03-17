%%  Maintains and updates the Agent's internal state
%%  queue_accumulator:
%%      Ingress and Egress queues
%%  relay_store:
%%      All other state, including planning and BTG stuff.

-module(state_manager).

%%  API
-export([
    start_link/2,
    start_link/0
    ]).

-export([clock/0]).

%%  Interface
-export([
    get_tables/1,
    get_queue/3,
    get_name/1,
    refresh_store/1
    ]).

clock() -> element(1, now()) * 10000 + element(2, now()).


%%  API
start_link() -> start_link(none, none).
start_link(_Type, _Args) ->
    {ok, Pid} = gen_event:start_link(),
    {ok, R} = connection_manager:start_link(),
    gen_event:add_sup_handler(Pid, accumulator, {agent_graphs:default_behaviour(), R}),
    gen_event:add_sup_handler(Pid, relay_store, []),
    {ok, Pid, R}.

%%  Interface
get_tables(Pid) ->
    gen_event:call(Pid, relay_store, get_tables).

get_queue(Pid, I, ingress) ->
    gen_event:call(Pid, accumulator, {get_ingress, I});
get_queue(Pid, I, egress) ->
    gen_event:call(Pid, accumulator, {get_egress, I}).

get_name(Pid) ->
    gen_event:call(Pid, relay_store, get_name).

refresh_store(Pid) ->
    gen_event:delete_handler(Pid, relay_store, []),
    gen_event:add_sup_handler(Pid, relay_store, []).
