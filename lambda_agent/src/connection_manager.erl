%%  Designed to manage the network communication between relay agents.
%%  Maintains a map of 'agent/pid -> inlet'
%%  Interactions between connection manager and other intra-agent processes are
%%  mostly one-way.
-module(connection_manager).

-export([
    start_link/2,
    start_link/0
    ]).

start_link() ->
    start_link(none, none).
start_link(_Type, _Args) ->
    {ok, P} = gen_event:start_link(),
    {ok, E} = echo:start_link(),
    gen_event:add_sup_handler(P, connection_handler, {E, E}),
    {ok, P}.
