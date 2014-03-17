-module(relay_agent).
-behaviour(application).
-export([start/2,
         stop/1]).

-export([random_ingress/1,
        incoming/0]).


start(_Type, [Name|_Args]) ->
    {ok, SM} = state_manager:start_link(worker, []),
    {ok, PM, PyState} = python_manager:start_link(worker, []),
    {ok, E} = echo:start_link(),
    ok = testing:load_queues(SM, 10),
    ok = testing:load_upstream(SM, 10),
    State_Name = erlang:binary_to_atom(Name, utf8),
    PyExt = <<"_py">>,
    PyName = erlang:binary_to_atom(<<Name/binary, PyExt/binary>>, utf8),
    register(State_Name, SM),
    register(PyName, PM),
    gen_event:notify(SM, {set_name, Name}),
    gen_event:notify(SM, {set_location, <<"My Butt">>}),
    gen_event:call(PM, predict, {gen_local_prediction, SM, E}),
    gen_event:call(PM, plan, {gen_plan, SM, E}),
    timer:apply_interval(2000, ?MODULE, random_ingress, [SM]),
    {ok, SM, [PM, PyState, E]}.

stop(State) ->
    [_PM, PyState|_] = State,
    python_manager:stop(PyState),
    ok.

random_ingress(SM) ->
    gen_event:notify(SM, incoming()).

incoming() ->
    {incoming, random:uniform(4), state_manager:clock(), 1}.
