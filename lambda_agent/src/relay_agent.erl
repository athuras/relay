-module(relay_agent).
-behaviour(application).
-export([start/2,
         stop/1]).

-export([random_ingress/1,
        incoming/0,
        make_plans/3,
        make_predictions/3]).


start(_Type, [Name, PyPid|_]) ->
    {ok, SM, R} = state_manager:start_link(worker, []),

    {ok, PM, [PyPid2]} = case PyPid of
        undefined -> python_manager:start_link();
        _ -> python_manager:start_link(worker, PyPid)
    end,

    {ok, E} = echo:start_link(),  %% For realz
    ok = testing:load_queues(SM, 10),
    ok = testing:load_upstream(SM, 10),
    State_Name = erlang:binary_to_atom(Name, utf8),
    PyExt = <<"_py">>,
    PyName = erlang:binary_to_atom(<<Name/binary, PyExt/binary>>, utf8),
    register(State_Name, SM),
    register(PyName, PM),
    gen_event:notify(SM, {set_name, Name}),
    gen_event:notify(SM, {set_location, <<"My Butt">>}),

    %%  Do Stuff ... Periodically
    timer:apply_interval(4000, ?MODULE, random_ingress, [SM]),
    timer:apply_interval(5000, ?MODULE, make_predictions, [SM, PM, E]),
    timer:apply_interval(15000, ?MODULE, make_plans, [SM, PM, E]),

    {ok, SM, [PM, PyPid2, R]}.

stop(State) ->
    [_PM, PyState|_] = State,
    python_manager:stop(PyState),
    ok.

random_ingress(SM) ->
    [gen_event:notify(SM, incoming()) || _ <- lists:seq(1, 10)].

incoming() ->
    {incoming, random:uniform(4), state_manager:clock(), 1}.

make_plans(SM, PM, E) ->
    gen_event:call(PM, plan, {gen_plan, SM, E}).

make_predictions(SM, PM, E) ->
    gen_event:call(PM, predict, {gen_local_prediction, SM, E}).
