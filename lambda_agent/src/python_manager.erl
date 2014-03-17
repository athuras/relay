-module(python_manager).

%%  API
-export([
    gen_params/2,
    gen_plan/2,
    gen_prediction/2,
    kill_proc/1,
    py_start/0,
    python_module_dir/0,
    reload_handlers/1,
    start_link/2,
    stop/1,
    test_plan/1,
    test_predict/1
    ]).

%% API
start_link(Type, _Args) ->
    {ok, PyPid} = py_start(),
    {ok, Pid} = gen_event:start_link(),
    gen_event:add_handler(Pid, predict, PyPid),
    gen_event:add_handler(Pid, plan, PyPid),
    {ok, Pid, [PyPid]}.

%%  For Sanity
gen_prediction(Pid, StorePid) ->
    gen_event:call(Pid, predict, {gen_prediction, StorePid}).

gen_params(Pid, StorePid) ->
    gen_event:call(Pid, predict, {gen_params, StorePid}).

gen_plan(Pid, StorePid) ->
    gen_event:call(Pid, plan, {gen_plan, StorePid}).

test_plan(Pid) ->
    gen_event:call(Pid, plan, test).

test_predict(Pid) ->
    gen_event:call(Pid, predict, test).

kill_proc(Pid) ->
    gen_event:notify(Pid, kill_python).

reload_handlers(Pid) ->
    Handlers = [predict, plan],
    lists:foreach(fun(X) -> gen_event:delete_handler(Pid, X, []) end,
                  Handlers),
    {ok, PyPid} = py_start(),
    lists:foreach(fun(X) -> gen_event:add_handler(Pid, X, PyPid) end,
                  Handlers),
    {ok, Pid}.

stop(State) ->
    [Pid] = State,
    python:stop(Pid).

py_start() ->
    python:start([{python_path, python_module_dir()}]).

python_module_dir() ->
    "/Users/ath/Documents/Dev/relay/lambda_agent/pyCrunch".
