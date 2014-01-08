-module(relay_utils).
-export([
	echos/1,
	test_gossip/1,
	test_network/1
	]).

echos(N) ->
	lists:map(fun(_) -> {ok, P} = echo:start_link(), P end, lists:seq(1, N)).

%%	@doc a test gossip message that invokes "BOOYAKASHA"
test_gossip(Echo) ->
	Wrapper = fun(Data, State) -> test_fun(Data, State, Echo) end,
	{gossip, [], Wrapper, sets:new()}.

%%	@doc Sets up a ring of netmed processes, the last one is an echo.
test_network(N) ->
	Nodes = lists:map(
			fun(_) -> {ok, P} = netmed:start_link(), P end,
			lists:seq(1, N - 1)),
	lists:foldl(fun setup/2, [], Nodes).

setup(A, []) ->
	{ok, E} = echo:start_link(),
	downstream_sub(A, E),
	[A,E];
setup(A, [B|Tail]) ->
	downstream_sub(A, B),
	[A,B|Tail].

downstream_sub(A, B) ->
	netmed:subscribe(A, B).

test_fun(Data, State, Pid) ->
	NewData = [self()|Data],
	ok = gen_event:notify(Pid, {gossip_fun, NewData}),
	{ok, State}.
