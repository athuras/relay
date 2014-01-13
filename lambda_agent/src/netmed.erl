-module(netmed).
-export([
	start_link/0,
	subscribe/2,
   	unsubscribe/2,
	alert/2,
	gossip/3,
	gossip/4,
	get_state/1,
	get_graph/1
	]).

start_link() ->
	{ok, Pid} = gen_event:start_link(),
	gen_event:add_handler(Pid, netmed_handler, []),
	{ok, Pid}.

%%	@doc Add the Pid to the agent's internal graph
%%	@end
subscribe(Pid, X) ->
	gen_event:notify(Pid, {subscribe, X}).

%%	@doc Remove the Pid from the agent's internal graph
%%	@end
unsubscribe(Pid, X) ->
	gen_event:notify(Pid, {unsubscribe, X}).

%%	@doc Alert agent of incoming vehicles
%%	@end
alert(Pid, Data) ->
	gen_event:notify(Pid, {alert, erlang:now(), Data}).

%%	@doc Propagate Information (or a Relay Operator) to the agent.
%%	@end
gossip(Pid, Data, Func) -> gossip(Pid, Data, Func, sets:new()).
gossip(Pid, Data, Func, Marked) ->
	gen_event:notify(Pid, {gossip, Data, Func, Marked}).

%%	@doc Get the current state
%%	@end
get_state(Pid) ->
	gen_event:call(Pid, netmed_handler, get_state).

%% @doc Get the Graph
%% @end
get_graph(Pid) ->
	gen_event:call(Pid, netmed_handler, get_graph).
