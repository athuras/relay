%%	Debug Module, Simply displays the events and messages received.
%%	You can attach/subscribe an echo process to an existing gen_event/netmed node.
%%
%%	Additionally, sending {notify, Pid, Message} to an echo process, will cause it
%%	to send Message to Pid. Which is helpful when debugging.
%%
%%  To use an echo process for forwarding (i.e. to spoof another process):
%%  >> {ok, A} = echo:start_link().
%%  // {ok, <0.43.0>}
%%  >> {ok, B} = echo:start_link().
%%  // {ok, <0.44.0>}
%%  >> gen_event:notify(A, {notify, B, "Hi there, love, B."}).
%%  // <0.43.0>, echo:handle_event "Hi there!"
%%  Voila!

-module(echo).
-behaviour(gen_event).

-export([
	start_link/0,
	init/1,
	handle_event/2,
	handle_call/2,
   	handle_info/2,
	code_change/3,
	terminate/2
	]).

start_link() ->
	{ok, Pid} = gen_event:start_link(),
	gen_event:add_handler(Pid, echo, []),
	{ok, Pid}.

init([]) ->
	{ok, []}.

handle_event({notify, Pid, Message}, State) ->
	gen_event:notify(Pid, Message),
	{ok, State};
handle_event({execute, F}, State) ->
	F(),
	{ok, State};
handle_event(Event, State) ->
	io:format("~p, echo:handle_event ", [self()]),
	erlang:display(Event),
	{ok, State}.

handle_call(Call, State) ->
	io:format("~p, echo:handle_call~n", [self()]),
	erlang:display(Call),
	{ok, Call, State}.

handle_info(Info, State) ->
	io:format("~p, Echo:handle_info~n", [self()]),
	erlang:display(Info),
	{ok, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_Reason, _State) -> ok.
