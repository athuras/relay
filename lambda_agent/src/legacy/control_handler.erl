%% The Entry Point for the Intersection Control Algorithms.
%% This module is logcally simple, but relies on (as yet unimplemented)
%% discrete optimization routines.
%%
%%
%% Given an "annotated behaviour transition graph" (ABTG see control_graph.erl)
%% and a prediction about traffic incident on the intersection, the controller
%% seeks to change its internal state (navigate the ABTG) to maximally serve the
%% incoming traffic.
-module(control_handler).

-behaviour(gen_event).

%% API
-export([start_link/0,
         add_handler/2]).

%% gen_event callbacks
-export([
    init/1,
    handle_event/2,
    handle_call/2,
    handle_info/2,
    code_change/3,
    terminate/2
    ]).

-record(state, {current_behaviour, btg, base}).

%% API Functions --------------------------------------------------------------
start_link() ->
    gen_eventL:start_link({local, ?MODULE}).


add_handler(Handler, Args) ->
    gen_event:add_handler(?MODULE, Handler, Args).

%% Interface Functions --------------------------------------------------------

init([]) ->
    BTG = control_graph:new(),
    Base = graph_builder:base_4(),
    {_, Current} = lists:max(
            lists:map(fun(X) -> {erlang:length(digraph:edges(X)), X} end,
                  digraph:vertices(BTG)
                )
            ),
    {ok, #state{current_behaviour=Current, btg=BTG, base=Base}}.


handle_event(_Event, State) ->
    {ok, State}.


handle_call(_, State) ->
    {ok, ok, State}.


handle_info(_, State) ->
    {ok, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


terminate(_Reason, _State) ->
    ok.

%% Private Functions ----------------------------------------------------------
