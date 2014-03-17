%%  Access Point for all Agent-specific data
%%  Includes
%%      * Graphs
%%      * Neighbourhood
%%      * Meta

-module(relay_store).
-behaviour(gen_event).
-export([
    init/1,
    handle_event/2,
    handle_call/2,
    handle_info/2,
    code_change/3,
    terminate/2
    ]).

-include("relay.hrl").

%%  State that isn't changed 'asynchronously', in that it isn't changed very fast.
%%  The accumulator handler manages the 'real-time' aspects of the Agent's state.
-record(state, {name, location,
                btg_table=new_ets_table(agent_graphs:new_btg()),
                b_table=new_ets_table(agent_graphs:new_behaviour_table()),
                prediction_time=[0],
                prediction=[],
                base_graph=agent_graphs:new_base_graph(),
                current_behaviour=0,
                current_plan={[0],[0]},
                delay_params=[]
                }).

init([]) ->
    {ok, #state{}}.

%%  Maintenance Related
handle_event({set_name, Name}, State) ->
    {ok, State#state{name=Name}};
handle_event({set_location, location}, State) ->
    {ok, State#state{location=location}};

%%  Planning Related
handle_event({new_plan, Plan_Stuff}, State) ->
    {ok, State#state{current_plan=Plan_Stuff}};
handle_event({set_behaviour, Bid}, State) ->
    {ok, State#state{current_behaviour=Bid}};


%%  Statistics and Inferred Properties
handle_event({update_delay_params, Taus}, State) ->
    {ok, State#state{delay_params=Taus}};

handle_event({update_graph_params, Base}, State) ->
    {ok, State#state{base_graph=Base}};

handle_event({new_prediction, {Time, P}}, State) ->
    {ok, State#state{prediction=P, prediction_time=[Time]}};

handle_event(_, State) ->
    {ok, State}.

%%  Calls...
handle_call(get_name, State) ->
    {ok, State#state.name, State};

handle_call(get_location, State) ->
    {ok, State#state.location, State};

handle_call(get_current_state, State) ->
    {ok, State#state.current_behaviour, State};

handle_call(get_current_plan, State) ->
    {ok, State#state.current_plan, State};

handle_call(get_tables, State) ->
    Tables = {ok, {State#state.btg_table, State#state.b_table}},
    {ok, Tables, State};

handle_call(get_delay_params, State=#state{delay_params=DP}) ->
    Taus = case DP of
        [] -> [lol_matrices:ones(4)
               || _ <- lists:seq(1, length(State#state.base_graph))
                 ];
        _ -> DP
    end,
    {ok, Taus, State#state{delay_params=Taus}};

handle_call(get_current_graph, State) ->
    {ok, get_current_graph(State), State};

handle_call(get_prediction, State) ->
    {ok, {State#state.prediction_time,
          State#state.prediction},
     State};

%%  Web-Server Call, Serialize Everything...
handle_call(request_state, State) ->
    %% First look up the behaviour
    {ok, state_to_visible_state(State), State};

handle_call(_Call, State) ->
    {ok, ok, State}.


%% Misc ...
handle_info(_, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    %  May want to bequest the various data structures to another process...
    ok.


%%  Private
get_behaviour(Tid, Bid) ->
    [{_, B}|_] = ets:lookup(Tid, Bid),
    B.
get_current_graph(State=#state{b_table=Tid, base_graph=G}) ->
    Bid = State#state.current_behaviour,
    lol_matrices:mult(get_behaviour(Tid, Bid), G).

%%  There has to be a better way, but I don't know it.
state_to_visible_state(State=#state{b_table=Tid, base_graph=G}) ->
    {Behaviours, Times} = State#state.current_plan,
    Rendered_Plan = [lol_matrices:mult(get_behaviour(Tid, B), G)
                     || B <- Behaviours],
    #visible_state{name=State#state.name,
                   location=State#state.name,
                   behaviour=get_current_graph(State),
                   prediction_time=State#state.prediction_time,
                   prediction=State#state.prediction,
                   current_plan=Rendered_Plan,
                   current_timing=Times,
                   delay_params=State#state.delay_params}.

new_ets_table(Data) ->
    Tid = ets:new(unnamed, [set]),
    ets:insert(Tid, Data),
    Tid.
