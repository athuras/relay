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

-record(state, {name, location,
                btg_table=new_ets_table(agent_graphs:new_btg()),
                b_table=new_ets_table(agent_graphs:new_behaviour_table()),
                base_graph=agent_graphs:new_base_graph(),
                current_behaviour=0, current_plan=[],
                delay_kernels=[]}).

init([]) ->
    {ok, #state{}}.

%%  Maintenance Related
handle_event({set_name, Name}, State) ->
    {ok, State#state{name=Name}};
handle_event({set_location, location}, State) ->
    {ok, State#state{location=location}};

%%  Planning Related
handle_event({set_behaviour, Bid}, State) ->
    {ok, State#state{current_behaviour=Bid}};


%%  Statistics and Inferred Properties
handle_event({update_delay_kernels, Taus}, State) ->
    {ok, State#state{delay_kernels=Taus}};
handle_event({update_graph_params, Base}, State) ->
    {ok, State#state{base_graph=Base}};

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
    Tables = [{btg_table, State#state.btg_table},
              {b_table, State#state.b_table}],
    {ok, Tables, State};

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

new_ets_table(Data) ->
    Tid = ets:new(unnamed, [set]),
    ets:insert(Tid, Data),
    Tid.
