-module(connection_handler).

-behaviour(gen_event).

-export([
    init/1,
    handle_event/2,
    handle_call/2,
    handle_info/2,
    code_change/3,
    terminate/2
    ]).

%%  subs:  dict:{Local_Out => Remote_pid}
%%  obs:   dict:{Remote_pid => Local_in}
-record(state, {subs=orddict:new(),
                obs=orddict:new(), local_SM, local_PM}).

init({SM, PM}) ->
    {ok, #state{local_SM=SM, local_PM=PM}}.


%%  If a process subscribes to an outlet, they are added to our obligations
%%  After which all outgoing messages from that outlet are forwarded their way
handle_event({subscribe, Remote, Local_Out}, State=#state{obs=O}) ->
    {ok, State#state{obs=orddict:store(Local_Out, Remote, O)}};

handle_event({unsubscribe, Remote, Local_Out}, State=#state{obs=O}) ->
    {ok, State#state{obs=orddict:erase(Local_Out, O)}};

%%  'subscribe' to a remote processes feed, route messages from that feed to
%%  the inlet.
handle_event({connect, Remote, R_Out, Local_In}, State=#state{subs=S}) ->
    gen_event:notify(Remote, {subscribe, self(), R_Out}),
    {ok, State#state{subs=orddict:store(Remote, Local_In, S)}};

handle_event({disconnect, Remote, R_Out, Local_In}, State=#state{subs=S}) ->
    gen_event:notify(Remote, {unsubscribe, self(), R_Out}),
    {ok, State#state{subs=orddict:erase(Remote, S)}};

%%  Forward the outbound event to all remotes
handle_event({outbound, Index, Time, Weight}, State=#state{obs=O}) ->
    Message = {incoming_upstream, self(), Time, Weight},
    try
        Remote = orddict:fetch(Index, O),
        gen_event:notify(Remote, Message),
        {ok, State}
    catch
        error:function_clause -> {ok, State}
    end;

%%  Forward the incoming events to the local state manager, setting the index.
handle_event({incoming_upstream, Remote, Time, Weight}, State=#state{subs=S}) ->
    try
        Inlet = orddict:fetch(Remote, S),
        Message = {incoming_upstream, Inlet, Time, Weight},
        gen_event:notify(State#state.local_SM, Message),
        {ok, State}
    catch
        error:function_clause -> {ok, State}
    end;

handle_event(_E, State) ->
    {ok, State}.

handle_call(_Call, State) ->
    {ok, ok, State}.

handle_info(_, State) ->
    {ok, State}.

code_change(_Old, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State=#state{subs=S, obs=O}) ->
    %%  Inform subscribers that we're terminating
    lists:foreach(
        fun({X, Y}) ->
                Message = {disconnect, self(), Y},
                gen_event:notify(Message, X)
        end,
        O
        ),
    ok.
