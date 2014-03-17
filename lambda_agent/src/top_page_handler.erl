-module(top_page_handler).

-include("jsonerl.hrl").

-export([init/3,
         handle/2,
         terminate/3]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {Agent, Req3} = cowboy_req:qs_val(<<"agent">>, Req2),
    Data = get_state(Agent),
    {ok, Req4} = reply(Method, Data, Req3),
    {ok, Req4, State}.

reply(<<"GET">>, undefined, Req) ->
    cowboy_req:reply(400, [], <<"undefined">>, Req);

reply(<<"GET">>, bad_agent, Req) ->
    cowboy_req:reply(400, [], <<"missing agent parameter">>, Req);

reply(<<"GET">>, Response, Req) ->
    cowboy_req:reply(200, [
            {<<"content-type">>, <<"test/plain; charset=utf-8">>}
            ], Response, Req);

reply(_, _, Req) ->
        %% Method not allowed
        cowboy_req:reply(405, Req).

terminate(_Reason, _Req, _State) ->
    ok.

get_state(Agent) ->

    case lists:member(A=erlang:binary_to_atom(Agent, utf8),
                      registered()) of
        true -> gen_event:call(A, relay_store, request_state);
        false -> bad_agent
    end.
