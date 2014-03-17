-module(spider).  %% 'cause it spins webs, with traffic ... yeah.

-export([connect/2,
         connect/4,
         spin/1,
         spin/5]).

%%  Abuse connection_handler.erl to spin a web.
connect(R1, R2) ->
    connect(R1, R2, 2, 4).
connect(R1, R2, I, J) ->
    gen_event:notify(R1, {subscribe, R2, I}),
    gen_event:notify(R2, {subscribe, R1, J}),
    ok.

spin([A, B, C, D, E|_]) ->
    spin(A, B, C, D, E).
spin(A, B, C, D, E) ->
    connect(A, B, 2, 4),
    connect(B, C, 2, 4),
    connect(A, D, 3, 4),
    connect(D, E, 2, 4),
    connect(B, D, 3, 1),
    connect(C, E, 2, 2),
    connect(C, E, 3, 1).
