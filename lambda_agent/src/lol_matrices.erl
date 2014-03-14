%% List-of-List (LOL) matrix operations.
%% For when you really need a LOL-Matrix.
-module(lol_matrices).
-export([add/1,
         add/2,
         mult/1,
         mult/2,
         vec_dot_vec/2,
         vec_dot_mat/2,
         mat_dot_mat/2,
         combine/3,
         transpose/1]).

%%  Currently relies on shapes being identical.
%%  Minor broadcasting!!

add({A, B}) -> add(A, B).
add(A, B) when is_number(A) and is_number(B) ->
    A + B;
add(A, B) when is_list(A) and is_number(B) ->
    add(B, A);
add(A, B) when is_number(A) and is_list(B) ->
    lists:map(fun(X) -> add(A, X) end, B);
add(A, B) when is_list(A) and is_list(B) ->
    combine(A, B, fun add/2).

mult({A, B}) -> mult(A, B).
mult(A, B) when is_number(A) and is_number(B) ->
    A * B;
mult(A, B) when is_number(A) and is_list(B) ->
    mult(B, A);
mult(A, B) when is_list(A) and is_number(B) ->
    lists:map(fun(X) -> mult(B, X) end, A);
mult(A, B) when is_list(A) and is_list(B) ->
    combine(A, B, fun mult/2).

vec_dot_vec(A, B) when is_list(A) and is_list(B) ->
    lists:sum(mult(A, B)).
vec_dot_mat(A, B) when is_list(A) and is_list(B) ->
    Bt = transpose(B),
    lists:map(fun(X) -> lists:foldl(fun add/2, 0, X) end,
              lists:map(fun(X) -> mult(A, X) end,
                        Bt)
             ).

mat_dot_mat(A, B) when is_list(A) and is_list(B) ->
    lists:map(fun(X) -> vec_dot_mat(X, B) end, A).

combine(A, B, F) when is_function(F) ->
    lists:map(fun({E1, E2}) -> F(E1, E2) end,
              lists:zip(A, B)).

transpose([]) -> [];
transpose(A) when is_list(A) ->
    transpose(A, []).
transpose(A, Trans) ->
    IsEmpty = lists:any(fun(X) -> X =:= [] end, A),
    case IsEmpty of
        false ->
            K = lists:map(fun([H|_]) -> H end, A),
            Z = lists:map(fun([_|T]) -> T end, A),
            transpose(Z, [K|Trans]);
        true ->
            lists:reverse(Trans)
    end.
