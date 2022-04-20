:- module(auxiliar_extras, [
    list_add/3,
    list_remove/3,
    list_contains/2,
    list_empty/1,
    list_length/2,
    list_concat/3,
    list_reverse/2,
    is_equal/2,
    true_/0,
    potencia/3,
    list_union/3
    ]).

% Util predicates used in the project.

list_add(X, [], [X]).
list_add(X, [Y|Z], [Y|R]):-list_add(X, Z, R).
list_remove(X, [X|L1], L1).
list_remove(X, [Y|L2], [Y|L1]) :- list_remove(X, L2, L1).
list_contains(X,[X|_]).
list_contains(X,[_|R]):-list_contains(X,R).
list_empty([]).
list_length([], 0).
list_length([_|TAIL],N) :- list_length(TAIL, N1), N is N1 + 1.

list_concat([], L, L).
list_concat([X1|L1], L2, [X1|L3]) :- list_concat(L1, L2, L3).

list_union([],[], []).
list_union(L1, [], L1).
list_union(L1, [X|L2], [X|L3]):-
    \+ list_contains(X, L1), list_union(L1, L2, L3).
list_union(L1, [X|L2], L3):-
    list_contains(X, L1), list_union(L1, L2, L3).
    

list_reverse([], []).
list_reverse([X|T], R) :- list_reverse(T, Z),
			  list_add(X, Z, R).
is_equal(X, X).
true_().

potencia(_, 0, 1).
potencia(X, 1, X).
potencia(X, N, P) :- 
    N > 0, N1 is N - 1,
    potencia(X, N1, R), 
    P is R*X.


