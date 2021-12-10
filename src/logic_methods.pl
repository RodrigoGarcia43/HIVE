:- module(logic_methods, [
    try_add_free_position/6,
    try_jump_in_direction/7,
    get_occupied_positions/3,
    free_hexagons/1,
    check_conex/3,
    dfs/7,
    find_updates/3,
    get_bug_from_position/3,
    check_stack/2,
    get_stack/2
    ]).

try_add_free_position([X,Y], Free, Occupied, NewFree, [XObs1, YObs1], [XObs2, YObs2]):-
    (\+ list_contains([X,Y], Occupied),
    \+ list_contains([X,Y], Free),
    (\+ list_contains([XObs1, YObs1], Occupied); \+ list_contains([XObs2, YObs2], Occupied)),
    
    list_add([X,Y], Free, NewFree));
    NewFree = Free.

try_jump_in_direction([X,Y], Free, Occupied, NewFree, DirX, DirY, FirstMove):-
    (list_contains([X,Y], Occupied), 
    NewX is X + DirX, NewY is Y + DirY,
    try_jump_in_direction([NewX, NewY], Free, Occupied, NewFree, DirX, DirY, 0));

    (
        (FirstMove \= 1,
        \+ list_contains([X,Y], Free),
        list_add([X,Y], Free, NewFree));
        NewFree = Free
    ).


get_occupied_positions([], Occupied, Occupied).
get_occupied_positions([[Bug,_]|T], Occupied, UpdatedOccupied):-
    get(Bug, position, Position),
    get(Position, x, X_), X is X_ -20,
    get(Position, y, Y_), Y is Y_ - 20,

    list_add([X,Y], Occupied, NewOccupied),

    get_occupied_positions(T, NewOccupied, UpdatedOccupied).

free_hexagons([]).
free_hexagons([X|T]):-
    free(X), free_hexagons(T).


check_conex([], _ , []).
check_conex([[X,Y]|T], Graph, UpdatedPositions):-
    
    check_conex(T, Graph, Positions0),
    (   
    (list_contains([X,Y], Graph), Graph0 = Graph);
        list_add([X,Y], Graph, Graph0)
    ),
    dfs(X, Y, Graph0, [], 0, _, Count),
    (
        (        
        list_length(Graph0, N), 
        Count=:=N,
        
        list_add([X,Y], Positions0, UpdatedPositions)
        );
        is_equal(Positions0, UpdatedPositions)
    ).


dfs(X,Y, Graph, Walked, Count, OutWalked, OutCount):-
    (
        (\+ list_contains([X,Y], Graph); list_contains([X,Y], Walked)),
        OutWalked = Walked, OutCount = Count
    );

    Count0 is Count + 1,
    
    list_add([X,Y], Walked, Walked0),

    X0 is X + 70, Y0 is Y + 49, %dawn right
    X1 is X + 70, Y1 is Y - 49, %up right
    X2 is X , Y2 is Y - 98, %up
    X3 is X - 70, Y3 is Y - 49, %up left
    X4 is X - 70, Y4 is Y + 49, %dawn left
    X5 is X , Y5 is Y + 98, %dawn

    dfs(X0,Y0, Graph, Walked0, Count0, Walked1, Count1),
    dfs(X1,Y1, Graph, Walked1, Count1, Walked2, Count2),
    dfs(X2,Y2, Graph, Walked2, Count2, Walked3, Count3),
    dfs(X3,Y3, Graph, Walked3, Count3, Walked4, Count4),
    dfs(X4,Y4, Graph, Walked4, Count4, Walked5, Count5),
    dfs(X5,Y5, Graph, Walked5, Count5, OutWalked, OutCount).
    
    
find_updates(L, [], L).
find_updates([_|T1], [_|T2], R):-    
    find_updates(T1,T2,R).

get_bug_from_position([X, Y], [[B, Color]|Bugs], Result):-
    get(B, position, Pos),
    get(Pos, x, X_), BugX is X_ -20,
    get(Pos, y, Y_), BugY is Y_ - 20,

    (
        (BugX =:= X, BugY =:=Y, Result = [B,Color]);
        get_bug_from_position([X,Y],Bugs, Result)
    ).

check_stack(@black_beetle_1, C):-
    nb_getval(bb1, Stack), list_length(Stack, C).
check_stack(@black_beetle_2, C):-
    nb_getval(bb2, Stack), list_length(Stack, C).
check_stack(@red_beetle_1, C):-
    nb_getval(rb1, Stack), list_length(Stack, C).
check_stack(@red_beetle_2, C):-
    nb_getval(rb2, Stack), list_length(Stack, C).
check_stack(@black_mosquito, C):-
    nb_getval(bm, Stack), list_length(Stack, C).
check_stack(@red_mosquito, C):-
    nb_getval(rm, Stack), list_length(Stack, C).
check_stack(_, 0).

get_stack(@black_beetle_1, B):-
    nb_getval(bb1, Stack), is_equal(Stack, B).
get_stack(@black_beetle_2, B):-
    nb_getval(bb2, Stack), is_equal(Stack, B).
get_stack(@red_beetle_1, B):-
    nb_getval(rb1, Stack), is_equal(Stack, B).
get_stack(@red_beetle_2, B):-
    nb_getval(rb2, Stack), is_equal(Stack, B).
get_stack(@black_mosquito, B):-
    nb_getval(bm, Stack), is_equal(Stack, B).
get_stack(@red_mosquito, B):-
    nb_getval(rm, Stack), is_equal(Stack, B).
get_stack(_, []).