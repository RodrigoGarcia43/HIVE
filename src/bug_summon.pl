:- module(bug_summon, [
    new_bug/1
    ]).

% Return the posible moves a bug can makes from outside the board
new_bug(Positions):-
    nb_getval(bugs, Bugs),
    Free = [],
    Occupied = [],
    new_bug_update_board(Bugs, Free, Occupied, UpdatedFree, UpdatedOccupied),  
    new_bug_clean(Bugs, UpdatedOccupied, UpdatedFree, Positions). 

new_bug_update_board([], Free, Occupied, Free, Occupied).
new_bug_update_board([[Bug,Color]|T], Free, Occupied, UpdatedFree, UpdatedOccupied):-
    get(Bug, position, Position),
    get(Position, x, X_), X is X_ -20,
    get(Position, y, Y_), Y is Y_ - 20,
    
    nb_getval(turn, Turn),

    list_add([X,Y], Occupied, NewOccupied),

    new_bug_update_board(T, Free, NewOccupied, NewFree, UpdatedOccupied),

    ((
    list_length(UpdatedOccupied, C), C > 1,
     \+ Turn \= Color, UpdatedFree = NewFree) ;

    (X0 is X + 70, Y0 is Y + 49,
    try_add_free_position([X0 , Y0], NewFree, UpdatedOccupied, R1, [-100000, -100000], [-100000, -100000]),
    X1 is X + 70, Y1 is Y - 49,
    try_add_free_position([X1 , Y1], R1, UpdatedOccupied, R2, [-100000, -100000], [-100000, -100000]),    
    X2 is X , Y2 is Y - 98,
    try_add_free_position([X2, Y2], R2, UpdatedOccupied, R3, [-100000, -100000], [-100000, -100000]),
    X3 is X - 70, Y3 is Y - 49,
    try_add_free_position([X3 , Y3], R3, UpdatedOccupied, R4, [-100000, -100000], [-100000, -100000]),
    X4 is X - 70, Y4 is Y + 49,
    try_add_free_position([X4 , Y4], R4, UpdatedOccupied, R5, [-100000, -100000], [-100000, -100000]),
    X5 is X , Y5 is Y + 98,
    try_add_free_position([X5, Y5], R5, UpdatedOccupied, UpdatedFree, [-100000, -100000], [-100000, -100000]))).

new_bug_clean([], _, Free, Free).
new_bug_clean([[Bug,Color]|T], Occupied, Free, UpdatedFree):-
    (get(Bug, position, Position),
    get(Position, x, X_), X is X_ -20,
    get(Position, y, Y_), Y is Y_ - 20,
    
    nb_getval(turn, Turn),

    new_bug_clean(T, Occupied, Free, NewFree)),    

    (
        (((list_length(Occupied, C), C < 2); Turn \= Color), UpdatedFree = NewFree);

        (X0 is X + 70, Y0 is Y + 49, %dawn right
        X1 is X + 70, Y1 is Y - 49, %up right
        X2 is X , Y2 is Y - 98, %up
        X3 is X - 70, Y3 is Y - 49, %up left
        X4 is X - 70, Y4 is Y + 49, %dawn left
        X5 is X , Y5 is Y + 98, %dawn

        try_list_remove([X0 , Y0], NewFree, R1),    
        try_list_remove([X1 , Y1], R1, R2),        
        try_list_remove([X2, Y2], R2, R3),    
        try_list_remove([X3 , Y3], R3, R4),    
        try_list_remove([X4 , Y4], R4, R5),
        try_list_remove([X5, Y5], R5, UpdatedFree))
    ).

try_list_remove(X, L, R):-
    (\+ list_contains(X, L), R = L);
    list_remove(X, L, R).

