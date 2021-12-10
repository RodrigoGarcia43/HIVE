:- module(queen, [
    queen/2,
    queen_make_decision/3,
    queen_obtain_posible_moves/2
    ]).

% Return the posible moves to make for the given bug
queen(Bug, Positions):-    
    
    Occupied = [],
    nb_getval(bugs, Bugs),
    get_occupied_positions(Bugs, Occupied, UpdatedOccupied),

    get(Bug, position, Pos),
    get(Pos, x, X_), X is X_ -20,
    get(Pos, y, Y_), Y is Y_ - 20,

    NewFree = [],
    

    X0 is X + 70, Y0 is Y + 49, %dawn right
    X1 is X + 70, Y1 is Y - 49, %up right
    X2 is X , Y2 is Y - 98, %up
    X3 is X - 70, Y3 is Y - 49, %up left
    X4 is X - 70, Y4 is Y + 49, %dawn left
    X5 is X , Y5 is Y + 98, %dawn

    try_add_free_position([X0 , Y0], NewFree, UpdatedOccupied, R1, [X5, Y5], [X1,Y1]),    
    try_add_free_position([X1 , Y1], R1, UpdatedOccupied, R2, [X0,Y0], [X2,Y2]),        
    try_add_free_position([X2, Y2], R2, UpdatedOccupied, R3, [X1, Y1], [X3,Y3]),    
    try_add_free_position([X3 , Y3], R3, UpdatedOccupied, R4, [X2,Y2], [X4,Y4]),    
    try_add_free_position([X4 , Y4], R4, UpdatedOccupied, R5, [X3,Y3], [X5,Y5]),
    try_add_free_position([X5, Y5], R5, UpdatedOccupied, R6, [X4,Y4], [X0,Y0]),
    list_remove([X,Y], UpdatedOccupied, Graph),
    check_conex(R6, Graph, Positions).

%-------------------------------------------------------------------------------------------------
% IA Section
%-------------------------------------------------------------------------------------------------

queen_make_decision(Bug, BestMove, Score):-
    (check_all_stacks(Bug, _), BestMove = [-1,-1], Score is -1000);
    
    (queen_obtain_posible_moves(Bug, Moves),    

    ((is_equal(Moves, []), BestMove = [-1,-1], Score is -1000);
    (nb_getval(turn_count, TurnCount),

    Occupied = [],
    nb_getval(bugs, Bugs),
    get_occupied_positions(Bugs, Occupied, Occupied0),
    
    get(Bug, position, Pos),
    get(Pos, x, X_), X is X_ -20,
    get(Pos, y, Y_), Y is Y_ -20,

    (
        ((\+ list_contains([X,Y], Occupied0), Occupied1 = Occupied0),
            (TurnCount > 2, Score3 is 100);
            Score3 is 0);
        (list_remove([X,Y], Occupied0, Occupied1), Score3 is 0)
    ),
    
    queen_select_best_depth_2(Moves, Occupied1, [X,Y], 0, BestMove, Score0),

    queen_calificate_position([X,Y], Occupied0, Score1),
    Score is Score0 /4 + (Score1 * 10) + Score3))).


queen_select_best_depth_2([], _, B, S, B, S).
queen_select_best_depth_2([[X,Y]|Moves], Occupied, PrevBest, PrevScore, Best, Score):-
    queen_calificate_position([X,Y], Occupied, NewScore0),

    NewScore1 is 6 - NewScore0,

    queen_obtain_posible_moves_from_position([X,Y], Occupied1, NewMoves),
    queen_select_best(NewMoves, Occupied1, _, 0, _, DepthScore),

    NewScore is NewScore1 + DepthScore / 4,


    (
        (NewScore >= PrevScore, queen_select_best_depth_2(Moves, Occupied, [X,Y], NewScore, Best, Score));
        queen_select_best_depth_2(Moves, Occupied, PrevBest, PrevScore, Best, Score)
    ).

queen_select_best([], _, B, S, B, S).
queen_select_best([[X,Y]|Moves], Occupied, PrevBest, PrevScore, Best, Score):-
    queen_calificate_position([X,Y], Occupied, NewScore0),

    NewScore is 12 - (NewScore0 * 2),

    (
        (NewScore >= PrevScore, queen_select_best(Moves, Occupied, [X,Y], NewScore, Best, Score));
        queen_select_best(Moves, Occupied, PrevBest, PrevScore, Best, Score)
    ).


queen_calificate_position([X,Y], Occupied, Score):-
    X0 is X + 70, Y0 is Y + 49, %dawn right
    X1 is X + 70, Y1 is Y - 49, %up right
    X2 is X , Y2 is Y - 98, %up
    X3 is X - 70, Y3 is Y - 49, %up left
    X4 is X - 70, Y4 is Y + 49, %dawn left
    X5 is X , Y5 is Y + 98, %dawn

    Count = 0,

    queen_look_in_direction([X0 , Y0], Occupied, Count, Count0),
    queen_look_in_direction([X1 , Y1], Occupied, Count0, Count1),        
    queen_look_in_direction([X2, Y2], Occupied, Count1, Count2),    
    queen_look_in_direction([X3 , Y3], Occupied, Count2, Count3),    
    queen_look_in_direction([X4 , Y4], Occupied, Count3, Count4),
    queen_look_in_direction([X5, Y5], Occupied, Count4, Score).


queen_look_in_direction([X,Y], Occupied, PrevC, C):-
    (list_contains([X,Y], Occupied), C is PrevC + 1);
    C is PrevC.


queen_obtain_posible_moves(Bug, Result):-
    Occupied = [],
    nb_getval(bugs, Bugs),
    get_occupied_positions(Bugs, Occupied, Occupied0),

    get(Bug, position, Pos),
    get(Pos, x, X_), X is X_ -20,
    get(Pos, y, Y_), Y is Y_ - 20,

    (
        (list_contains([X,Y], Occupied0), list_remove([X,Y], Occupied0, Occupied1),
        queen_obtain_posible_moves_from_position([X,Y], Occupied1, Result));
        new_bug(Result)
    ).

queen_obtain_posible_moves_from_position([X,Y], Occupied, Result):-
    list_add([X,Y], Occupied, UpdatedOccupied),

    NewFree = [],
    

    X0 is X + 70, Y0 is Y + 49, %dawn right
    X1 is X + 70, Y1 is Y - 49, %up right
    X2 is X , Y2 is Y - 98, %up
    X3 is X - 70, Y3 is Y - 49, %up left
    X4 is X - 70, Y4 is Y + 49, %dawn left
    X5 is X , Y5 is Y + 98, %dawn

    try_add_free_position([X0 , Y0], NewFree, UpdatedOccupied, R1, [X5, Y5], [X1,Y1]),    
    try_add_free_position([X1 , Y1], R1, UpdatedOccupied, R2, [X0,Y0], [X2,Y2]),        
    try_add_free_position([X2, Y2], R2, UpdatedOccupied, R3, [X1, Y1], [X3,Y3]),    
    try_add_free_position([X3 , Y3], R3, UpdatedOccupied, R4, [X2,Y2], [X4,Y4]),    
    try_add_free_position([X4 , Y4], R4, UpdatedOccupied, R5, [X3,Y3], [X5,Y5]),
    try_add_free_position([X5, Y5], R5, UpdatedOccupied, R6, [X4,Y4], [X0,Y0]),
    list_remove([X,Y], UpdatedOccupied, Graph),
    check_conex(R6, Graph, Result).