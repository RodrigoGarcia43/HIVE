:- module(grasshopper, [
    grasshopper/2,
    grasshopper_make_decision/3,
    grasshopper_obtain_posible_moves/2
    ]).

% Return the posible moves to make for the given bug
grasshopper(Grasshopper, Positions):-
    
    Occupied = [],
    nb_getval(bugs, Bugs),
    get_occupied_positions(Bugs, Occupied, UpdatedOccupied),

    get(Grasshopper, position, Pos),
    get(Pos, x, X_), X is X_ -20,
    get(Pos, y, Y_), Y is Y_ - 20,

    NewFree = [],
    

    X0 is X + 70, Y0 is Y + 49, %dawn right
    X1 is X + 70, Y1 is Y - 49, %up right
    X2 is X , Y2 is Y - 98, %up
    X3 is X - 70, Y3 is Y - 49, %up left
    X4 is X - 70, Y4 is Y + 49, %dawn left
    X5 is X , Y5 is Y + 98, %dawn

    try_jump_in_direction([X0 , Y0], NewFree, UpdatedOccupied, R1, 70, 49,1),    
    try_jump_in_direction([X1 , Y1], R1, UpdatedOccupied, R2, 70, -49,1),        
    try_jump_in_direction([X2, Y2], R2, UpdatedOccupied, R3, 0, -98,1),    
    try_jump_in_direction([X3 , Y3], R3, UpdatedOccupied, R4, -70, -49,1),    
    try_jump_in_direction([X4 , Y4], R4, UpdatedOccupied, R5, -70, 49,1),
    try_jump_in_direction([X5, Y5], R5, UpdatedOccupied, R6, 0, 98,1),
    list_remove([X,Y], UpdatedOccupied, Graph),
    check_conex(R6, Graph, Positions).

%-------------------------------------------------------------------------------------------------
% IA Section
%-------------------------------------------------------------------------------------------------

grasshopper_make_decision(Bug, BestMove, Score):-
    (check_all_stacks(Bug, _), BestMove = [-1,-1], Score is -1000);
    (grasshopper_obtain_posible_moves(Bug, Moves),    

    ((is_equal(Moves, []), BestMove = [-1,-1], Score is -1000);
    (
    
    Occupied = [],
    nb_getval(bugs, Bugs),
    get_occupied_positions(Bugs, Occupied, Occupied0),
    
    get(Bug, position, Pos),
    get(Pos, x, X_), X is X_ -20,
    get(Pos, y, Y_), Y is Y_ -20,
    
    (
        ((\+ list_contains([X,Y], Occupied0), Occupied1 = Occupied0, OnBoard is 0));
        (list_remove([X,Y], Occupied0, Occupied1), OnBoard is 1)
    ),
    
    grasshopper_select_best_depth_3(Moves, Bug, Occupied1, [X,Y], -1000000000, BestMove, Score0),

    (
        (\+ (OnBoard > 0), Score is Score0 + 2);
        (
            grasshopper_calificate_position([X,Y], Occupied0, Score1),
            
            Score is Score0 - (Score1 * 1.2)
        )
    )))).

grasshopper_select_best_depth_3([], _,  _, B, S, B, S).
grasshopper_select_best_depth_3([[X,Y]|Moves], Bug, Occupied, PrevBest, PrevScore, Best, Score):-
    grasshopper_calificate_position([X,Y], Occupied, NewScore1),

    grasshopper_obtain_posible_moves_from_position([X,Y], Bug, Occupied, NewMoves),
    grasshopper_select_best_depth_2(NewMoves, Bug, Occupied, _, -10000000000, _, DepthScore),

    NewScore is NewScore1 + DepthScore / 2,


    (
        (NewScore >= PrevScore, grasshopper_select_best_depth_3(Moves, Bug, Occupied, [X,Y], NewScore, Best, Score));
        grasshopper_select_best_depth_3(Moves, Bug, Occupied, PrevBest, PrevScore, Best, Score)
    ).

grasshopper_select_best_depth_2([], _, _, B, S, B, S).
grasshopper_select_best_depth_2([[X,Y]|Moves], Bug, Occupied, PrevBest, PrevScore, Best, Score):-
    grasshopper_calificate_position([X,Y], Occupied, NewScore1),

    grasshopper_obtain_posible_moves_from_position([X,Y], Bug, Occupied, NewMoves),
    grasshopper_select_best(NewMoves, Occupied, _, -10000000000, _, DepthScore),

    NewScore is NewScore1 + DepthScore / 4,


    (
        (NewScore >= PrevScore, grasshopper_select_best_depth_2(Moves, Bug, Occupied, [X,Y], NewScore, Best, Score));
        grasshopper_select_best_depth_2(Moves, Bug, Occupied, PrevBest, PrevScore, Best, Score)
    ).

grasshopper_select_best([], _, B, S, B, S).
grasshopper_select_best([[X,Y]|Moves], Occupied, PrevBest, PrevScore, Best, Score):-
    grasshopper_calificate_position([X,Y], Occupied, NewScore),


    (
        (NewScore >= PrevScore, grasshopper_select_best(Moves, Occupied, [X,Y], NewScore, Best, Score));
        grasshopper_select_best(Moves, Occupied, PrevBest, PrevScore, Best, Score)
    ).


grasshopper_calificate_position([X,Y] , Occupied, Score):-
    X0 is X + 70, Y0 is Y + 49, %dawn right
    X1 is X + 70, Y1 is Y - 49, %up right
    X2 is X , Y2 is Y - 98, %up
    X3 is X - 70, Y3 is Y - 49, %up left
    X4 is X - 70, Y4 is Y + 49, %dawn left
    X5 is X , Y5 is Y + 98, %dawn

    Count = 0,

    grasshopper_look_in_direction([X0 , Y0], Occupied, Count, Count0, Q0, C0),
    grasshopper_look_in_direction([X1 , Y1], Occupied, Count0, Count1, Q1, C1),        
    grasshopper_look_in_direction([X2, Y2], Occupied, Count1, Count2, Q2, C2),    
    grasshopper_look_in_direction([X3 , Y3], Occupied, Count2, Count3, Q3, C3),    
    grasshopper_look_in_direction([X4 , Y4], Occupied, Count3, Count4, Q4, C4),
    grasshopper_look_in_direction([X5, Y5], Occupied, Count4, Score1, Q5, C5),
    
    Sum is C0 + C1 + C2 + C3 + C4 + C5,
    Q is Q0 + Q1 + Q2 + Q3 + Q4 + Q5,
    (
      (Sum > 4, Q > 0, Score2 is 20);
      Score2 is 0
    ),
    % Score is Score1 + Score2. 
    Score is Score1 + Score2.
    


grasshopper_look_in_direction([X,Y], Occupied, PrevC, C, Q, Any):-
    (        
        list_contains([X,Y], Occupied), Any is 1,     
        nb_getval(bugs, Bugs), 
        nb_getval(turn, Turn),
        get_bug_from_position([X,Y], Bugs, [Bug, Color]),
        (
            (
                is_equal(Turn, 'red'),
                ((is_equal(Bug, @red_queen), C is PrevC + 20, Q is 1);
                (is_equal(Color, 'black'), C is PrevC + 2, Q is 0);
                C is PrevC, Q is 0  )
            );
            (
                is_equal(Turn, 'black'),
                ((is_equal(Bug, @black_queen), C is PrevC + 20, Q is 1);
                (is_equal(Color, 'red'), C is PrevC + 2, Q is 0);
                C is PrevC, Q is 0)
            )
        )
    );
    C is PrevC, Q is 0, Any is 0.


grasshopper_obtain_posible_moves(Bug, Result):-
    Occupied = [],
    nb_getval(bugs, Bugs),
    get_occupied_positions(Bugs, Occupied, Occupied0),

    get(Bug, position, Pos),
    get(Pos, x, X_), X is X_ -20,
    get(Pos, y, Y_), Y is Y_ - 20,

    (
        (list_contains([X,Y], Occupied0), list_remove([X,Y], Occupied0, Occupied1),
        grasshopper_obtain_posible_moves_from_position([X,Y], Bug, Occupied1, Result));
        new_bug(Result)
    ).

grasshopper_obtain_posible_moves_from_position([X,Y], _, Occupied, Result):-
    list_add([X,Y], Occupied, UpdatedOccupied),

    NewFree = [],
    

    X0 is X + 70, Y0 is Y + 49, %dawn right
    X1 is X + 70, Y1 is Y - 49, %up right
    X2 is X , Y2 is Y - 98, %up
    X3 is X - 70, Y3 is Y - 49, %up left
    X4 is X - 70, Y4 is Y + 49, %dawn left
    X5 is X , Y5 is Y + 98, %dawn

    try_jump_in_direction([X0 , Y0], NewFree, UpdatedOccupied, R1, 70, 49,1),    
    try_jump_in_direction([X1 , Y1], R1, UpdatedOccupied, R2, 70, -49,1),        
    try_jump_in_direction([X2, Y2], R2, UpdatedOccupied, R3, 0, -98,1),    
    try_jump_in_direction([X3 , Y3], R3, UpdatedOccupied, R4, -70, -49,1),    
    try_jump_in_direction([X4 , Y4], R4, UpdatedOccupied, R5, -70, 49,1),
    try_jump_in_direction([X5, Y5], R5, UpdatedOccupied, R6, 0, 98,1),
    list_remove([X,Y], UpdatedOccupied, Graph),
    check_conex(R6, Graph, Result).