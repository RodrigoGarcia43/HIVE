:- module(beetle, [
    beetle/2,
    beetle_make_decision/3,
    beetle_obtain_posible_moves/2
    ]).

% Return the posible moves to make for the given bug
beetle(Beetle, Positions):-
    Occupied = [],
    nb_getval(bugs, Bugs),
    get_occupied_positions(Bugs, Occupied, UpdatedOccupied),

    get(Beetle, position, Pos),
    get(Pos, x, X_), X is X_ -20,
    get(Pos, y, Y_), Y is Y_ - 20,

    NewFree = [],
    

    X0 is X + 70, Y0 is Y + 49, %dawn right
    X1 is X + 70, Y1 is Y - 49, %up right
    X2 is X , Y2 is Y - 98, %up
    X3 is X - 70, Y3 is Y - 49, %up left
    X4 is X - 70, Y4 is Y + 49, %dawn left
    X5 is X , Y5 is Y + 98, %dawn

    try_add_free_position([X0 , Y0], NewFree, [], R1, [-1000,-1000], [-1000,-1000]),    
    try_add_free_position([X1 , Y1], R1, [], R2,[-1000,-1000], [-1000,-1000]),        
    try_add_free_position([X2, Y2], R2, [], R3, [-1000,-1000], [-1000,-1000]),    
    try_add_free_position([X3 , Y3], R3, [], R4, [-1000,-1000], [-1000,-1000]),    
    try_add_free_position([X4 , Y4], R4, [], R5,[-1000,-1000], [-1000,-1000]),
    try_add_free_position([X5, Y5], R5, [], R6, [-1000,-1000], [-1000,-1000]),

    (check_stack(Beetle, C), C > 0, Graph = UpdatedOccupied; list_remove([X,Y], UpdatedOccupied, Graph)),

    check_conex(R6, Graph, Positions).

%-------------------------------------------------------------------------------------------------
% IA Section
%-------------------------------------------------------------------------------------------------

beetle_make_decision(Bug, BestMove, Score):-
    (check_all_stacks(Bug, _), BestMove = [-1,-1], Score is -1000);
    (beetle_obtain_posible_moves(Bug, Moves),    

    ((is_equal(Moves, []), BestMove = [-1,-1], Score is -1000);
    (
    
    Occupied = [],
    nb_getval(bugs, Bugs),
    nb_getval(turn, Turn),
    get_occupied_positions(Bugs, Occupied, Occupied0),
    
    (
        (check_stack(Bug, C), C \= 1, Score3 = 0);
        (
            get_stack(Bug, [StackedBug, StackedColor]),
                       
            get(StackedBug, name, Name),
            (
                (is_equal(Name, 'ant'), Score3_ is 5);
                (is_equal(Name, 'queen'), Score3_ is 2);
                (is_equal(Name, 'beetle'), Score3_ is 2);
                Score3_ is 1
            ),
            (
                (
                    is_equal(Turn, 'red'),
                    ((is_equal(StackedColor, 'red'), Score3 is -Score3_);
                    (is_equal(StackedColor, 'black'), Score3 is +Score3_))
                );
                (
                    is_equal(Turn, 'black'),
                    ((is_equal(StackedColor, 'black'), Score3 is -Score3_);
                    (is_equal(StackedColor, 'red'), Score3 is +Score3_))
                )
            )
        )
    ),
    
    get(Bug, position, Pos),
    get(Pos, x, X_), X is X_ -20,
    get(Pos, y, Y_), Y is Y_ -20,
    
    (
        ((\+ list_contains([X,Y], Occupied0), Occupied1 = Occupied0, OnBoard is 0));
        (list_remove([X,Y], Occupied0, Occupied1), OnBoard is 1)
    ),
    
    beetle_select_best_depth_2(Moves, Bug, Occupied1, OnBoard, [X,Y], -1000000000, BestMove, Score0),

    (
        (\+ (OnBoard > 0), Score is Score0 + 5);
        (
            beetle_calificate_position([X,Y], OnBoard, Occupied0, Score1),
            
            Score is (Score0 - (Score1 * 1.2) - Score3) * 0.35
        )
    )))).


beetle_select_best_depth_2([], _, _, _, B, S, B, S).
beetle_select_best_depth_2([[X,Y]|Moves], Bug, Occupied, OnBoard, PrevBest, PrevScore, Best, Score):-
    beetle_calificate_position([X,Y], OnBoard, Occupied, NewScore1),

    beetle_obtain_posible_moves_from_position([X,Y], Bug, Occupied, NewMoves),
    beetle_select_best(NewMoves, Occupied, OnBoard, _, -10000000000, _, DepthScore),

    NewScore is NewScore1 + DepthScore / 2,


    (
        (NewScore >= PrevScore, beetle_select_best_depth_2(Moves, Bug, Occupied, OnBoard, [X,Y], NewScore, Best, Score));
        beetle_select_best_depth_2(Moves, Bug, Occupied, OnBoard, PrevBest, PrevScore, Best, Score)
    ).

beetle_select_best([], _, _, B, S, B, S).
beetle_select_best([[X,Y]|Moves], Occupied, OnBoard, PrevBest, PrevScore, Best, Score):-
    beetle_calificate_position([X,Y], OnBoard, Occupied, NewScore),


    (
        (NewScore >= PrevScore, beetle_select_best(Moves, Occupied, OnBoard, [X,Y], NewScore, Best, Score));
        beetle_select_best(Moves, Occupied, OnBoard, PrevBest, PrevScore, Best, Score)
    ).


beetle_calificate_position([X,Y], OnBoard , Occupied, Score):-
    X0 is X + 70, Y0 is Y + 49, %dawn right
    X1 is X + 70, Y1 is Y - 49, %up right
    X2 is X , Y2 is Y - 98, %up
    X3 is X - 70, Y3 is Y - 49, %up left
    X4 is X - 70, Y4 is Y + 49, %dawn left
    X5 is X , Y5 is Y + 98, %dawn

    Count = 0,

    beetle_look_in_direction([X0 , Y0], Occupied, Count, Count0, Q0),
    beetle_look_in_direction([X1 , Y1], Occupied, Count0, Count1, Q1),        
    beetle_look_in_direction([X2, Y2], Occupied, Count1, Count2, Q2),    
    beetle_look_in_direction([X3 , Y3], Occupied, Count2, Count3, Q3),    
    beetle_look_in_direction([X4 , Y4], Occupied, Count3, Count4, Q4),
    beetle_look_in_direction([X5, Y5], Occupied, Count4, Score2, Q5),

    (
        (
            list_contains([X,Y], Occupied),       
            nb_getval(bugs, Bugs), 
            nb_getval(turn, Turn),
            get_bug_from_position([X,Y], Bugs, [Bug, Color]),
            get(Bug, name, Name),
            (
                (
                    is_equal(Turn, 'red'),
                    ((
                        is_equal(Color, 'red'),
                        (
                            (is_equal(Name, 'ant'), Score1 is 5);
                            (is_equal(Bug, @red_queen), Score1 is -100);
                            (is_equal(Name, 'beetle'), Score1 is 3);
                            Score1 is 1
                        )
                    );
                    is_equal(Color, 'red'), Score1 is -5)
                );
                (
                    is_equal(Turn, 'black'),
                    ((
                        is_equal(Color, 'black'),
                        (
                            (is_equal(Name, 'ant'), Score1 is 5);
                            (is_equal(Bug, @black_queen), Score1 is -100);
                            (is_equal(Name, 'beetle'), Score1 is 3);
                            Score1 is 1
                        )
                    );
                    is_equal(Color, 'red'), Score1 is -5)
                )
            )
        );
        (
            (Q is Q0 + Q1 + Q2 + Q3 + Q4 + Q5, Q > 0, Score1 is 9);
            Score1 is 0
        )
        
    ),
    (
        (\+ (OnBoard > 0), Score3 is 0);
        (
            (
                (
                    is_equal(Turn, 'red'),
                    get(@red_queen, position, Pos)
                );
                (
                    is_equal(Turn, 'black'),
                    get(@black_queen, position, Pos)
                )
            ),
            get(Pos, x, X_), QX is X_ -20,
            get(Pos, y, Y_), QY is Y_ -20,
            DisX_ is X - QX,
            potencia(DisX_, 2, DisX),
            DisY_ is Y - QY,
            potencia(DisY_, 2, DisY),
            Dis_ is DisX + DisY,
            sqrt(Dis_, Dis),        
            Score3 is Dis /10
        )
    ),
    
    % Score is Score1 + Score2. 
    Score is Score1 + Score2 - Score3.
    


beetle_look_in_direction([X,Y], Occupied, PrevC, C, Q):-
    (        
        list_contains([X,Y], Occupied),       
        nb_getval(bugs, Bugs), 
        nb_getval(turn, Turn),
        get_bug_from_position([X,Y], Bugs, [Bug, Color]),
        (
            (
                is_equal(Turn, 'red'),
                ((is_equal(Bug, @red_queen), C is PrevC + 15, Q is 1);
                (is_equal(Color, 'black'), C is PrevC + 0.3, Q is 0);
                C is PrevC, Q is 0)
            );
            (
                is_equal(Turn, 'black'),
                ((is_equal(Bug, @black_queen), C is PrevC + 15, Q is 1);
                (is_equal(Color, 'red'), C is PrevC + 0.3, Q is 0);
                C is PrevC, Q is 0)
            )
        )
    );
    C is PrevC, Q is 0.


beetle_obtain_posible_moves(Bug, Result):-
    Occupied = [],
    nb_getval(bugs, Bugs),
    get_occupied_positions(Bugs, Occupied, Occupied0),

    get(Bug, position, Pos),
    get(Pos, x, X_), X is X_ -20,
    get(Pos, y, Y_), Y is Y_ - 20,

    (
        (list_contains([X,Y], Occupied0), list_remove([X,Y], Occupied0, Occupied1),
        beetle_obtain_posible_moves_from_position([X,Y], Bug, Occupied1, Result));
        new_bug(Result)
    ).

beetle_obtain_posible_moves_from_position([X,Y], Bug, Occupied, Result):-
    list_add([X,Y], Occupied, UpdatedOccupied),

    NewFree = [],
    

    X0 is X + 70, Y0 is Y + 49, %dawn right
    X1 is X + 70, Y1 is Y - 49, %up right
    X2 is X , Y2 is Y - 98, %up
    X3 is X - 70, Y3 is Y - 49, %up left
    X4 is X - 70, Y4 is Y + 49, %dawn left
    X5 is X , Y5 is Y + 98, %dawn

    try_add_free_position([X0 , Y0], NewFree, [], R1, [-1000,-1000], [-1000,-1000]),    
    try_add_free_position([X1 , Y1], R1, [], R2,[-1000,-1000], [-1000,-1000]),        
    try_add_free_position([X2, Y2], R2, [], R3, [-1000,-1000], [-1000,-1000]),    
    try_add_free_position([X3 , Y3], R3, [], R4, [-1000,-1000], [-1000,-1000]),    
    try_add_free_position([X4 , Y4], R4, [], R5,[-1000,-1000], [-1000,-1000]),
    try_add_free_position([X5, Y5], R5, [], R6, [-1000,-1000], [-1000,-1000]),

    (check_stack(Bug, C), C > 0, Graph = UpdatedOccupied; list_remove([X,Y], UpdatedOccupied, Graph)),

    check_conex(R6, Graph, Result).