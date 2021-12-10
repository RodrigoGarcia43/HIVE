:- module(ant, [
    ant/5,
    ant_make_decision/3,
    ant_obtain_posible_moves/2
    ]).

% Return the posible moves to make for the given bug
ant([], _, _, Positions, Positions).
ant([[X, Y]|T], Xant, Yant, Positions, OutPositions):-
    
    Occupied = [],
    nb_getval(bugs, Bugs),
    get_occupied_positions(Bugs, Occupied, UpdatedOccupied),    

    X0 is X + 70, Y0 is Y + 49, %dawn right
    X1 is X + 70, Y1 is Y - 49, %up right
    X2 is X , Y2 is Y - 98, %up
    X3 is X - 70, Y3 is Y - 49, %up left
    X4 is X - 70, Y4 is Y + 49, %dawn left
    X5 is X , Y5 is Y + 98, %dawn

    try_add_free_position([X0 , Y0], Positions, UpdatedOccupied, R1, [X5, Y5], [X1,Y1]),    
    try_add_free_position([X1 , Y1], R1, UpdatedOccupied, R2, [X0,Y0], [X2,Y2]),        
    try_add_free_position([X2, Y2], R2, UpdatedOccupied, R3, [X1, Y1], [X3,Y3]),    
    try_add_free_position([X3 , Y3], R3, UpdatedOccupied, R4, [X2,Y2], [X4,Y4]),    
    try_add_free_position([X4 , Y4], R4, UpdatedOccupied, R5, [X3,Y3], [X5,Y5]),
    try_add_free_position([X5, Y5], R5, UpdatedOccupied, R6, [X4,Y4], [X0,Y0]),
    list_remove([Xant,Yant], UpdatedOccupied, Graph),
    check_conex(R6, Graph, ReversedIterationPositions),
    list_reverse(ReversedIterationPositions, IterationPositions),

    find_updates(IterationPositions, Positions, AddedPositions),
    list_concat(T, AddedPositions, NewT),
    ant(NewT, Xant, Yant, IterationPositions, OutPositions).

%-------------------------------------------------------------------------------------------------
% IA Section
%-------------------------------------------------------------------------------------------------

ant_make_decision(Bug, BestMove, Score):-
    (check_all_stacks(Bug, _), BestMove = [-1,-1], Score is -1000);
    (ant_obtain_posible_moves(Bug, Moves),    

    ((is_equal(Moves, []), BestMove = [-1,-1], Score is -1000);
    (Occupied = [],
    nb_getval(bugs, Bugs),
    get_occupied_positions(Bugs, Occupied, Occupied0),
    
    get(Bug, position, Pos),
    get(Pos, x, X_), X is X_ -20,
    get(Pos, y, Y_), Y is Y_ - 20,

    (
        (\+ list_contains([X,Y], Occupied0), Occupied1 = Occupied0);
        list_remove([X,Y], Occupied0, Occupied1)
    ),
    
    ant_select_best(Moves, Occupied1, [X,Y], 0, BestMove, Score0),

    (
        (\+ list_contains([X,Y], Occupied0), Score is Score0 + 20);
        (ant_calificate_position([X,Y], Occupied1, Score1),
        Score = Score0 - (Score1 * 1.2))
    )))).
    


ant_select_best([], _, B, S, B, S).
ant_select_best([[X,Y]|Moves], Occupied, PrevBest, PrevScore, Best, Score):-
    ant_calificate_position([X,Y], Occupied, NewScore),
    (
        (NewScore >= PrevScore, ant_select_best(Moves, Occupied, [X,Y], NewScore, Best, Score));
        ant_select_best(Moves, Occupied, PrevBest, PrevScore, Best, Score)
    ).


ant_calificate_position([X,Y], Occupied, Score):-
    X0 is X + 70, Y0 is Y + 49, %dawn right
    X1 is X + 70, Y1 is Y - 49, %up right
    X2 is X , Y2 is Y - 98, %up
    X3 is X - 70, Y3 is Y - 49, %up left
    X4 is X - 70, Y4 is Y + 49, %dawn left
    X5 is X , Y5 is Y + 98, %dawn

    Count = 0,

    ant_look_in_direction([X0 , Y0], Occupied, Count, Count0, [X5, Y5], [X1,Y1]),    
    ant_look_in_direction([X1 , Y1], Occupied, Count0, Count1, [X0,Y0], [X2,Y2]),        
    ant_look_in_direction([X2, Y2], Occupied, Count1, Count2, [X1, Y1], [X3,Y3]),    
    ant_look_in_direction([X3 , Y3], Occupied, Count2, Count3, [X2,Y2], [X4,Y4]),    
    ant_look_in_direction([X4 , Y4], Occupied, Count3, Count4, [X3,Y3], [X5,Y5]),
    ant_look_in_direction([X5, Y5], Occupied, Count4, Score, [X4,Y4], [X0,Y0]).


ant_look_in_direction([X,Y], Occupied, PrevC, C, [XL, YL], [XR, YR]):-    
    (        
        list_contains([X,Y], Occupied),       
        nb_getval(bugs, Bugs), 
        nb_getval(turn, Turn),
        get_bug_from_position([X,Y], Bugs, [Bug, Color]),
        % get(Bug, name, Name),
        (
            (
                is_equal(Turn, 'red'),                
                ((
                    is_equal(Bug, @red_queen), C0 is PrevC + 35,
                    (
                        ((\+ list_contains([XL,YL], Occupied), C1 is C0 + 10);
                        C1 is C0),
                        ((\+ list_contains([XR,YR], Occupied), C is C1 + 10);
                        C is C1)
                    )
                );
                (is_equal(Color, 'black'), C is PrevC + 0.3);
                C is PrevC)
            );
            (
                is_equal(Turn, 'black'),
                ((
                    is_equal(Bug, @black_queen), C0 is PrevC + 35,
                    (
                        ((\+ list_contains([XL,YL], Occupied), C1 is C0 + 10);
                        C1 is C0),
                        ((\+ list_contains([XR,YR], Occupied), C is C1 + 10);
                        C is C1)
                    )
                );
                (is_equal(Color, 'red'), C is PrevC + 0.3);
                C is PrevC)
            )
        )
    );
    C is PrevC + 0.2.


ant_obtain_posible_moves(Bug, Result):-
    (
        (
            Occupied = [],
            nb_getval(bugs, Bugs),
            get_occupied_positions(Bugs, Occupied, Occupied0),
        
            get(Bug, position, Pos),
            get(Pos, x, X_), X is X_ -20,
            get(Pos, y, Y_), Y is Y_ - 20,

            list_contains([X,Y], Occupied0), 
            ant([[X,Y]], X, Y, [], Result)
        );

        new_bug(Result)
    ).

