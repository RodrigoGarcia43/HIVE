:- module(spider, [
    spider/8,
    spider_make_decision/3,
    spider_obtain_posible_moves/2
    ]).

% Return the posible moves to make for the given bug
spider([],_,_,_,P,P,R,R).
spider([[X, Y]|T], Xspider, Yspider, Count, Positions, OutPositions, PrevResult, Result):-
    (UpdatedCount is Count - 1),

    ((\+ UpdatedCount >= 0, OutPositions = Positions, Result = PrevResult);
    (
    
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
    list_remove([Xspider,Yspider], UpdatedOccupied, Graph),
    check_conex(R6, Graph, ReversedIterationPositions),
    list_reverse(ReversedIterationPositions, IterationPositions),

    find_updates(IterationPositions, Positions, AddedPositions),

    
    spider(T, Xspider, Yspider, Count, IterationPositions, NewPositions, PrevResult, FinalResult),

    (
        ((\+ UpdatedCount >= 1,        
        list_concat(FinalResult, AddedPositions, NewResult));
        NewResult = FinalResult
        )    
    ),

    spider(AddedPositions, Xspider, Yspider, UpdatedCount, NewPositions, OutPositions, NewResult, Result))).

%-------------------------------------------------------------------------------------------------
% IA Section
%-------------------------------------------------------------------------------------------------

spider_make_decision(Bug, BestMove, Score):-
    (check_all_stacks(Bug, _), BestMove = [-1,-1], Score is -1000);
    (spider_obtain_posible_moves(Bug, Moves),
    

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
    
    
    spider_select_best_depth_3(Moves, Bug, Occupied1, [X,Y], -1000000000, BestMove, Score0),
    
    (
        (\+ (OnBoard > 0), Score is Score0 + 20);
        (
            spider_calificate_position([X,Y], Occupied0, Score1),
            
            Score is Score0 - (Score1 * 1.2)
        )
    )))).

spider_select_best_depth_3([], _,  _, B, S, B, S).
spider_select_best_depth_3([[X,Y]|Moves], Bug, Occupied, PrevBest, PrevScore, Best, Score):-
    
    spider_calificate_position([X,Y], Occupied, NewScore1),

    % get(Bug, position, Pos),
    % get(Pos, x, X_), SX is X_ -20,
    % get(Pos, y, Y_), SY is Y_ - 20,
    
    list_add([X,Y], Occupied, UpdatedOccupied),
    spider_get_moves_from_position([[X,Y]], X, Y, 3, UpdatedOccupied, [], _, [], NewMoves),
    % list_length(NewMoves, L), send(@pce, write_ln, L),
    spider_select_best_depth_2(NewMoves, Bug, Occupied, _, -10000000000, _, DepthScore),

    NewScore is NewScore1 + DepthScore / 2,


    (
        (NewScore >= PrevScore, spider_select_best_depth_3(Moves, Bug, Occupied, [X,Y], NewScore, Best, Score));
        spider_select_best_depth_3(Moves, Bug, Occupied, PrevBest, PrevScore, Best, Score)
    ).

spider_select_best_depth_2([], _, _, B, S, B, S).
spider_select_best_depth_2([[X,Y]|Moves], Bug, Occupied, PrevBest, PrevScore, Best, Score):-
    spider_calificate_position([X,Y], Occupied, NewScore1),

    % get(Bug, position, Pos),
    % get(Pos, x, X_), SX is X_ -20,
    % get(Pos, y, Y_), SY is Y_ - 20,
    list_add([X,Y], Occupied, UpdatedOccupied),
    spider_get_moves_from_position([[X,Y]], X, Y, 3, UpdatedOccupied, [], _, [], NewMoves),
    spider_select_best(NewMoves, Occupied, _, -10000000000, _, DepthScore),

    NewScore is NewScore1 + DepthScore / 4,


    (
        (NewScore >= PrevScore, spider_select_best_depth_2(Moves, Bug, Occupied, [X,Y], NewScore, Best, Score));
        spider_select_best_depth_2(Moves, Bug, Occupied, PrevBest, PrevScore, Best, Score)
    ).

spider_select_best([], _, B, S, B, S).
spider_select_best([[X,Y]|Moves], Occupied, PrevBest, PrevScore, Best, Score):-
    spider_calificate_position([X,Y], Occupied, NewScore),


    (
        (NewScore >= PrevScore, spider_select_best(Moves, Occupied, [X,Y], NewScore, Best, Score));
        spider_select_best(Moves, Occupied, PrevBest, PrevScore, Best, Score)
    ).


spider_calificate_position([X,Y] , Occupied, Score):-
    X0 is X + 70, Y0 is Y + 49, %dawn right
    X1 is X + 70, Y1 is Y - 49, %up right
    X2 is X , Y2 is Y - 98, %up
    X3 is X - 70, Y3 is Y - 49, %up left
    X4 is X - 70, Y4 is Y + 49, %dawn left
    X5 is X , Y5 is Y + 98, %dawn

    Count = 0,

    spider_look_in_direction([X0 , Y0], Occupied, Count, Count0),
    spider_look_in_direction([X1 , Y1], Occupied, Count0, Count1),        
    spider_look_in_direction([X2, Y2], Occupied, Count1, Count2),    
    spider_look_in_direction([X3 , Y3], Occupied, Count2, Count3),    
    spider_look_in_direction([X4 , Y4], Occupied, Count3, Count4),
    spider_look_in_direction([X5, Y5], Occupied, Count4, Score).
    
    


spider_look_in_direction([X,Y], Occupied, PrevC, C):-
    (        
        list_contains([X,Y], Occupied),     
        nb_getval(bugs, Bugs), 
        nb_getval(turn, Turn),
        get_bug_from_position([X,Y], Bugs, [Bug, Color]),
        (
            (
                is_equal(Turn, 'red'),
                ((is_equal(Bug, @red_queen), C is PrevC + 30);
                (is_equal(Color, 'black'), C is PrevC + 0.3);
                C is PrevC)
            );
            (
                is_equal(Turn, 'black'),
                ((is_equal(Bug, @black_queen), C is PrevC + 30);
                (is_equal(Color, 'red'), C is PrevC + 0.3);
                C is PrevC)
            )
        )
    );
    C is PrevC.


spider_obtain_posible_moves(Bug, Result):-
    Occupied = [],
    nb_getval(bugs, Bugs),
    get_occupied_positions(Bugs, Occupied, Occupied0),

    get(Bug, position, Pos),
    get(Pos, x, X_), X is X_ -20,
    get(Pos, y, Y_), Y is Y_ - 20,

    (
        (list_contains([X,Y], Occupied0),
        spider_get_moves_from_position([[X,Y]], X, Y, 3, Occupied0, [], _, [], Result));
        new_bug(Result)
    ).

spider_get_moves_from_position([],_,_,_,_,P,P,R,R).
spider_get_moves_from_position([[X, Y]|T], Xspider, Yspider, Count, Occupied, Positions, OutPositions, PrevResult, Result):-
    % (send(@pce, write_ln, X),
    % send(@pce, write_ln, Y),
    (UpdatedCount is Count - 1),
    ((\+ UpdatedCount >= 0, OutPositions = Positions, Result = PrevResult);
    (  

    X0 is X + 70, Y0 is Y + 49, %dawn right
    X1 is X + 70, Y1 is Y - 49, %up right
    X2 is X , Y2 is Y - 98, %up
    X3 is X - 70, Y3 is Y - 49, %up left
    X4 is X - 70, Y4 is Y + 49, %dawn left
    X5 is X , Y5 is Y + 98, %dawn

    try_add_free_position([X0 , Y0], Positions, Occupied, R1, [X5, Y5], [X1,Y1]),    
    try_add_free_position([X1 , Y1], R1, Occupied, R2, [X0,Y0], [X2,Y2]),        
    try_add_free_position([X2, Y2], R2, Occupied, R3, [X1, Y1], [X3,Y3]),    
    try_add_free_position([X3 , Y3], R3, Occupied, R4, [X2,Y2], [X4,Y4]),    
    try_add_free_position([X4 , Y4], R4, Occupied, R5, [X3,Y3], [X5,Y5]),
    try_add_free_position([X5, Y5], R5, Occupied, R6, [X4,Y4], [X0,Y0]),
    list_remove([Xspider,Yspider], Occupied, Graph),
    check_conex(R6, Graph, ReversedIterationPositions),
    list_reverse(ReversedIterationPositions, IterationPositions),

    find_updates(IterationPositions, Positions, AddedPositions),

    
    spider_get_moves_from_position(T, Xspider, Yspider, Count, Occupied, IterationPositions, NewPositions, PrevResult, FinalResult),

    (
        ((\+ UpdatedCount >= 1,        
        list_concat(FinalResult, AddedPositions, NewResult));
        NewResult = FinalResult
        )    
    ),
    
    spider_get_moves_from_position(AddedPositions, Xspider, Yspider, UpdatedCount, Occupied, NewPositions, OutPositions, NewResult, Result))).
