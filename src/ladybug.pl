:- module(ladybug, [
    ladybug/8
    ]).

% Return the posible moves to make for the given bug
ladybug([],_,_,_,P,P,R,R).
ladybug([[X, Y]|T], Xladybug, Yladybug, Count, Positions0, OutPositions, PrevResult, Result):-
    (UpdatedCount is Count - 1),

    ((\+ UpdatedCount >= 0, OutPositions = Positions0, Result = PrevResult);
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

    (
        (list_length(Positions0, Len), Len > 0, Positions = Positions0);
        Positions = [[X,Y]]
    ),

    (
        (
            UpdatedCount >=1,
            ladybug_try_add_free_position([X0 , Y0], Positions, UpdatedOccupied, R1),    
            ladybug_try_add_free_position([X1 , Y1], R1, UpdatedOccupied, R2),        
            ladybug_try_add_free_position([X2, Y2], R2, UpdatedOccupied, R3),    
            ladybug_try_add_free_position([X3 , Y3], R3, UpdatedOccupied, R4),    
            ladybug_try_add_free_position([X4 , Y4], R4, UpdatedOccupied, R5),
            ladybug_try_add_free_position([X5, Y5], R5, UpdatedOccupied, R6)
        );
        (try_add_free_position([X0 , Y0], Positions, UpdatedOccupied, R1, [-100000000000, -100000000000], [-100000000000, -100000000000]),    
        try_add_free_position([X1 , Y1], R1, UpdatedOccupied, R2, [-100000000000, -100000000000], [-100000000000, -100000000000]),        
        try_add_free_position([X2, Y2], R2, UpdatedOccupied, R3, [-100000000000, -100000000000], [-100000000000, -100000000000]),    
        try_add_free_position([X3 , Y3], R3, UpdatedOccupied, R4, [-100000000000, -100000000000], [-100000000000, -100000000000]),    
        try_add_free_position([X4 , Y4], R4, UpdatedOccupied, R5, [-100000000000, -100000000000], [-100000000000, -100000000000]),
        try_add_free_position([X5, Y5], R5, UpdatedOccupied, R6, [-100000000000, -100000000000], [-100000000000, -100000000000]))
    ),

    
    list_remove([Xladybug,Yladybug], UpdatedOccupied, Graph),
    check_conex(R6, Graph, ReversedIterationPositions),
    list_reverse(ReversedIterationPositions, IterationPositions),

    find_updates(IterationPositions, Positions, AddedPositions),

    
    ladybug(T, Xladybug, Yladybug, Count, IterationPositions, NewPositions, PrevResult, FinalResult),

    (
        ((\+ UpdatedCount >= 1,        
        list_concat(FinalResult, AddedPositions, NewResult));
        NewResult = FinalResult
        )    
    ),

    ladybug(AddedPositions, Xladybug, Yladybug, UpdatedCount, NewPositions, OutPositions, NewResult, Result))).

ladybug_try_add_free_position([X,Y], Free, Occupied, NewFree):-
    (list_contains([X,Y], Occupied),
    \+ list_contains([X,Y], Free),
    
    list_add([X,Y], Free, NewFree), list_length(NewFree, L), (send(@pce, write_ln, L)));
    NewFree = Free.

