:- module(mosquito, [
    mosquito/2 
    ]).

% Return the posible moves to make for the given bug
mosquito(Mosquito, Positions):-
    Occupied = [],
    nb_getval(bugs, Bugs),
    get_occupied_positions(Bugs, Occupied, UpdatedOccupied),

    get(Mosquito, position, Pos),
    get(Pos, x, X_), X is X_ -20,
    get(Pos, y, Y_), Y is Y_ - 20,

    NewFree = [],
    

    X0 is X + 70, Y0 is Y + 49, %dawn right
    X1 is X + 70, Y1 is Y - 49, %up right
    X2 is X , Y2 is Y - 98, %up
    X3 is X - 70, Y3 is Y - 49, %up left
    X4 is X - 70, Y4 is Y + 49, %dawn left
    X5 is X , Y5 is Y + 98, %dawn

    (
        (check_stack(Mosquito, C), C > 0, beetle(Mosquito, Positions));

        (
            mosquito_copy_in_direction([X0 , Y0], Mosquito, UpdatedOccupied,  NewFree, R1),    
            mosquito_copy_in_direction([X1 , Y1], Mosquito, UpdatedOccupied, R1, R2),        
            mosquito_copy_in_direction([X2, Y2], Mosquito, UpdatedOccupied, R2, R3),    
            mosquito_copy_in_direction([X3 , Y3], Mosquito, UpdatedOccupied, R3, R4),    
            mosquito_copy_in_direction([X4 , Y4], Mosquito, UpdatedOccupied, R4, R5),
            mosquito_copy_in_direction([X5, Y5], Mosquito, UpdatedOccupied, R5, Positions)
        )
    ).

%-------------------------------------------------------------------------------------------------
% IA Section
%-------------------------------------------------------------------------------------------------

mosquito_copy_in_direction([X, Y], Mosquito, Occupied, Positions, UpdatedPositions):-
    (\+ list_contains([X,Y], Occupied), UpdatedPositions = Positions);
    (
        nb_getval(bugs, Bugs),
        get_bug_from_position([X,Y], Bugs, [Bug, _]),

        get(Bug, name, Name),
        % send(@pce, write_ln, Name),
        
        (
            is_equal(Name, 'mosquito'), UpdatedPositions = Positions;
            is_equal(Name, 'queen'), queen(Mosquito, L);
            is_equal(Name, 'grasshopper'), grasshopper(Mosquito, L);
            is_equal(Name, 'beetle'), beetle(Mosquito, L);
            is_equal(Name, 'ant'), ant([[X, Y]], X, Y, [], L);
            is_equal(Name, 'spider'), spider([[X, Y]], X, Y, 3, [], _, [], L);
            is_equal(Name, 'ladybug'), ladybug([[X, Y]], X, Y, 3, [], _, [], L)
        ),
        list_union(L, Positions, UpdatedPositions)
    ).



