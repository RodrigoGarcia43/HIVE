:- module(pillbug, [
    pillbug/3
    ]).

% Return the posible moves to make for the given bug
pillbug(Bug, Positions, Color):-    
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

    pillbug_try_add_free_position([X0 , Y0], Bug, Color, NewFree, UpdatedOccupied, R1, [X5, Y5], [X1,Y1]),    
    pillbug_try_add_free_position([X1 , Y1], Bug, Color, R1, UpdatedOccupied, R2, [X0,Y0], [X2,Y2]),        
    pillbug_try_add_free_position([X2, Y2], Bug, Color, R2, UpdatedOccupied, R3, [X1, Y1], [X3,Y3]),    
    pillbug_try_add_free_position([X3 , Y3], Bug, Color, R3, UpdatedOccupied, R4, [X2,Y2], [X4,Y4]),    
    pillbug_try_add_free_position([X4 , Y4], Bug, Color, R4, UpdatedOccupied, R5, [X3,Y3], [X5,Y5]),
    pillbug_try_add_free_position([X5, Y5], Bug, Color, R5, UpdatedOccupied, R6, [X4,Y4], [X0,Y0]),
    list_remove([X,Y], UpdatedOccupied, Graph),
    check_conex(R6, Graph, Positions).

pillbug_try_add_free_position([X,Y], Pillbug, Color, Free, Occupied, NewFree, [XObs1, YObs1], [XObs2, YObs2]):-
    (
        list_contains([X,Y], Occupied),
        nb_getval(bugs, Bugs),
        get_bug_from_position([X,Y], Bugs, [Bug, _]),
        nb_getval(last, Last),
        \+ is_equal(Last, Bug),
        

        (
            (check_stack(Bug, Stack), Stack > 0);
            set_pillbug_hex([X,Y], Pillbug, Bug, Color)
        ),
        
        NewFree = Free
    );
    (\+ list_contains([X,Y], Occupied),
    \+ list_contains([X,Y], Free),
    (\+ list_contains([XObs1, YObs1], Occupied); \+ list_contains([XObs2, YObs2], Occupied)),
    
    list_add([X,Y], Free, NewFree));
    NewFree = Free.


set_pillbug_hex([X,Y], Pillbug, TargetBug, Color):-
    new(D, device),
    new(Gray_hex, bitmap(resource(gray_hex), @on)),
    new(Gray_cube, bitmap(resource(gray_cube), @on)),
    send(Gray_hex, name, 1),send(Gray_cube, name, 1), 
    send(D, display, Gray_hex, point(0,0)),
    send(D, display, Gray_cube, point(20,20)),
    send(@board, display, D, point(X,Y)),
    send(Gray_cube, recogniser, click_gesture(left, '', single, message(@prolog, pillbug_select_bug_to_move, Pillbug, TargetBug, Color))),

    nb_getval(hexagons, Hexagons),
    list_add(D, Hexagons, NewHexagons),
    nb_setval(hexagons, NewHexagons).

pillbug_select_bug_to_move(Pillbug, TargetBug, Color):-    
    pillbug_get_positions_to_move(Pillbug, TargetBug, Positions),
    nb_getval(hexagons, Hexagons),
    free_hexagons(Hexagons),
    nb_setval(hexagons, []),
    fill_with_gray(Positions, TargetBug, Color).

pillbug_get_positions_to_move(Bug, TargetBug, Positions):-    
    % send(@pce, write_ln, 'XXXXXXXXX'),
    Occupied = [],
    nb_getval(bugs, Bugs),
    get_occupied_positions(Bugs, Occupied, UpdatedOccupied),

    get(Bug, position, Pos),
    get(Pos, x, X_), X is X_ -20,
    get(Pos, y, Y_), Y is Y_ -20,

    

    get(TargetBug, position, TPos),
    get(TPos, x, TX_), TX is TX_ -20,
    get(TPos, y, TY_), TY is TY_ - 20,


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

    
    list_remove([TX,TY], UpdatedOccupied, Graph),
    check_conex(R6, Graph, Positions).




