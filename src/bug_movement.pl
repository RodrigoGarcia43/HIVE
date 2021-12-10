:- module(bug_movement, [
    move_bug_to_position/4
    ]).

move_bug_to_position(Bug, X,Y, HexColor):-
    % update turn and las bug moved
    nb_setval(turn, HexColor),
    nb_setval(last, Bug),

    nb_getval(bugs, Bugs),

    (        
        % Add bug to board list if it wasn't there
        list_contains([Bug, _], Bugs);
        (
            list_add([Bug, HexColor], Bugs, NewBugs),
            nb_setval(bugs, NewBugs)            
        )
    ),    
    
    stack_bug(Bug, X,Y),
    send(@board, display, Bug, point(X,Y)),

    % clean gray hexagons
    nb_getval(hexagons, Hexagons),
    free_hexagons(Hexagons),
    nb_setval(hexagons, []),

    % update turn count
    nb_getval(turn_count, Turn),
    N is Turn + 1,
    nb_setval(turn_count, N),
    
    % game over condition
    check_for_loose(@black_queen),
    check_for_loose(@red_queen),
    
    nb_getval(vs_IA, VS),
    nb_getval(turn, NewTurn),
    
    (
        % check if game is in game over 
        (is_equal(NewTurn, 'no'));

        % move IA if player_vs_IA mode
        (VS =:= 1, is_equal(HexColor, 'black'), move_IA('black'));        

        true_()
    ).



find_bug_at_position(X, Y, [[Bug, Color]| Bugs], OutBug, OutColor):-
    (get(Bug, position, Pos),
    get(Pos, x, X_), TargetX is X_ -20,
    get(Pos, y, Y_), TargetY is Y_ - 20,

    X =:= TargetX, Y =:= TargetY, OutBug = Bug, OutColor = Color);
    find_bug_at_position(X,Y,Bugs, OutBug, OutColor).

display_stacked(Window, [[Bug, Color]|_], X, Y):-
    nb_getval(bugs, Bugs),
    list_add([Bug, Color], Bugs, NewBugs),
    nb_setval(bugs, NewBugs),
    send(Window, display, Bug, point(X,Y)).

remove_from_board(Window, Bug, X, Y):-
    nb_getval(bugs, Bugs),
    list_remove([Bug, _], Bugs, NewBugs),
    nb_setval(bugs, NewBugs),
    send(Window, display, Bug, point(X,Y)).

stack_bug(@black_mosquito, X, Y):-
    get(@black_mosquito, position, Pos),
    get(Pos, x, X_), PrevX is X_ -20,
    get(Pos, y, Y_), PrevY is Y_ - 20,

    nb_getval(bm, Stack),
    list_length(Stack, C),

    nb_getval(bugs, Bugs),
    Occupied = [],
    get_occupied_positions(Bugs, Occupied, UpdatedOccupied),
    (
        (\+ list_contains([X,Y],UpdatedOccupied), nb_setval(bm, []));
        (
            find_bug_at_position(X,Y, Bugs, TargetBug, TargetColor),
            remove_from_board(@left_side, TargetBug, 30, 710),
            nb_setval(bm, [[TargetBug, TargetColor]])
        )
    ),
    
    
    (
        \+ C > 0;
        display_stacked(@board, Stack, PrevX, PrevY)
    ).

stack_bug(@black_beetle_1, X, Y):-
    get(@black_beetle_1, position, Pos),
    get(Pos, x, X_), PrevX is X_ -20,
    get(Pos, y, Y_), PrevY is Y_ - 20,

    nb_getval(bb1, Stack),
    list_length(Stack, C),

    nb_getval(bugs, Bugs),
    Occupied = [],
    get_occupied_positions(Bugs, Occupied, UpdatedOccupied),
    (
        (\+ list_contains([X,Y],UpdatedOccupied), nb_setval(bb1, []));
        (
            find_bug_at_position(X,Y, Bugs, TargetBug, TargetColor),
            remove_from_board(@left_side, TargetBug, 10, 310),
            nb_setval(bb1, [[TargetBug, TargetColor]])
        )
    ),
    
    
    (
        \+ C > 0;
        display_stacked(@board, Stack, PrevX, PrevY)
    ).
    

stack_bug(@black_beetle_2, X, Y):-
    get(@black_beetle_2, position, Pos),
    get(Pos, x, X_), PrevX is X_ -20,
    get(Pos, y, Y_), PrevY is Y_ - 20,

    nb_getval(bb2, Stack),
    list_length(Stack, C),

    nb_getval(bugs, Bugs),
    Occupied = [],
    get_occupied_positions(Bugs, Occupied, UpdatedOccupied),
    (
        (\+ list_contains([X,Y],UpdatedOccupied), nb_setval(bb2, []));
        (
            find_bug_at_position(X,Y, Bugs, TargetBug, TargetColor),
            remove_from_board(@left_side, TargetBug, 10, 410),
            nb_setval(bb2, [[TargetBug, TargetColor]])
        )
    ),
    
    (
        \+ C > 0;
        display_stacked(@board, Stack, PrevX, PrevY)
    ).

stack_bug(@red_mosquito, X, Y):-
    get(@red_mosquito, position, Pos),
    get(Pos, x, X_), PrevX is X_ -20,
    get(Pos, y, Y_), PrevY is Y_ -20,

    nb_getval(rm, Stack),
    list_length(Stack, C),

    nb_getval(bugs, Bugs),
    Occupied = [],
    get_occupied_positions(Bugs, Occupied, UpdatedOccupied),
    (
        (\+ list_contains([X,Y],UpdatedOccupied), nb_setval(rm, []));
        (
            find_bug_at_position(X,Y, Bugs, TargetBug, TargetColor),
            remove_from_board(@right_side, TargetBug, 30, 710),
            nb_setval(rm, [[TargetBug, TargetColor]])
        )
    ),
    
    (
        \+ C > 0;
        display_stacked(@board, Stack, PrevX, PrevY)
    ).

stack_bug(@red_beetle_1, X, Y):-
    get(@red_beetle_1, position, Pos),
    get(Pos, x, X_), PrevX is X_ -20,
    get(Pos, y, Y_), PrevY is Y_ -20,

    nb_getval(rb1, Stack),
    list_length(Stack, C),

    nb_getval(bugs, Bugs),
    Occupied = [],
    get_occupied_positions(Bugs, Occupied, UpdatedOccupied),
    (
        (\+ list_contains([X,Y],UpdatedOccupied), nb_setval(rb1, []));
        (
            find_bug_at_position(X,Y, Bugs, TargetBug, TargetColor),
            remove_from_board(@right_side, TargetBug, 10, 310),
            nb_setval(rb1, [[TargetBug, TargetColor]])
        )
    ),
    
    (
        \+ C > 0;
        display_stacked(@board, Stack, PrevX, PrevY)
    ).
    

stack_bug(@red_beetle_2, X, Y):-
    get(@red_beetle_2, position, Pos),
    get(Pos, x, X_), PrevX is X_ -20,
    get(Pos, y, Y_), PrevY is Y_ - 20,

    nb_getval(rb2, Stack),
    list_length(Stack, C),

    nb_getval(bugs, Bugs),
    Occupied = [],
    get_occupied_positions(Bugs, Occupied, UpdatedOccupied),
    (
        (\+ list_contains([X,Y],UpdatedOccupied), nb_setval(rb2, []));
        (
            find_bug_at_position(X,Y, Bugs, TargetBug, TargetColor),
            remove_from_board(@right_side, TargetBug, 10, 410),
            nb_setval(rb2, [[TargetBug, TargetColor]])
        )
    ),
    
    (
        \+ C > 0;
        display_stacked(@board, Stack, PrevX, PrevY)
    ).
    

stack_bug(_,_,_).    

check_for_loose(Queen):-
    (
    Occupied = [],
    nb_getval(bugs, Bugs),
    get_occupied_positions(Bugs, Occupied, UpdatedOccupied),

    (
        (check_all_stacks(Queen, Other), 
        get(Other, position, Pos),
        get(Pos, x, X_), X is X_ -20,
        get(Pos, y, Y_), Y is Y_ - 20);

        (get(Queen, position, Pos),
        get(Pos, x, X_), X is X_ -20,
        get(Pos, y, Y_), Y is Y_ - 20)
    ),
    

    X0 is X + 70, Y0 is Y + 49, %dawn right
    X1 is X + 70, Y1 is Y - 49, %up right
    X2 is X , Y2 is Y - 98, %up
    X3 is X - 70, Y3 is Y - 49, %up left
    X4 is X - 70, Y4 is Y + 49, %dawn left
    X5 is X , Y5 is Y + 98, %dawn
    
    list_contains([X0,Y0], UpdatedOccupied),
    list_contains([X1,Y1], UpdatedOccupied),
    list_contains([X2,Y2], UpdatedOccupied),
    list_contains([X3,Y3], UpdatedOccupied),
    list_contains([X4,Y4], UpdatedOccupied),
    list_contains([X5,Y5], UpdatedOccupied),

    new(@game_over, window('Game Over', size(400,300))),
    new(GameOver, bitmap(resource(game_over), @on)),
    send(@game_over, display, GameOver, point(0,0)),
    send(@game_over, open),
    nb_setval(turn, 'no')
    );
    true_().