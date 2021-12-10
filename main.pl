:- use_module(library(pce)).
:- use_module(library(lists)).
:- use_module('./src/auxiliar_methods', [
    list_add/3,
    list_remove/3,
    list_contains/2,
    list_empty/1,
    list_length/2,
    list_concat/3,
    list_reverse/2,
    is_equal/2,
    true_/0,
    potencia/3,
    list_union/3
    ]).
:- use_module('./src/bug_movement', [
    move_bug_to_position/4
    ]).
:- use_module('./src/bug_summon', [
    new_bug/1
    ]).
:- use_module('./src/queen', [
    queen/2
    ]).
:- use_module('./src/grasshopper', [
    grasshopper/2
    ]).
:- use_module('./src/beetle', [
    beetle/2
    ]).
:- use_module('./src/ant', [
    ant/5
    ]).
:- use_module('./src/spider', [
    spider/8
    ]).
:- use_module('./src/ladybug', [
    ladybug/8
    ]).
:- use_module('./src/mosquito', [
    mosquito/2
    ]).
:- use_module('./src/pillbug', [
    pillbug/3
    ]).
:- use_module('./src/logic_methods', [
    try_add_free_position/6,
    try_jump_in_direction/7,
    get_occupied_positions/3,
    free_hexagons/1,
    check_conex/3,
    dfs/7,
    find_updates/3,
    get_bug_from_position/3,
    check_stack/2,
    get_stack/2
    ]).

:- use_module('./src/rival_ia', [
    move_IA/1
    ]).

:- pce_image_directory('./').


% Loading picture resources

resource(queen, image, image('Queen.xpm')).
resource(mosquito, image, image('Mosquito.xpm')).
resource(spider, image, image('spider1.xpm')).
resource(grasshopper, image, image('grasshopper.xpm')).
resource(ant, image, image('ant1.xpm')).
resource(beetle, image, image('Beetle.xpm')).
resource(ladybug, image, image('ladybug1.xpm')).
resource(pillbug, image, image('pillbug.xpm')).
resource(black_hex, image, image('plantilla negra 1.xpm')).
resource(red_hex, image, image('plantilla roja 1.xpm')).
resource(gray_hex, image, image('plantilla gris 1.xpm')).
resource(gray_cube, image, image('gray cube.xpm')).
resource(black_bg, image, image('Black BG.xpm')).
resource(main_menu, image, image('main.xpm')).
resource(game_over, image, image('game over.xpm')).

% init the program
run():-
    free_all(),
    menu().

% simple mode
run_board(1):-
    nb_setval(mode, 1),
    free_all(),
    nb_setval(vs_IA, 0),
    board().

% plus mode
run_board(2):-
    nb_setval(mode, 2),
    free_all(),
    nb_setval(vs_IA, 0),
    board_plus().

% player vs IA
run_board(3):-
    nb_setval(mode, 3),
    free_all(),
    nb_setval(vs_IA, 1),
    board().

% IA vs IA
run_board(4):-
    nb_setval(mode, 4),
    free_all(),
    nb_setval(vs_IA, 2),
    board().

% display Main Menu
menu():-
    new(@frame, frame('HIVE')),
    new(@menu, window('Menu', size(1200,600))),
    send(@frame, append(@menu)),
    new(Menu, device),

    new(Black, bitmap(resource(black_bg), @on)),
    send(Menu, display, Black),
    new(BGImage, bitmap(resource(main_menu), @on)),
    send(Menu, display, BGImage),
    
    % Mode buttons
    send(Menu, display, button('Close', message(@prolog, free_all)), point(600,550)), 
    send(Menu, display, button('Start local game', message(@prolog, run_board, 1)), point(595,110)),
    send(Menu, display, button('Start local game +', message(@prolog, run_board, 2)), point(590,150)),  
    send(Menu, display, button('Start game vs IA', message(@prolog, run_board, 3)), point(595,190)), 
    send(Menu, display, button('Start simulation IA vs IA', message(@prolog, run_board, 4)), point(575,230)), 
    send(@menu, display, Menu, point(0,0)),
     
    send(@frame, open).

% Classic mode Board
board() :-
    free_all(),
    nb_setval(hexagons, []),
    nb_setval(bugs, []),
    nb_setval(turn, 'red'),  
    nb_setval(turn_count, 0),
    nb_setval(last, -1),

    nb_getval(vs_IA, IA),

    new(@frame, frame('HIVE')),
    new(@board, picture('Board', size(700,600))),    
    (
        % Display PLAY! button in IA vs IA mode
        \+ IA =:= 2;
        send(@board, display, button('PLAY!', message(@prolog, ia_play)), point(10,10))
    ),   

    send(@frame, append(@board)), 

    send(new(@menu, window('Menu', size(500,50))), above(@board)),
    new(Black, bitmap(resource(black_bg), @on)),
    send(@menu, display, Black),
    nb_getval(mode, Mode),

    % Upper menu in board
    send(@menu, display, button('Restart', message(@prolog, run_board, Mode)), point(80,10)),
    send(@menu, display, button('Return to main menu', message(@prolog, run)), point(280,10)),
    send(@menu, display, button('Close', message(@prolog,free_all)), point(540,10)),

    send(new(@left_side, picture), left(@board)),
    send(new(@right_side, picture), right(@board)),

    % Initializing all the bugs
    initialize_bug(@left_side, queen, black_hex, point(10,10), 'queen', 'black', @black_queen),
    
    initialize_bug(@left_side, ant, black_hex, point(10,110), 'ant', 'black', BA1),
    initialize_bug(@left_side, ant, black_hex, point(30,110), 'ant', 'black', BA2),
    initialize_bug(@left_side, ant, black_hex, point(50,110), 'ant', 'black', BA3),

    initialize_bug(@left_side, grasshopper, black_hex, point(10,210), 'grasshopper', 'black', BG1),
    initialize_bug(@left_side, grasshopper, black_hex, point(30,210), 'grasshopper', 'black', BG2),
    initialize_bug(@left_side, grasshopper, black_hex, point(50,210), 'grasshopper', 'black', BG3),

    initialize_bug(@left_side, beetle, black_hex, point(10,310), 'beetle', 'black', @black_beetle_1),
    nb_setval(bb1, []),
    initialize_bug(@left_side, beetle, black_hex, point(10,410), 'beetle', 'black', @black_beetle_2),
    nb_setval(bb2, []),

    initialize_bug(@left_side, spider, black_hex, point(10,510), 'spider', 'black', BS1),
    initialize_bug(@left_side, spider, black_hex, point(30,510), 'spider', 'black', BS2),

    nb_setval(black_bugs, [@black_queen, BA1, BA2, BA3, BG1, BG2, BG3, @black_beetle_1, @black_beetle_2, BS1, BS2]),

    initialize_bug(@right_side, queen, red_hex, point(10,10), 'queen', 'red', @red_queen),
    
    initialize_bug(@right_side, ant, red_hex, point(10,110), 'ant', 'red', RA1),
    initialize_bug(@right_side, ant, red_hex, point(30,110), 'ant', 'red', RA2),
    initialize_bug(@right_side, ant, red_hex, point(50,110), 'ant', 'red', RA3),

    initialize_bug(@right_side, grasshopper, red_hex, point(10,210), 'grasshopper', 'red', RG1),
    initialize_bug(@right_side, grasshopper, red_hex, point(30,210), 'grasshopper', 'red', RG2),
    initialize_bug(@right_side, grasshopper, red_hex, point(50,210), 'grasshopper', 'red', RG3),

    initialize_bug(@right_side, beetle, red_hex, point(10,310), 'beetle', 'red', @red_beetle_1),
    nb_setval(rb1, []),
    initialize_bug(@right_side, beetle, red_hex, point(10,410), 'beetle', 'red', @red_beetle_2),
    nb_setval(rb2, []),

    initialize_bug(@right_side, spider, red_hex, point(10,510), 'spider', 'red', RS1),
    initialize_bug(@right_side, spider, red_hex, point(30,510), 'spider', 'red', RS2),
    nb_setval(bm, []),
    nb_setval(rm, []),

    nb_setval(red_bugs, [@red_queen, RA1, RA2, RA3, RG1, RG2, RG3, @red_beetle_1, @red_beetle_2, RS1, RS2]),

    send(@board, open).

% Expanded Mode Board
board_plus() :-
    free_all(),
    nb_setval(hexagons, []),
    nb_setval(bugs, []),
    nb_setval(turn, 'red'),  
    nb_setval(turn_count, 0),
    nb_setval(last, -1),

    new(@frame, frame('HIVE')),
    new(@board, picture('Board', size(700,600))),    

    send(@frame, append(@board)), 

    send(new(@menu, window('Menu', size(500,50))), above(@board)),
    new(Black, bitmap(resource(black_bg), @on)),
    send(@menu, display, Black),
    nb_getval(mode, Mode),
    send(@menu, display, button('Restart', message(@prolog, run_board, Mode)), point(80,10)),
    send(@menu, display, button('Return to main menu', message(@prolog, run)), point(280,10)),
    send(@menu, display, button('Close', message(@prolog,free_all)), point(540,10)),

    send(new(@left_side, picture), left(@board)),
    send(new(@right_side, picture), right(@board)),

    initialize_bug(@left_side, queen, black_hex, point(10,10), 'queen', 'black', @black_queen),
    
    initialize_bug(@left_side, ant, black_hex, point(10,110), 'ant', 'black', _),
    initialize_bug(@left_side, ant, black_hex, point(30,110), 'ant', 'black', _),
    initialize_bug(@left_side, ant, black_hex, point(50,110), 'ant', 'black', _),

    initialize_bug(@left_side, grasshopper, black_hex, point(10,210), 'grasshopper', 'black', _),
    initialize_bug(@left_side, grasshopper, black_hex, point(30,210), 'grasshopper', 'black', _),
    initialize_bug(@left_side, grasshopper, black_hex, point(50,210), 'grasshopper', 'black', _),

    initialize_bug(@left_side, beetle, black_hex, point(10,310), 'beetle', 'black', @black_beetle_1),
    nb_setval(bb1, []),
    initialize_bug(@left_side, beetle, black_hex, point(10,410), 'beetle', 'black', @black_beetle_2),
    nb_setval(bb2, []),

    initialize_bug(@left_side, spider, black_hex, point(10,510), 'spider', 'black', _),
    initialize_bug(@left_side, spider, black_hex, point(30,510), 'spider', 'black', _),

    % Extra Bugs
    initialize_bug(@left_side, ladybug, black_hex, point(30,610), 'ladybug', 'black', _),

    initialize_bug(@left_side, mosquito, black_hex, point(30,710), 'mosquito', 'black', @black_mosquito),
    nb_setval(bm, []),

    initialize_bug(@left_side, pillbug, black_hex, point(30,810), 'pillbug', 'black', _),

    initialize_bug(@right_side, queen, red_hex, point(10,10), 'queen', 'red', @red_queen),
    
    initialize_bug(@right_side, ant, red_hex, point(10,110), 'ant', 'red', A1),
    initialize_bug(@right_side, ant, red_hex, point(30,110), 'ant', 'red', A2),
    initialize_bug(@right_side, ant, red_hex, point(50,110), 'ant', 'red', A3),

    initialize_bug(@right_side, grasshopper, red_hex, point(10,210), 'grasshopper', 'red', G1),
    initialize_bug(@right_side, grasshopper, red_hex, point(30,210), 'grasshopper', 'red', G2),
    initialize_bug(@right_side, grasshopper, red_hex, point(50,210), 'grasshopper', 'red', G3),

    initialize_bug(@right_side, beetle, red_hex, point(10,310), 'beetle', 'red', @red_beetle_1),
    nb_setval(rb1, []),
    initialize_bug(@right_side, beetle, red_hex, point(10,410), 'beetle', 'red', @red_beetle_2),
    nb_setval(rb2, []),

    initialize_bug(@right_side, spider, red_hex, point(10,510), 'spider', 'red', S1),
    initialize_bug(@right_side, spider, red_hex, point(30,510), 'spider', 'red', S2),

    % Extra Bugs

    initialize_bug(@right_side, ladybug, red_hex, point(30,610), 'ladybug', 'red', _),

    initialize_bug(@right_side, mosquito, red_hex, point(30,710), 'mosquito', 'red', @red_mosquito),
    nb_setval(rm, []),

    initialize_bug(@right_side, pillbug, red_hex, point(30,810), 'pillbug', 'red', _),

    nb_setval(red_bugs, [@red_queen, A1, A2, A3, G1, G2, G3, @red_beetle_1, @red_beetle_2, S1, S2]),

    send(@board, open).


initialize_bug(Window, Image, Hexagon, Pos, BugType, HexColor, OutBug):-
    new(OutBug, device),
    send(OutBug, name, BugType),

    new(Hex, bitmap(resource(Hexagon), @on)),
    send(OutBug, display, Hex, point(-20,-20)),

    new(Bug, bitmap(resource(Image), @on)),
    send(OutBug, display, Bug),    

    send(Bug, recogniser, click_gesture(left, '', single, message(@prolog, select_bug, OutBug, HexColor))),

    send(Window, display, OutBug, Pos).

% called when player clicks a bug
select_bug(Bug, HexColor):-
    (
        Occupied = [],
        nb_getval(bugs, Bugs),
        get_occupied_positions(Bugs, Occupied, UpdatedOccupied),
        nb_getval(turn_count, TurnCount)
    ),

    (
        (
            % Return if clicked wrong color bug
            nb_getval(turn, Turn),
            (\+ (Turn \= HexColor); \+ (Turn \= 'no'))
        );

        % Return if clicked stacked bug
        (check_all_stacks(Bug, _),
        nb_getval(hexagons, Hexagons),
        free_hexagons(Hexagons),
        nb_setval(hexagons, []) );

        % Return if clicked bug recently moved by pillbug 
        (nb_getval(last, Last), is_equal(Last, Bug),
        nb_getval(hexagons, Hexagons),
        free_hexagons(Hexagons),
        nb_setval(hexagons, []) );

        (
            % Return if third black turn and black queen is not on board
            \+(HexColor \= 'black'), \+(TurnCount \= 4), 
            get(@black_queen, position, Pos),
            get(Pos, x, X_), X is X_ -20,
            get(Pos, y, Y_), Y is Y_ - 20,
            \+ list_contains([X,Y],UpdatedOccupied), 
            get(Bug, name, Name), (Name \= 'queen'),
            \+ check_all_stacks(@black_queen, _),
            
            nb_getval(hexagons, Hexagons),
            free_hexagons(Hexagons),
            nb_setval(hexagons, []) 
        );  
        
        (
            % Return if third red turn and red queen is not on board
            \+(HexColor \= 'red'), \+(TurnCount \= 5),
            get(@red_queen, position, Pos),
            get(Pos, x, X_), X is X_ -20,
            get(Pos, y, Y_), Y is Y_ - 20,
            \+ list_contains([X,Y],UpdatedOccupied), 
            get(Bug, name, Name), (Name \= 'queen'),
            \+ check_all_stacks(@red_queen, _),
            
            nb_getval(hexagons, Hexagons),
            free_hexagons(Hexagons),
            nb_setval(hexagons, []) 
        );
        

        (
            nb_getval(hexagons, Hexagons),
            free_hexagons(Hexagons),
            nb_setval(hexagons, []),
            nb_getval(bugs, Bugs),

            get(Bug, position, Pos),
            get(Pos, x, X_), X is X_ -20,
            get(Pos, y, Y_), Y is Y_ - 20,

            (list_empty(Bugs), L = [[300,200]]; % first move

            \+ list_contains([Bug, HexColor], Bugs), new_bug(L); % bug outside board
            
                (
                    get(Bug, name, Name),
                    send(@pce, write_ln, Name),
                    (
                        is_equal(Name, 'queen'), queen(Bug, L);
                        is_equal(Name, 'grasshopper'), grasshopper(Bug, L);
                        is_equal(Name, 'beetle'), beetle(Bug, L);
                        is_equal(Name, 'mosquito'), mosquito(Bug, L);
                        is_equal(Name, 'pillbug'), pillbug(Bug, L, HexColor);
                        is_equal(Name, 'ant'), ant([[X, Y]], X, Y, [], L);
                        is_equal(Name, 'spider'), spider([[X, Y]], X, Y, 3, [], _, [], L);
                        is_equal(Name, 'ladybug'), ladybug([[X, Y]], X, Y, 3, [], _, [], L)
                    )
                )
            ),
            % List L have all the posible moves that the clicked bug can make
            fill_with_gray(L, Bug, HexColor)
        )
    ).

% Check if the given bug is under a bittle or a mosquito
% Return in Out the Beetle or mosquito
check_all_stacks(Bug, Out):-
    nb_getval(bb1, S1),
    nb_getval(bb2, S2),
    nb_getval(rb1, S3),
    nb_getval(rb2, S4),
    nb_getval(bm, S5),
    nb_getval(rm, S6),
    (
        list_contains([Bug, _], S1), Out = @black_beetle_1;
        list_contains([Bug, _], S2), Out = @black_beetle_2;
        list_contains([Bug, _], S3), Out = @red_beetle_1;
        list_contains([Bug, _], S4), Out = @red_beetle_2;
        list_contains([Bug, _], S5), Out = @black_mosquito;
        list_contains([Bug, _], S6), Out = @red_mosquito
    ).

fill_with_gray([], _, _).
fill_with_gray([[X,Y]|T], Bug, HexColor):-
    new(D, device),
    new(Gray_hex, bitmap(resource(gray_hex), @on)),
    new(Gray_cube, bitmap(resource(gray_cube), @on)),
    send(Gray_hex, name, 1),send(Gray_cube, name, 1), 
    send(D, display, Gray_hex, point(0,0)),
    send(D, display, Gray_cube, point(20,20)),
    send(@board, display, D, point(X,Y)),
    send(Gray_cube, recogniser, click_gesture(left, '', single, message(@prolog, move_bug_to_position, Bug, X,Y, HexColor))),

    nb_getval(hexagons, Hexagons),
    list_add(D, Hexagons, NewHexagons),
    nb_setval(hexagons, NewHexagons),    
    fill_with_gray(T, Bug, HexColor).

ia_play():-
    (
        (nb_getval(turn_count, C), C =:= 0, move_bug_to_position(@black_beetle_2, 300, 200, 'black'));

        (nb_getval(turn, Turn), move_IA(Turn))
    ).
    

% Clean @ variables of XPCE
free_all():-
    free(@board),
    free(@frame),
    free(@left_side),
    free(@right_side),
    free(@free_positions),
    free(@gray_hex),
    free(@black_queen),
    free(@black_beetle_1),
    free(@black_beetle_2),
    free(@red_beetle_1),
    free(@red_beetle_2),
    free(@black_mosquito),
    free(@red_mosquito),
    free(@menu),
    free(@game_over),
    free(@red_queen).


