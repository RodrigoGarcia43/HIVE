:- module(rival_ia, [
    move_IA/1
    ]).

:- use_module('./src/queen', [
    queen_make_decision/3,
    queen_obtain_posible_moves/2
    ]).

:- use_module('./src/ant', [
    ant_make_decision/3,
    ant_obtain_posible_moves/2
    ]).

:- use_module('./src/beetle', [
    beetle_make_decision/3,
    beetle_obtain_posible_moves/2
    ]).

:- use_module('./src/grasshopper', [
    grasshopper_make_decision/3,
    grasshopper_obtain_posible_moves/2
    ]).

:- use_module('./src/spider', [
    spider_make_decision/3,
    spider_obtain_posible_moves/2
    ]).




move_IA('black'):-
    send(@pce, write_ln, 'IA moving'),
    nb_getval(red_bugs, [Q, A1, A2, A3, G1, G2, G3, B1, B2, S1, S2]),
    
    queen_make_decision(Q, [QX, QY], ScoreQ),
    
    ant_make_decision(A1, [A1X, A1Y], ScoreA1),
    ant_make_decision(A2, [A2X, A2Y], ScoreA2),
    ant_make_decision(A3, [A3X, A3Y], ScoreA3),

    grasshopper_make_decision(G1, [G1X, G1Y], ScoreG1),
    grasshopper_make_decision(G2, [G2X, G2Y], ScoreG2),
    grasshopper_make_decision(G3, [G3X, G3Y], ScoreG3),

    beetle_make_decision(B1, [B1X, B1Y], ScoreB1),
    beetle_make_decision(B2, [B2X, B2Y], ScoreB2),

    spider_make_decision(S1, [S1X, S1Y], ScoreS1),
    spider_make_decision(S2, [S2X, S2Y], ScoreS2),


    get_max([[Q,QX,QY,ScoreQ], [A1,A1X,A1Y,ScoreA1], [A2,A2X,A2Y,ScoreA2], [A3,A3X,A3Y,ScoreA3], [G1,G1X,G1Y,ScoreG1], [G2,G2X,G2Y,ScoreG2], [G3,G3X,G3Y,ScoreG3], [B1,B1X,B1Y,ScoreB1], [B2,B2X,B2Y,ScoreB2], [S1,S1X,S1Y,ScoreS1], [S2,S2X,S2Y,ScoreS2]], ScoreQ, Q, Score, [B,X,Y]),
    
    send(@pce, write_ln, Score),

    get(B, name, Name),
    ia_fill_with_gray(B, Name, 'red'),
    nb_getval(hexagons, Hex),
    free_hexagons(Hex),
    nb_setval(hexagons, []),
    
    move_bug_to_position(B, X, Y, 'red').

move_IA('red'):-
    
    send(@pce, write_ln, 'IA moving'),
    nb_getval(black_bugs, [Q, A1, A2, A3, G1, G2, G3, B1, B2, S1, S2]),
    
    queen_make_decision(Q, [QX, QY], ScoreQ),
    
    ant_make_decision(A1, [A1X, A1Y], ScoreA1),
    ant_make_decision(A2, [A2X, A2Y], ScoreA2),
    ant_make_decision(A3, [A3X, A3Y], ScoreA3),

    grasshopper_make_decision(G1, [G1X, G1Y], ScoreG1),
    grasshopper_make_decision(G2, [G2X, G2Y], ScoreG2),
    grasshopper_make_decision(G3, [G3X, G3Y], ScoreG3),

    beetle_make_decision(B1, [B1X, B1Y], ScoreB1),
    beetle_make_decision(B2, [B2X, B2Y], ScoreB2),

    spider_make_decision(S1, [S1X, S1Y], ScoreS1),
    spider_make_decision(S2, [S2X, S2Y], ScoreS2),


    get_max([[Q,QX,QY,ScoreQ], [A1,A1X,A1Y,ScoreA1], [A2,A2X,A2Y,ScoreA2], [A3,A3X,A3Y,ScoreA3], [G1,G1X,G1Y,ScoreG1], [G2,G2X,G2Y,ScoreG2], [G3,G3X,G3Y,ScoreG3], [B1,B1X,B1Y,ScoreB1], [B2,B2X,B2Y,ScoreB2], [S1,S1X,S1Y,ScoreS1], [S2,S2X,S2Y,ScoreS2]], ScoreQ, Q, Score, [B,X,Y]),
    
    send(@pce, write_ln, Score),

    get(B, name, Name),
    ia_fill_with_gray(B, Name, 'black'),
    nb_getval(hexagons, Hex),
    free_hexagons(Hex),
    nb_setval(hexagons, []),
    
    move_bug_to_position(B, X, Y, 'black').

get_max([], M,B,M,B).
get_max([[B,X,Y, Score]|Pos], PrevMax, PrevBest, Max, Best):-
    (Score>=PrevMax, get_max(Pos,Score,[B,X,Y],Max,Best));
    get_max(Pos,PrevMax,PrevBest,Max,Best).

ia_fill_with_gray(Bug, 'queen', Color):-
    queen_obtain_posible_moves(Bug, Positions),
    fill_with_gray(Positions, Bug, Color),
    % flush_output,
    sleep(1).

ia_fill_with_gray(Bug, 'ant', Color):-
    ant_obtain_posible_moves(Bug, Positions),
    fill_with_gray(Positions, Bug, Color),
    % flush_output,
    sleep(1).

ia_fill_with_gray(Bug, 'beetle', Color):-
    beetle_obtain_posible_moves(Bug, Positions),
    fill_with_gray(Positions, Bug, Color),
    % flush_output,
    sleep(1).

ia_fill_with_gray(Bug, 'grasshopper', Color):-
    grasshopper_obtain_posible_moves(Bug, Positions),
    fill_with_gray(Positions, Bug, Color),
    % flush_output,
    sleep(1).

ia_fill_with_gray(Bug, 'spider', Color):-
    spider_obtain_posible_moves(Bug, Positions),
    fill_with_gray(Positions, Bug, Color),
    % flush_output,
    sleep(1).

