:- use_module(library(lists)).

getXInput(X):-
    get_code(Code), skip_line,
    UppercaseCode is Code /\ \(32),
    UppercaseCode >= 65, UppercaseCode < 91,
    X is UppercaseCode-65.


getInput(X,Y):-
    repeat,
    getXInput(X),
    read(Y),
    write(X),
    write(Y).


matrix(Board, X, Y, Value) :-
    nth0(Y, Board, Row),
    nth0(X, Row, Value).




adjacentUp(Board, X, Y, Adj):-
    Y = 0,
    matrix(Board, X, Y, Adj).

adjacentUp(Board, X, Y, Adj):-
    Y > 0,
    Up is Y-1,
    matrix(Board, X, Up, Adj).


adjacentDown(Board, X, Y, Adj):-
    length(Board, Len),
    Y =:= (Len-1),
    matrix(Board, X, Y, Adj).

adjacentDown(Board, X, Y, Adj):-
    length(Board, Len),
    Y < (Len-1),
    Down is Y+1,
    matrix(Board, X, Down, Adj).


adjacentLeft(Board, X, Y, Adj):-
    X = 0,
    matrix(Board, X, Y, Adj).

adjacentLeft(Board, X, Y, Adj):-
    X > 0,
    Left is X-1,
    matrix(Board, Left, Y, Adj).



adjacentRight(Board, X, Y, Adj):-
    length(Board, Len),
    X =:= (Len-1),
    matrix(Board, X, Y, Adj).
   

adjacentRight(Board, X, Y, Adj):-
    length(Board, Len),
    X < (Len-1),
    Right is X+1,
    matrix(Board, Right, Y, Adj).




valid_move(Board, 1, X, Y):-
    matrix(Board, X, Y, Value),
    Value = ' ',
    adjacentUp(Board, X, Y, AdjUp), AdjUp \= 'O', !,
    adjacentDown(Board, X, Y, AdjDown), AdjDown \= 'O', !,
    adjacentLeft(Board, X, Y, AdjLeft), AdjLeft \= 'O', !,
    adjacentRight(Board, X, Y, AdjRight), AdjRight \= 'O'.

valid_move(Board, 2, X, Y):-
    matrix(Board, X, Y, Value),
    Value = ' ', !,
    adjacentUp(Board, X, Y, AdjUp), AdjUp \= 'X', !,
    adjacentDown(Board, X, Y, AdjDown), AdjDown \= 'X', !,
    adjacentLeft(Board, X, Y, AdjLeft), AdjLeft \= 'X', !,
    adjacentRight(Board, X, Y, AdjRight), AdjRight \= 'X'.




/*[[' ', 'X', ' '], [' ', ' ', ' '], [' ', ' ', ' ']],*/

nextPlay(Board, X, Y, NewX, NewY):-
    length(Board, Len),
    CX is X+1,
    NewY is Y+CX//Len,
    NewX is (CX mod Len).
    
lastMove(Board, X, Y):-
    length(Board, Len),
    X =:= Len-1,
    Y =:= Len-1.

addValidMove(Board, Player, X, Y, Valids, NewValids):-
    valid_move(Board, Player, X, Y),
    !,
    append(Valids, [[X,Y]], NewValids).

addValidMove(_, _, _, _, Valids, NewValids):-
    append(Valids, [], NewValids).

    
/*moves vão ser guardados da seguinte forma: [[1,1], [2,1], [4,1]] -> [[X,Y]]*/

list_valid_moves(Board, Player, X, Y, Valids, NewValids):-
    lastMove(Board, X, Y),
    !,
    addValidMove(Board, Player, X, Y, Valids, AddedValids),
    append(AddedValids, [], NewValids).


list_valid_moves(Board, Player, X, Y, Valids, NewValids):-
    addValidMove(Board, Player, X, Y, Valids, AddedValids),
    nextPlay(Board, X, Y, NextX, NextY),
    list_valid_moves(Board, Player, NextX, NextY, AddedValids, NewValids).


/* valid_moves(+GameState, -ValidMoves) => valid_moves([2, [[' ', 'X', ' '], [' ', 'X', ' '], ['X', ' ', ' ']]], Vals).*/
valid_moves([Player, Board], ValidMoves):-
    list_valid_moves(Board, Player, 0, 0, [], ValidMoves).
/*
play_game:-
    getGameConfigs(Size, GameMode),
    initial_state(Size, Board),
    gameCycle(Board, 1, GameMode).

gameCycle(Board, Player, _):-
    game_over([Player, Board], Winner),
    Winner \= '0',
    !,
    write('Player '), write(Winner), write(' won!'), nl.

gameCycle(Board, Player, GameMode):-
    choose_move(Board)
*/




