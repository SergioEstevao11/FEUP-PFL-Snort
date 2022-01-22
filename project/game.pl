:- use_module(library(lists)).
:- use_module(library(random)).


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





/*move(1, [[' ', 'X', ' '], [' ', 'X', ' '], ['X', ' ', ' ']], [2,2], NewBoard).*/

replace(X, Row, NewValue, NewRow) :-
  nth0(X, Row, _, R),
  nth0(X, NewRow, NewValue, R).

move(1, Board, [X, Y], NewGameState):-
    nth0(Y, Board, OldRow),
    replace(X, OldRow, 'X', NewRow),
    replace(Y, Board, NewRow, NewGameState).

move(2, Board, [X, Y], NewGameState):-
    nth0(Y, Board, OldRow),
    replace(X, OldRow, 'O', NewRow),
    replace(Y, Board, NewRow, NewGameState).






game_over(GameState, Winner):-
    valid_moves([1, GameState], ValidMoves),
    length(ValidMoves, Len),
    Len =:= 0,
    !,
    Winner is 2.

game_over(GameState, Winner):-
    valid_moves([2, GameState], ValidMoves),
    length(ValidMoves, Len),
    Len =:= 0,
    !,
    Winner is 1.

game_over(_, Winner):-
    Winner is 0.




