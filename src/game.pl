:- use_module(library(lists)).
:- use_module(library(random)).

/**
 * Initializes the starting Board
 */
initial_state(Size, Board) :-
    length(Row, Size),
    maplist(=(' '), Row),           
    length(Board, Size),
    maplist(=(Row), Board).



/**
 * Gets value in the X & Y position of the Board
 */
matrix(Board, X, Y, Value) :-
    nth0(Y, Board, Row),
    nth0(X, Row, Value).



/**
 * Gets the value on the square above the X & Y position
 * 
 * Returns the value in the X & Y position if there is no square above
 */
adjacentUp(Board, X, Y, Adj):-
    Y = 0,
    matrix(Board, X, Y, Adj).


adjacentUp(Board, X, Y, Adj):-
    Y > 0,
    Up is Y-1,
    matrix(Board, X, Up, Adj).

/**
 * Gets the value on the square below the X & Y position
 * 
 * Returns the value in the X & Y position if there is no square below
 */
adjacentDown(Board, X, Y, Adj):-
    length(Board, Len),
    Y =:= (Len-1),
    matrix(Board, X, Y, Adj).

adjacentDown(Board, X, Y, Adj):-
    length(Board, Len),
    Y < (Len-1),
    Down is Y+1,
    matrix(Board, X, Down, Adj).

/**
 * Gets the value on the square left to the X & Y position
 * 
 * Returns the value in the X & Y position if there is no square to the left
 */
adjacentLeft(Board, X, Y, Adj):-
    X = 0,
    matrix(Board, X, Y, Adj).

adjacentLeft(Board, X, Y, Adj):-
    X > 0,
    Left is X-1,
    matrix(Board, Left, Y, Adj).

/**
 * Gets the value on the square right to the X & Y position
 * 
 * Returns the value in the X & Y position if there is no square to the right
 */
adjacentRight(Board, X, Y, Adj):-
    length(Board, Len),
    X =:= (Len-1),
    matrix(Board, X, Y, Adj).
   

adjacentRight(Board, X, Y, Adj):-
    length(Board, Len),
    X < (Len-1),
    Right is X+1,
    matrix(Board, Right, Y, Adj).



/**
 * Verifies a certain move is allowed
 */
legal_move(Board, 1, X, Y):-
    matrix(Board, X, Y, Value),
    Value = ' ',
    adjacentUp(Board, X, Y, AdjUp), AdjUp \= 'O',
    adjacentDown(Board, X, Y, AdjDown), AdjDown \= 'O',
    adjacentLeft(Board, X, Y, AdjLeft), AdjLeft \= 'O',
    adjacentRight(Board, X, Y, AdjRight), AdjRight \= 'O'.

legal_move(Board, 2, X, Y):-
    matrix(Board, X, Y, Value),
    Value = ' ',
    adjacentUp(Board, X, Y, AdjUp), AdjUp \= 'X',
    adjacentDown(Board, X, Y, AdjDown), AdjDown \= 'X',
    adjacentLeft(Board, X, Y, AdjLeft), AdjLeft \= 'X',
    adjacentRight(Board, X, Y, AdjRight), AdjRight \= 'X'.



/**
 * Increments the X & Y coordiantes, going from left to right and then top to bottom
 */
nextPlay(Board, X, Y, NewX, NewY):-
    length(Board, Len),
    CX is X+1,
    NewY is Y+CX//Len,
    NewX is (CX mod Len).

/**
 * Verifies if a certain move is the bottom-right square (last position of the board)
 */
lastMove(Board, X, Y):-
    length(Board, Len),
    X =:= Len-1,
    Y =:= Len-1.

/**
 * Adds a move to the Valids list if it is legal
 */
addValidMove(Board, Player, X, Y, Valids, NewValids):-
    legal_move(Board, Player, X, Y),
    !,
    append(Valids, [[X,Y]], NewValids).

addValidMove(_, _, _, _, Valids, NewValids):-
    append(Valids, [], NewValids).


/**
 * Auxiliary function of valid_moves that lists all valid moves for a given player and board
 */
list_valid_moves(Board, Player, X, Y, Valids, NewValids):-
    lastMove(Board, X, Y),
    !,
    addValidMove(Board, Player, X, Y, Valids, AddedValids),
    append(AddedValids, [], NewValids).


list_valid_moves(Board, Player, X, Y, Valids, NewValids):-
    addValidMove(Board, Player, X, Y, Valids, AddedValids),
    nextPlay(Board, X, Y, NextX, NextY),
    list_valid_moves(Board, Player, NextX, NextY, AddedValids, NewValids).

/**
 * Lists all the valid moves for a given player and board
 */
valid_moves(Player, Board, ValidMoves):-
    list_valid_moves(Board, Player, 0, 0, [], ValidMoves).



/**
 * Replaces the value in the index X of an array
 */
replace(X, Row, NewValue, NewRow) :-
  nth0(X, Row, _, R),
  nth0(X, NewRow, NewValue, R).

/**
 * Makes a move on the board
 */
move(Board, 1, [X, Y], NewGameState):-
    nth0(Y, Board, OldRow),
    replace(X, OldRow, 'X', NewRow),
    replace(Y, Board, NewRow, NewGameState).

move(Board, 2, [X, Y], NewGameState):-
    nth0(Y, Board, OldRow),
    replace(X, OldRow, 'O', NewRow),
    replace(Y, Board, NewRow, NewGameState).



/**
 * Function that makes a random valid move on the board
 */
botMove(Board, Move):-
    valid_moves(2, Board, ValidMoves),
    length(ValidMoves, Len),
    random(0, Len, Index),
    nth0(Index, ValidMoves, Move).



/**
 * Verifies if a player as own, by checking if the opponent has any valid moves left, returns 0 if no one has won
 */
game_over(GameState, 1, Winner):-
    valid_moves(1, GameState, ValidMoves),
    length(ValidMoves, Len),
    Len =:= 0,
    !,
    Winner is 2.

game_over(GameState, 2, Winner):-
    valid_moves(2, GameState, ValidMoves),
    length(ValidMoves, Len),
    Len =:= 0,
    !,
    Winner is 1.

game_over(_, _,  Winner):-
    Winner is 0.




