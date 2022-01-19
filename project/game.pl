:- use_module(library(lists)).

getXInput(X):-
    get_code(Code), skip_line,
    UppercaseCode is Code /\ \(32),
    UppercaseCode >= 65, UppercaseCode < 91,
    X is UppercaseCode-65.


getInput(X,Y):-
    getXInput(X),
    read(Y),
    write(X),
    write(Y).


matrix(Board, Y, X, Value) :-
    nth0(Y, Board, Row),
    nth0(X, Row, Value).



adjacentUp(Board, X, Y, Adj):-
    Y > 0,
    !,
    Up is Y-1,
    matrix(Board, Up, X, Adj).

adjacentDown(Board, X, Y, Adj):-
    length(Board, Len),
    Y < (Len-1),
    !,
    Down is Y+1,
    matrix(Board, Down, X, Adj).

adjacentLeft(Board, X, Y, Adj):-
    X > 0,
    !,
    Left is X-1,
    matrix(Board, Y, Left, Adj).

adjacentRight(Board, X, Y, Adj):-
    length(Board, Len),
    X < (Len-1),
    !,
    Right is X+1,
    matrix(Board, Y, Right, Adj).


valid_move(Board, 1, X, Y):-
    adjacentUp(Board, X, Y, AdjUp), AdjUp \= 'O',
    adjacentDown(Board, X, Y, AdjDown), AdjDown \= 'O',
    adjacentLeft(Board, X, Y, AdjLeft), AdjLeft \= 'O',
    adjacentRight(Board, X, Y, AdjRight), AdjRight \= 'O'.

valid_move(Board, 2, X, Y):-
    adjacentUp(Board, X, Y, AdjUp), AdjUp \= 'X',
    adjacentDown(Board, X, Y, AdjDown), AdjDown \= 'X',
    adjacentLeft(Board, X, Y, AdjLeft), AdjLeft \= 'X',
    adjacentRight(Board, X, Y, AdjRight), AdjRight \= 'X'.


/*[[' ', 'X', ' '], [' ', ' ', ' '], [' ', ' ', ' ']],*/

/*list_valid_moves(Size, Board, 1, X, Y, Valids):-*/




/*Player 2 uses O*/
/*list_valid_moves(Board, 2, Valids):-*/



