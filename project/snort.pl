show_board([]).
show_board([H|T]) :- write(H), nl, print_matrix(T).

read_input(X, Y):-
repeat,
write('Player 1'),
read(X-Y),
X = hello,
!.
X<

%game(..., Board) :- repeat, showBoard(Board), getInput(Board, Turn, X, Y), makeMove(Board, newBoard), checkGameEnd(newBoard, Turn), !, getWinner(Turn).1

/*TO-DO

GenerateBoard(X, Y, Board)

ShowBoard(Board)

getInput(Board, Turn, X, Y)

legalMove(Board, Turn, X, Y)

makeMove(Board, Turn, X, Y, newBoard)

canMakeMove(Board, Turn)

checkGameEnd(newBoard, Turn)

game(Board, Turn)

*/
 1 2 3 4 5 
A
B
C
D


/*
adjacentUp(Board, X, Y, Adj):-
    Y < 0,
    !,
    matrix(Board, )

adjacentDown(Board, X, Y, Adj):-

adjacentLeft(Board, X, Y, Adj)

adjacentRight(Board, X, Y, Adj)

valid_move(Board, 1, X, Y):-
    matrix(Board, X)

list_valid_moves(Board, 1, X, Y, Valids):-



list_valid_moves(Board, 2, Valids):-

*/