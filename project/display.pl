:- use_module(library(lists)).

:- ensure_loaded(game).


charToCoord(Char, Coord):-
    atom_codes(Char, Code),
    UppercaseCode is Code /\ \(32),
    UppercaseCode >= 65, UppercaseCode < 91,
    Coord is UppercaseCode-65.

atomToCoord(Atom, Coord):-
    atom_codes(Atom, Code),
    Coord is Code-49.

cls :- write('\33\[2J').

getGameConfigs(Size,GameMode) :-
    mainMenu,
    gameMenu(GameMode),
    getBoardInput(Size).


drawMainMenu :-
    write('++++++++++++++++++++++++'),nl,
    write('|        Welcome to    |'),nl,
    write('|          Snort       |'),nl,
    write('++++++++++++++++++++++++'),nl,
    write('|     1 - Begin Game   |'),nl,
    write('|     2 - Rules        |'),nl,
    write('|     3 - About Us     |'),nl,
    write('|     4 - Exit Game    |'),nl,
    write('++++++++++++++++++++++++'),nl.

drawGameMenu :-
    write('++++++++++++++++++++++++++++++'),nl,
    write('|     1 - Player vs Player    |'),nl,
    write('|     2 - Player vs Bot       |'),nl,
    write('+++++++++++++++++++++++++++++'),nl.


mainMenu :- 
    cls,
    drawMainMenu,
    repeat,
    read(Option),   
    number(Option),
    nl,
    (
        Option = 1, drawGameMenu, gameMenu(GameMode);
        Option = 2, true;
        invalidInput
    ).

gameMenu(GameMode) :- 
    cls,
    drawGameMenu,
    repeat,
    read(Option),   
    number(Option),
    nl,
    (
        Option = 1;
        Option = 2;
        invalidInput
    ),
    GameMode is Option - 1.

invalidInput :- 
    write('The option you chose is not available! Please try again:'),nl,fail.

invalidMove :- 
    write('The move you have selected is not valid! Please try again:'),nl,fail.

getBoardInput(Size) :- 
    write('Welcome to our game! Please enter the size of the board in which you would like to play (From 4x4 to 9x9): '),nl,
    repeat,
    read(BoardSize),   
    number(BoardSize),
    nl,
    (
        BoardSize >3, BoardSize < 10;
        invalidInput
    ),
    Size is BoardSize.

choose_move(Player, Board, 0, Move):-
    getUserMove(Board, Player, Move),
    !,
    write('Here').

choose_move(2, Board, 1, Move):-
    valid_moves([Player, Board], ValidMoves),
    length(ValidMoves, Len),
    random(0, Len, Index),
    nth0(Index, ValidMoves, Move).

getUserMove(Board, Player, Move) :- 
    write('Its your turn to make a move! Which move would you like to make?'),nl,
    write('Please enter your input with the letter followed by the number as in A5, for example.'),nl,
    repeat,
    read(InputMove),   
    atom_length(InputMove, Length),
    sub_atom(InputMove, 1, 1, _, X),
    sub_atom(InputMove, 0, 1, _, Y),
    atomToCoord(X, XInt),
    charToCoord(Y, YInt),
    nl,
    (
        Length = 2, valid_move(Board,Player,XInt,YInt), append([XInt,YInt], [], Move);
        invalidMove
    ). 

element_at(X,[X|_],1).
element_at(X,[_|L],K) :- K > 1, K1 is K - 1, element_at(X,L,K1).


initial_state(Size, Board) :-
    length(Row, Size),
    maplist(=(' '), Row),           
    length(Board, Size),
    maplist(=(Row), Board).

display_game(Board) :-
    length(Board, Length),
    drawCollumIds(Length,Length),write(' |'),nl,
    write(' '),drawBoardTop(Length),
    write('|'),nl,
	drawBoardBody(Board,Length,Length).

drawBoardBody(Board,Length,Rows) :-
    Rows > 0,
    RowsLeft is Rows - 1,
    LetterId is Length - Rows + 1,
    write(' |'),
    drawRowSepparation(Length),nl,
    element_at(Letter,['A','B','C','D','E','F','G','H','I'],LetterId),
    write(Letter),write('|'),
    drawValue(Board,Length,Rows),nl,
    write(' |'),
    drawRow(Length),nl,
    drawBoardBody(Board,Length,RowsLeft).
drawBoardBody(_,_,0).

drawRowSepparation(Length):-
    Length > 0,
    LengthLeft is Length - 1,
    write('    |'),
    drawRowSepparation(LengthLeft).
drawRowSepparation(0).

drawValue(Board, Length, Row):-
    length(Board,Size),
    Length > 0,
    LengthLeft is Length - 1,
    X is Size - Row,
    Y is Size - Length,
    matrix(Board,X,Y,Value),
    write(' '),write(Value),write('  |'),
    drawValue(Board, LengthLeft, Row).
drawValue(_,0,_).


drawRow(Length):-
    Length > 0,
    LengthLeft is Length - 1,
    write('____|'),
    drawRow(LengthLeft).
drawRow(0).

drawBoardTop(Length) :-
    Length > 0,
    LengthLeft is Length - 1,
    write('|____'),
    drawBoardTop(LengthLeft).
drawBoardTop(0).

drawCollumIds(Length,LengthLeft) :-
    LengthLeft > 0,
    Next is LengthLeft - 1,
    Id is Length - LengthLeft + 1,
    write(' |  '),write(Id),
    drawCollumIds(Length,Next).
drawCollumIds(_,0).

