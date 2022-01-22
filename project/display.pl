:- use_module(library(lists)).


cls :- write('\33\[2J').

drawMainMenu :-
    write('        Welcome to      '),nl,
    write('          Snort         '),nl,
    write('++++++++++++++++++++++++'),nl,
    write('     1 - Begin Game     '),nl,
    write('     2 - Rules          '),nl,
    write('     3 - About Us       '),nl,
    write('     4 - ELengthit Game      '),nl,
    write('++++++++++++++++++++++++'),nl.

startGame :- 
    cls,
    drawMainMenu,
    repeat,
    read(Option),   
    number(Option),
    nl,
    (
        Option = 1, getUserInput, true  ;
        Option = 4, true;
        invalidInput
    ).

invalidInput :- 
    write('The option you chose is not available! Please try again:'),nl,fail.

getUserInput :- 
    write('Welcome to our game! Please enter the size of the board in which you would like to play (From 4x4 to 9x9): '),nl,
    repeat,
    read(Size),   
    number(Size),
    nl,
    (
        Size >3, Size < 10, generateBoard(Size,Board), drawBoard(Board,Size);
        invalidInput
    ).

element_at(X,[X|_],1).
element_at(X,[_|L],K) :- K > 1, K1 is K - 1, element_at(X,L,K1).


generateBoard(Length, Board) :-
    length(Row, Length),
    maplist(=([]), Row),        
    length(Board, Length),
    maplist(=(Row), Board).

drawBoard(Board,Length) :-
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
    drawRowSepparation(Length),nl,
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


letters(['A','B','C','D','E','F','G','H','I']).