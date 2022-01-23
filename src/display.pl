:- use_module(library(lists)).


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

drawRules :-
    write('++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'),nl,
    write('|                            SNORT, is a board game from 1970                                          |'),nl,
    write('|                The name comes from the name of its inventor (Simon NORTon)                           |'),nl,
    write('|            Two players, black and white, take turns dropping pieces onto empty squares               |'),nl,
    write('|    The pieces must be placed on squares that are not orthognoally adjacent to another players piece  |'),nl,
    write('|                   The game ends when there are no more moves are available.                          |'),nl,
    write('|                        The last player to make a move wins the game.                                 |'),nl,
    write('|                                                                                                      |'),nl,
    write('|                                        1 - Go back                                                   |'),nl,
    write('++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'),nl.

drawAbout :-
    write('++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'),nl,
    write('|                            PFL - Programacao Funcional e Logica                                      |'),nl,
    write('|                                                                                                      |'),nl,
    write('|                                          Snort                                                       |'),nl,
    write('|                                                                                                      |'),nl,
    write('|                                Joao Afonso, up201905589                                              |'),nl,
    write('|                               Sergio Estevao, up201905680                                            |'),nl,
    write('|                                                                                                      |'),nl,
    write('|                                        1 - Go back                                                   |'),nl,
    write('++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'),nl.

invalidInput :- 
    write('The option you chose is not available! Please try again:'),nl,fail.

invalidMove :- 
    write('The move you have selected is not valid! Please try again:'),nl,fail.



announceBotMove([X,Y]) :-
    LetterCode is Y + 65,
    char_code(YMove, LetterCode),
    XMove is X + 1,
    nl,
    write('The bot has played '),write(YMove),write(XMove),write('!'),nl,nl.



element_at(X,[X|_],1).
element_at(X,[_|L],K) :- K > 1, K1 is K - 1, element_at(X,L,K1).


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
    Y is Size - Row,
    X is Size - Length,
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

