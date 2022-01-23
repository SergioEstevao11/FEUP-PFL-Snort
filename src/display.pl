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
mainMenu :- 
    cls,
    drawMainMenu,
    repeat,
    read(Option),   
    nl,
    (
        Option = 1, drawGameMenu;
        Option = 2, rulesMenu;
        Option = 3, aboutMenu;
        invalidInput
    ).

rulesMenu :-
    cls,nl,
    drawRules,
    repeat,
    read(Option),   
    nl,
    (
        Option = 1, mainMenu;
        invalidInput
    ).
    
aboutMenu :-
    cls,nl,
    drawAbout,
    repeat,
    read(Option),   
    nl,
    (
        Option = 1, mainMenu;
        invalidInput
    ).
    

gameMenu(GameMode) :- 
    cls,
    drawGameMenu,
    repeat,
    read(Option),   
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

choose_move(Board, 2, 1, Move):-
    valid_moves([2, Board], ValidMoves),
    length(ValidMoves, Len),
    random(0, Len, Index),
    nth0(Index, ValidMoves, Move),
    announceBotMove(Move).


announceBotMove([X,Y]) :-
    LetterCode is Y + 65,
    char_code(YMove, LetterCode),
    XMove is X + 1,
    nl,
    write('The bot has played '),write(YMove),write(XMove),write('!'),nl,nl.

choose_move(Board, Player, GameMode, Move):-
    getUserMove(Board, Player, Move).

getUserMove(Board, Player, Move) :- 
    nl,
    write('Its player '),write(Player),write(' turn! Which move would you like to make?'),nl,
    write('Please enter your input with a lowercase letter followed by the number as in a5, for example.'),nl,
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

