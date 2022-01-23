
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
    botMove(Board, Move),
    announceBotMove(Move).

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
        Length = 2, legal_move(Board,Player,XInt,YInt), append([XInt,YInt], [], Move);
        invalidMove
    ). 

