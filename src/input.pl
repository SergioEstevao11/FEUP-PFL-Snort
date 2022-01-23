/**
 * Converts a char to a coordinate A -> 0, for example
 */
charToCoord(Char, Coord):-
    atom_codes(Char, Code),
    UppercaseCode is Code /\ \(32),
    UppercaseCode >= 65, UppercaseCode < 91,
    Coord is UppercaseCode-65.


/**
 * Converts a char to a coordinate A -> 0, for example
 */
atomToCoord(Atom, Coord):-
    atom_codes(Atom, Code),
    Coord is Code-49.

/**
 * Clears the terminal screen 
 */
cls :- write('\33\[2J').

/**
 * Function that handles the beginning of the game 
 */
getGameConfigs(Size,GameMode) :-
    mainMenu(Size,GameMode,End),
    End =:= 1,
    !,fail.

/**
 * Function that handles the main menu 
 */
mainMenu(Size,GameMode,End) :- 
    cls,
    drawMainMenu,
    repeat,
    read(Option),   
    write('BEM VINDO AO JOGO CERWEGWER'),nl,
    write(Option),
    nl,
    (
        Option = 1, gameMenu(Size,GameMode);
        Option = 2, rulesMenu;
        Option = 3, aboutMenu;
        Option = 4, End is 1, true;
        invalidInput
    ).

/**
 * Function that handles the rules menu 
 */
rulesMenu :-
    cls,nl,
    drawRules,
    repeat,
    read(Option),   
    nl,
    (
        Option = 1, mainMenu(_);
        invalidInput
    ).
    
/**
 * Function that handles the 'about us' menu 
 */
aboutMenu :-
    cls,nl,
    drawAbout,
    repeat,
    read(Option),   
    nl,
    (
        Option = 1, mainMenu(_);
        invalidInput
    ).
    
/**
 * Function that handles the game option menu 
 */
gameMenu(Size,GameMode) :- 
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
    GameMode is Option - 1,
    getBoardInput(Size).

/**
 * Function that gets the user's input for the size of the board
 */
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




/**
 * Function that handles the choice of a move
 */

choose_move(Board, 2, 1, Move):-
    botMove(Board, Move),
    announceBotMove(Move).

choose_move(Board, Player, GameMode, Move):-
    getUserMove(Board, Player, Move).

/**
 * Function that asks the user's which move he would like to play and handles it
 */

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

