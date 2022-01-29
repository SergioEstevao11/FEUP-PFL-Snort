:- use_module(library(lists)).

/**
 * Draws the main menu of the game
 */
drawMainMenu :-
    write('______________________________________________'),nl,
    write('|       _____                      _          |'),nl,
    write('|      / ____|                    | |         |'),nl,
    write('|     | (___   _ __    ___   _ __ | |_        |'),nl,
    write('|      \\___ \\ | _  \\  / _ \\ |  __|| __|       |'),nl,
    write('|      ____) || | | || (_) || |   | |_        |'),nl,
    write('|     |_____/ |_| |_| \\___/ |_|    \\__|       |'),nl,
    write('|                                             |'),nl,    
    write('|                                             |'),nl,
    write('|               1 - Start Game                |'),nl,    
    write('|               2 - Rules                     |'),nl,
    write('|               3 - About Us                  |'),nl,
    write('|               4 - Exit Game                 |'),nl,
    write('|_____________________________________________|'),nl.



/**
 * Draws the game menu of the game
 */

drawGameMenu :-
        write('______________________________________________'),nl,
        write('|       _____                                 |'),nl,
        write('|      / ____|                                |'),nl,
        write('|     | |  __   __ _  _ __ ___    ___         |'),nl,
        write('|     | | |_ | / _  || _  _   \\  / _ \\        |'),nl,
        write('|     | |__| || (_| || | | | | ||  __/        |'),nl,
        write('|      \\_____| \\__,_||_| |_| |_| \\___|        |'),nl,
        write('|                                             |'),nl,    
        write('|                                             |'),nl,
        write('|            1 - Player vs Player             |'),nl,
        write('|            2 - Player vs Bot                |'),nl,
        write('|_____________________________________________|'),nl.

/**
 * Draws the rules of the game
 */

drawRules :-
    write(' ______________________________________________________________________________________________________'),nl,
    write('|                              _____         _                                                         |'),nl,
    write('|                             |  __ \\       | |                                                        |'),nl,
    write('|                             | |__) |_   _ | |  ___  ___                                              |'),nl,
    write('|                             |  _  /| | | || | / _ \\/ __|                                             |'),nl,
    write('|                             | | \\ \\| |_| || ||  __/\\__ \\                                             |'),nl,
    write('|                             |_|  \\_\\\\__,_||_| \\___||___/                                             |'),nl,
    write('|                                                                                                      |'),nl,    
    write('|                                                                                                      |'),nl,
    write('|                                                                                                      |'),nl,    
    write('|                            SNORT, is a board game from 1970                                          |'),nl,
    write('|                The name comes from the name of its inventor (Simon NORTon)                           |'),nl,
    write('|            Two players, black and white, take turns dropping pieces onto empty squares               |'),nl,
    write('|    The pieces must be placed on squares that are not orthognoally adjacent to another players piece  |'),nl,
    write('|                   The game ends when there are no more moves are available.                          |'),nl,
    write('|                        The last player to make a move wins the game.                                 |'),nl,
    write('|                                                                                                      |'),nl,
    write('|                                        1 - Go back                                                   |'),nl,
    write('|______________________________________________________________________________________________________|'),nl.

/**
 * Draws the 'about us' section of the game
 */

drawAbout :-
    write(' ______________________________________________________________________________________________________'),nl,
    write('|                                        _                    _                                        |'),nl,
    write('|                                 /\\    | |                  | |                                       |'),nl,
    write('|                                /  \\   | |__    ___   _   _ | |_                                      |'),nl,
    write('|                               / /\\ \\  |  _ \\  / _ \\ | | | || __|                                     |'),nl,
    write('|                              / ____ \\ | |_) || (_) || |_| || |_                                      |'),nl,
    write('|                             /_/    \\_\\|_.__/  \\___/  \\__,_| \\__|                                     |'),nl,
    write('|                                                                                                      |'),nl,
    write('|                                                                                                      |'),nl,    
    write('|                                                                                                      |'),nl,
    write('|                            PFL - Programacao Funcional e Logica                                      |'),nl,
    write('|                                                                                                      |'),nl,
    write('|                                          Snort                                                       |'),nl,
    write('|                                                                                                      |'),nl,
    write('|                                Joao Afonso, up201905589                                              |'),nl,
    write('|                               Sergio Estevao, up201905680                                            |'),nl,
    write('|                                                                                                      |'),nl,
    write('|                                        1 - Go back                                                   |'),nl,
    write('|______________________________________________________________________________________________________|'),nl.


/**
 * Handles invalid inputs, sending a fail to activate the repeat and allow user to try again
 */

invalidInput :- 
    write('The option you chose is not available! Please try again:'),nl,fail.

/**
 * Handles invalid move provided by the user, sending a fail to activate the repeat and allow user to try again
 */

invalidMove :- 
    write('The move you have selected is not valid! Please try again:'),nl,fail.



/**
 * Announces when the bot has played a move, showing which move the bot has played
 */

announceBotMove([X,Y]) :-
    LetterCode is Y + 65,
    char_code(YMove, LetterCode),
    XMove is X + 1,
    nl,
    write('The bot has played '),write(YMove),write(XMove),write('!'),nl,nl.


/**
 * Gets the element at  a specific position of an array
 */

element_at(X,[X|_],1).
element_at(X,[_|L],K) :- K > 1, K1 is K - 1, element_at(X,L,K1).

/**
 * Displays the board of the game
 */

display_game(Board) :-
    length(Board, Length),
    drawCollumIds(Length,Length),write(' |'),nl,
    write(' '),drawBoardTop(Length),
    write('|'),nl,
	drawBoardBody(Board,Length,Length).

/**
 * Main function that draws the body of the board
 */


drawBoardBody(Board,Length,Rows) :-
    Rows > 0,
    RowsLeft is Rows - 1,
    LetterId is Length - Rows + 1,
    write(' |'),
    drawRowSepparation(Length),nl,
    element_at(Letter,['a','b','c','d','e','f','g','h','i'],LetterId),
    write(Letter),write('|'),
    drawValue(Board,Length,Rows),nl,
    write(' |'),
    drawRow(Length),nl,
    drawBoardBody(Board,Length,RowsLeft).
drawBoardBody(_,_,0).

/**
 * Function that draws a separation between each row in order for the board to be more pleasing to the eyes
 */

drawRowSepparation(Length):-
    Length > 0,
    LengthLeft is Length - 1,
    write('    |'),
    drawRowSepparation(LengthLeft).
drawRowSepparation(0).


/**
 * Function that draws an X or O on each empty square depending on the state of the game
 */

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


/**
 * Draws the main separation between each row
 */

drawRow(Length):-
    Length > 0,
    LengthLeft is Length - 1,
    write('____|'),
    drawRow(LengthLeft).
drawRow(0).

/**
 * Draws the top limit of the board 
 */

drawBoardTop(Length) :-
    Length > 0,
    LengthLeft is Length - 1,
    write('|____'),
    drawBoardTop(LengthLeft).
drawBoardTop(0).

/**
 * Draws the ids of each collumn 
 */
drawCollumIds(Length,LengthLeft) :-
    LengthLeft > 0,
    Next is LengthLeft - 1,
    Id is Length - LengthLeft + 1,
    write(' |  '),write(Id),
    drawCollumIds(Length,Next).
drawCollumIds(_,0).

