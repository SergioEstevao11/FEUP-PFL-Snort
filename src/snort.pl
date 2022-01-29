:- ensure_loaded(display).
:- ensure_loaded(game).
:- ensure_loaded(input).


/**
 * Switches game turns
 */
next_player(1, 2).
next_player(2, 1).

/**
 * Randomizes the starting player
 */
coinToss(Player) :- random(1,3, Player).



/**
 * Starts the Game
 */
play :-
    getGameConfigs(Size, GameMode),
    initial_state(Size, Board),
    coinToss(Player),
    display_game(Board),
    game_cycle(Board, Player, GameMode).



/**
 * Game's main loop, stops when a game_over is detected
 */

game_cycle(Board, Player, _GameMode):-
    game_over(Board, Player, Winner),
    Winner \= 0,
    !,
    write('Player '), write(Winner), write(' won!'), nl.
    

game_cycle(Board, Player, GameMode):-
    choose_move(Board, Player, GameMode, Move),
    move(Board, Player, Move, NewBoard),
    next_player(Player, NextPlayer),
    display_game(NewBoard),
    game_cycle(NewBoard, NextPlayer, GameMode).

