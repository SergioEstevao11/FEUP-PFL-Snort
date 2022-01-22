:- ensure_loaded(display).

/*

play_game:-
    getGameConfigs(Size, GameMode),
    initial_state(Size, Board),
    gameCycle(Board, 1, GameMode).

gameCycle(GameState, Player, _):-
    game_over(GameState, Winner),
    Winner \= '0',
    !,
    write('Player '), write(Winner), write(' won!'), nl.

gameCycle(Board, Player, GameMode):-
    choose_move(Board)

*/