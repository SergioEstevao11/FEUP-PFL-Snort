:- ensure_loaded(display).


next_player(1, NextPlayer):- NextPlayer is 2.

next_player(2, NextPlayer):- NextPlayer is 1.



play_game:-
    getGameConfigs(Size, GameMode),
    initial_state(Size, Board),
    display_game(Board),
    game_cycle(Board, 1, GameMode).

game_cycle(GameState, Player, _):-
    game_over(GameState, Winner),
    Winner \= 0,
    !,
    write('Player '), write(Winner), write(' won!'), nl.

game_cycle(Board, Player, GameMode):-
    choose_move(Board, Player, GameMode, Move),
    move(Player, Board, Move, NewBoard),
    next_player(Player, NextPlayer),
    display_game(Board),
    game_cycle(NewBoard, NewPlayer, GameMode).

