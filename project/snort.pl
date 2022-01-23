:- ensure_loaded(display).


next_player(1, 2).
next_player(2, 1).



play_game:-
    getGameConfigs(Size, GameMode),
    initial_state(Size, Board),
    display_game(Board),
    game_cycle(Board, 1, GameMode).



game_cycle(Board, Player, GameMode):-
    game_over(Board, Winner),
    Winner \= 0,
    !,
    write('Player '), write(Winner), write(' won!'), nl.
    


game_cycle(Board, Player, GameMode):-
    choose_move(Board, Player, GameMode, Move),
    move(Board, Player, Move, NewBoard),
    next_player(Player, NextPlayer),
    display_game(NewBoard),
    game_cycle(NewBoard, NextPlayer, GameMode).

