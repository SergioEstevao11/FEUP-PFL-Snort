matrix(Board, X, Y, Value) :-
    nth0(Y, Board, Row),
    nth0(X, Row, Value).