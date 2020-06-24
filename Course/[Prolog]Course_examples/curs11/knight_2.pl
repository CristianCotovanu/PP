%% a rezolva problema înseamnă a porni căutarea dintr-o stare inițială
% solve_knight(N, Sol) 
solve_knight(N, Sol) :- fill_board(N, [1/1], RSol), reverse(RSol, Sol).

%% Path = calea urmată de cal până acum
%% Sol = calea completă, va fi egală cu Path când ajungem într-o stare finală
% fill_board(N, Path, Sol)
fill_board(N, Path, Path) :- NSq is N*N, length(Path, NSq), !.
fill_board(N, [Prev|Others], Sol) :- 
	arc(N, Prev, S), 
	\+member(S, Others), 
	fill_board(N, [S,Prev|Others], Sol).

%% descrie o tranziție validă între două poziții pe tabla de șah
% arc(N, Prev, Next)
arc(N, X1/Y1, X2/Y2) :-
	member((Dx, Dy), [(-2, -1), (-2, 1), (-1, -2), (-1, 2), (1, -2), (1, 2), (2, -1), (2, 1)]),
	X2 is X1 + Dx, X2 > 0, X2 =< N,
	Y2 is Y1 + Dy, Y2 > 0, Y2 =< N.