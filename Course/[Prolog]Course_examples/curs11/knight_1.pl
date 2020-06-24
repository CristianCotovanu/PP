%% a rezolva problema înseamnă a genera un template și apoi a îl instanția complet
% solve_knight(N, Sol) 
solve_knight(N, Sol) :- NSq is N*N, template(NSq, Template), fill_board(N, Template), reverse(Template, Sol).

%% generează un template
% template(N, Template)
template(1, [1/1]) :- !.
template(N, [_|T]) :- N1 is N-1, template(N1, T).

%% instanțierea unui template se face recursiv pe principiul:
%%		- template-ul corespunzător stării inițiale este util
%%		- template-ul corespunzător stării S este util dacă
%%				- se poate ajunge în S dintr-o altă stare Prev 
%%				- template-ul corespunzător lui Prev este util
% fill_board(N, Template)
fill_board(_, [_]).
fill_board(N, [S,Prev|Others]) :- 
	fill_board(N, [Prev|Others]),
	arc(N, Prev, S),
	\+member(S, Others).

%% descrie o tranziție validă între două poziții pe tabla de șah
% arc(N, Prev, Next)
arc(N, X1/Y1, X2/Y2) :- 
	member((Dx,Dy), [(-2,-1), (-2,1), (-1,-2), (-1,2), (1,-2), (1,2), (2,-1), (2,1)]),
	X2 is X1 + Dx, X2 > 0, X2 =< N,
	Y2 is Y1 + Dy, Y2 > 0, Y2 =< N.
	
