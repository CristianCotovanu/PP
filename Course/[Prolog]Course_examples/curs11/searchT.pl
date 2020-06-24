solve(Sol) :- template(Template), fill(Template), reverse(Template, Sol).

fill(S) :- initial_state(S).
fill([Next, S | States]) :- fill([S | States]), arc(S, Next), \+member(Next, States).

%% am abstractizat (vor trebui specificate pentru fiecare problemă particulară)
%%		- initial_state
%%		- template (pe post de stare finală)
%%		- arc
n(5).
template(Template) :- n(N), NSq is N*N, template(NSq, Template).
template(1, [1/1]) :- !.
template(N, [_|T]) :- N1 is N-1, template(N1, T).

initial_state([1/1]).

arc(S, Next) :- n(N), arc(N, S, Next).

%% pt săritura calului
%% 	- stare = poziție în template

arc(N, X1/Y1, X2/Y2) :-
	member((Dx, Dy), [(-2, -1), (-2, 1), (-1, -2), (-1, 2), (1, -2), (1, 2), (2, -1), (2, 1)]),
	X2 is X1 + Dx, X2 > 0, X2 =< N,
	Y2 is Y1 + Dy, Y2 > 0, Y2 =< N.



	
