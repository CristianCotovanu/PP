solve(Sol) :- initial_state(State), search(State, Sol).

search(State, Sol) :- final_state(State), !, reverse(State, Sol).
search([S | State], Sol) :- arc(S, Next), \+member(Next, State), search([Next, S | State], Sol).

%% am abstractizat (vor trebui specificate pentru fiecare problemă particulară)
%%		- initial_state
%%		- final_state
%%		- arc
n(5).
initial_state([1/1]).
final_state(State) :- n(N), NSq is N*N, length(State, NSq).

arc(S, Next) :- n(N), arc(N, S, Next).
arc(N, X1/Y1, X2/Y2) :-
	member((Dx, Dy), [(-2, -1), (-2, 1), (-1, -2), (-1, 2), (1, -2), (1, 2), (2, -1), (2, 1)]),
	X2 is X1 + Dx, X2 > 0, X2 =< N,
	Y2 is Y1 + Dy, Y2 > 0, Y2 =< N.

	
