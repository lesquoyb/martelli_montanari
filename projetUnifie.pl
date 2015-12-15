:-[projet].
/*
unifie([]):- true.
unifie(bottom):- false.
unifie(P) :-
	choix_premier(P, E, R, _),
	apply(R, E, P, Q),
	unifie(Q),
.
*/
unifie([], R):-true, !.
unifie(P, pondere) :-
	write("==== unifie ===="), nl,
	choix_pondere(P,Q,E,R),
	reduit(R, E, P, Q),
	unifie(Q, S)
.


choix_pondere(P, Q, E, R):-
true	
.
%TODO: proposer d'autres strats
