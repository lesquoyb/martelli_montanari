/* Definition de l'operateur */
:- op(200, xfy, [?=]).

net(X, Y) :- X == Y.

print(Term) :-
    current_prolog_flag(print_write_options, Options), !,
    write_term(Term, Options).

unifie(X) :- 
%	regle(X, R).
	reduit(R, E, X, []).

reduit(n, E, P, Q) :-
	union(Q, E, P).

reduit(R, E, P, Q) :-
	
	regle(E, R).


regle(X, n) :- % n = la règle de nettoyage
	arg(1, X, Y),
	arg(2, X, Z),
	net(Y, Z).

