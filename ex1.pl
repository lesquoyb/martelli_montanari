:- op(200, xfy, [?=]).

print(Term):-
	current_prolog_flag(print_write_option, Options),!,
	write_term(Term, Options)
.

unifie(P):-
	reduit(_ , _ , P, Q),
	print(Q)	
.

decompose(E):-
	compound_name_arity(E,?=, _), %ma version de prolog lève une erreur avec functor pour les fonctions d'ari
	arg(1,E, X),
	arg(2,E, Y),
	compound_name_arity(X, Z, A),
	compound_name_arity(Y, Z, A)
.

decompose(E, P, Q):-
	%et avec plusieurs niveaux de décomposition à faire ?
	%print(E),
	union(E, K, P),
	print(K),
	arg(1, E, I),
	arg(2, E, J),
	arg(N, I, L),
	arg(N, J, O),

	union([L ?= I], K,  Q) %ne devrait pas marcher, mais un truc dans le genre
.

regle(E, decompose):-
	decompose(E)
.

occur_check(V, V):- !.%TODO: Achtung !!
occur_check(V, T):-
	%TODO: il se passe quoi si V n'est pas une variable ?
	compound_name_arity(T, _ , _),
	arg(_,T, Z),
	occur_check(V, Z)
.

reduit(R, E, P, Q):-
	union(P, E, Q),
	regle(E, R),
	R(E, P, Q)
.


