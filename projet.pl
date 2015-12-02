:- op(200, xfy, [?=]).

print(Term):-
	current_prolog_flag(print_write_option, Options),!,
	write_term(Term, Options)
.
print(Term) :-
    write_term(Term, [ portray(true),
	                   numbervars(true),
					   quoted(true)
					 ])
.

unifie(P):-
	member(E, P),
	%length(E, 1),%pour empêcher une liste vide
	reduit(_ ,E , P, Q),
	print(Q)
.

is_function(F, Name, Arity):- 
	not(atom(F)),
	compound_name_arity(F, Name, Arity)
.


regle(E, decompose):-
	not(atom(E)),
	split(E, X, Y),
	is_function(X, N, A),
	is_function(Y, N, A)
.

regle(E, simplify):-
	split(E, _, R),
	not(var(R))
.

regle(E, rename):-
	split(E, _, R),
	var(R)
.

regle(E, expand):-
	split(E, X, Y),
	not(occur_check(X, Y))
.

regle(E, check):- 
	split(E, X, Y),
	not(X == Y),
	occur_check(X, Y)
.

regle(E, orient):-
	split(E, T, _),
	not(var(T))
.

regle(E, clash):-
	split(E, L, R),
	is_function(L, N, A),
	is_function(R, K, B),
	or(not(K == N), not(A == B))	
.

split(E, L, R):-
	arg(1, E, L),
	arg(2, E, R)	
.

occur_check(V, V):- !. %TODO: Achtung !!
occur_check(V, T):-
	%TODO: il se passe quoi si V n'est pas une variable ?
	is_function(T, _, _),
	arg(_,T, Z),
	occur_check(V, Z)
.

reduit(R, E, P, Q):-
    not(atom(E)),
	functor(E, ?=, _),
	regle(E, R),
	print(R),
	print(' '),
	print(E),
	apply(R,E,P,Q)
.

apply(clash, E, P, Q):-
	not(true) %TODO probablement n'importe quoi
.
apply(decompose, E, P, Q):-
	%TODO
.
apply(R, E, P, Q):-
	print(nope)
.


