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
	term_variables(P, Q),
	reduit(_ ,E , P,Q),
	print('======'),
	print(Q),
	print('yes')
.

is_function(F, Name, Arity):- 
%	not(atom(F)),
%	print(F),
%	functor(F, Name, Arity)
	compound(F)
.


regle(E, decompose):-
	not(atom(E)),
	split(E, X, Y),
	is_function(X, N, A),
	is_function(Y, N, A)
.

regle(E, simplify):-
	split(E, _, R),
	not(var(R)),
	not(compound(R))
.

regle(E, rename):-
	split(E, _, R),
	var(R)
.

regle(E, expand):-
	split(E, X, Y),
	var(X),
	not(occur_check(X, Y))
.

regle(E, check):- 
	split(E, X, Y),
	not(X == Y),
	var(X),
	occur_check(X, Y)
.

regle(E, orient):-
	split(E, T, _),
	functor(T, _, _)
.

regle(E, clash):-
	split(E, L, R),
	is_function(L, N, A),
	is_function(R, K, B),
	%or(not(K == N), not(A == B)),
	not(true)	
.

split(E, L, R):-
	arg(1, E, L),
	arg(2, E, R)	
.

%occur_check(V, V):- !. %TODO: Achtung !!
/*occur_check(V, T):-
	%TODO: il se passe quoi si V n'est pas une variable ?
	is_function(T, _, _),
	arg(_,T, Z),
	occur_check(V, Z)
.*/

occur_check(V, T) :-
	is_function(T, _, _),
	term_variables(T, L),
	memberchk(_V, _L)

.

reduit(R, E, P, Q):-
	not(atom(E)),
	functor(E, ?=, _),
	regle(E, R),
	print('=='),
	print(R),
	print(' '),
	print(E),
	apply(R,E,P,Q)
.

apply(simplify, E, P, Q) :-
	delete(P, E, RP),
	split(E, LT, RT),
	LT = RT
.

apply(rename, E, P, Q) :-
	delete(P, E, RP),
	split(E, LT, RT),
	LT = RT
.

apply(expand, E, P, Q) :-
	delete(P, E, RP),
	split(E, LT, RT),
	LT = RT
.

apply(clash, _, _, _):-
	not(true)
.

apply(check, _, _, _) :-
	not(true)
.

apply(orient, E, P, Q) :-
	split(E, L, R),
	atom_concat(R, '?=', Z),
	atom_concat(Z, L, T),
	delete(P, E, TP),
	RP = [T|TP]
	%Idem, P <- TP
.

apply(decompose, E, P, Q):-
	split(E, L, R),
	L =.. [_|TermLeft],
	R =.. [_|TermRight],
	decomp(TermLeft, TermRight, S)
	%TODO Remplacer P par la valeur de S
.

decomp([], [], _).
decomp([X|XTail], [Y|YTail], P) :-
	print(X),
	atom_concat(Z, Yname, W),
	decomp(XTail, YTail, S),
	P=[W|S]
.



