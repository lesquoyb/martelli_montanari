:- op(20, xfy, [?=]).

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

choix_premier( [E|P], _, E, R):- %TODO
	regle(E,R),!	
.

unifie([]):- true, !.
unifie(bottom):- false, !.
unifie(P) :-
	write("systeme: "),print(P),nl,
	choix_premier(P, _, E, R),
	write(R),write(": "),print(E),nl,
	reduit(R, E, P, Q),
	unifie(Q)
.


regle(E, decompose):-
	not(atom(E)),
	split(E, X, Y),
	compound(X),
	compound(Y),
	compound_name_arity(X, N, A),
	compound_name_arity(Y, N, A)
.

	

regle(E, simplify):-
	split(E, L, R),
	not(var(R)),
	var(L),
	not(compound(R))%TODO: vérifier que ça colle bien à la définission
.


regle(E, rename):-
	split(E, L, R),
	var(R),
	var(L)
.

regle(E, expand):-
	split(E, X, Y),
	var(X),
	compound(Y),
	not(occur_check(X, Y))
.

regle(E, clash):-
	split(E, L, R),
	compound(L),
	compound(R),
	compound_name_arity(L, Nl, Al),
	compound_name_arity(R, Nr, Ar),
	not( (Nl == Nr, Al == Ar) )
.

regle(E, check):-
	split(E, X, Y),
	not(X == Y),
	var(X),
	occur_check(X, Y)
.

regle(E, orient):-
	split(E, L, R),
	var(R),
	not(var(L))
.

split(E, L, R):-
	arg(1, E, L),
	arg(2, E, R)
.

occur_check(V, T) :-
	var(V),
	compound(T),
	term_variables(T, L),
	occur_check_list(V, L)
.
occur_check_list(_, []):-
	not(true)
.
occur_check_list(V, [C|T]) :-
	occur_check_list(V, T);
	V == C
.

unif_list([], [],[]):- true.
unif_list([L|List1], [R|List2], [L ?= R| Rp]):-
	unif_list(List1, List2, Rp)
.

reduit(simplify, E, P, Q) :-
	split(E, X, T),
	X = T,
	delete_elem(E, P, Q)
.
reduit(rename, E, P, Q):-
	split(E, X, T),
	X = T,
	delete_elem(E, P, Q)
.

reduit(expand, E, P, Q) :-
	split(E, X, T),
	X = T,
	delete_elem(E, P, Q)
.

reduit(check, _, _, bottom):- false . %TODO: surement de la merde
reduit(orient, E, P, [ R ?= L | Tp ]) :-
	split(E, L, R),
	delete_elem(E, P, Tp)
.
reduit(decompose, E, P, S):-
	split(E, L, R),
	L =.. [_|ArgsL],
	R =.. [_|ArgsR],
	unif_list(ArgsL, ArgsR, Res),
	delete_elem(E, P, Pp),
	union(Res, Pp, S)
.
reduit(clash, _, _, bottom):- false . %TODO: surement n'imp'



delete_elem(_, [], []) :- !.%Achtung 
delete_elem(Elem, [Elem|Set], Set):- ! .%Achtung
delete_elem(Elem, [E|Set], [E|R]):-
	delete_elem(Elem, Set, R)
.

apply_rules(_, [], P, P):- true.
apply_rules(E, [FirstRule | ListRules], P, Q):-

	write('=='),
	print(FirstRule),
	write(' '),
	print(E),
	nl,
	(
		regle(E, FirstRule) ->
			print(FirstRule),write(", est appliqué sur: "), print(E),nl,
			reduit(FirstRule, E, P, Qtmp),
			write("une fois appliqué: "),print(Qtmp),nl,
			apply_rules(E, ListRules, Qtmp, Q)
	;
		apply_rules(E, ListRules, P, Q),
		print(FirstRule),write(", ne peut être appliqué sur: "), print(E),nl
	)
.


init_rules_list([rename, simplify, expand, check, orient, decompose, clash]):- true .


reduce(E, P, Q):-
    not(atom(E)),
	functor(E, ?=, _),
	init_rules_list(ListRules),
	apply_rules(E, ListRules, P, Q)
.




