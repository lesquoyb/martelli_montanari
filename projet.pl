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

unifie([], R):-true, !.
unifie(P, premier):- unifie(P).
unifie(P, pondere) :-
	write("==== unifie ===="), nl,
	choix_pondere(P,_,E, R),
	write("règle: "),print(R),nl,
	write("E: "), print(E),nl,
	reduit(R, E, P, Q),
	write("Q:"),print(Q),nl,
	unifie(Q, pondere)
.
%TODO: question 3
select_rule([], _, _, _):- false, !.%on a parcouru la liste des règle sans en trouver une qui fonctionne
select_rule( [Next |  MasterList], [], Pbase, P, R, E):-
	select_rule(MasterList, Next, Pbase, P, R, E)
.
select_rule(MasterList, [FirstRule | ListRules], Pbase, [], R, E):-
	select_rule(MasterList, ListRules, Pbase, Pbase, R, E)
.
select_rule( MasterList,[FirstRule | ListRules], Pbase, [Ep|P], R, E):-
	(
		
		write("test: "),print(Ep),write(" avec "),print(FirstRule),nl,
		regle(Ep, FirstRule),
		R = FirstRule,
		E = Ep,	
		!
	;
		select_rule(MasterList, [FirstRule | ListRules], Pbase, P, R, E),!
	)
.

liste_pondere([ [clash, check],
				[rename, simplify],
				[orient, decompose],
				[expand]
			  ]):- 
	true 
.

choix_pondere(P, _, E, R):-
	liste_pondere( [FirstRules | List] ),
	select_rule(List,FirstRules, P, P, R, E)
.
%TODO: proposer d'autres strats(random ?)
%TODO: en fait il faut enlever E de P dans choix


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
	var(L),
	not(var(R)),
	not(compound(R))
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
reduit(check, _, _, bottom):- false . 

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
reduit(clash, _, _, bottom):- false .

delete_elem(_, [], []) :- !.
delete_elem(Elem, [Elem|Set], Set):- ! .
delete_elem(Elem, [E|Set], [E|R]):-
	delete_elem(Elem, Set, R)
.

