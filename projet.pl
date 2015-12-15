:- op(20,xfy,?=).

% Prédicats d'affichage fournis

% set_echo: ce prédicat active l'affichage par le prédicat echo
set_echo :- assert(echo_on).

% clr_echo: ce prédicat inhibe l'affichage par le prédicat echo
clr_echo :- retractall(echo_on).

% echo(T): si le flag echo_on est positionné, echo(T) affiche le terme T
%          sinon, echo(T) réussit simplement en ne faisant rien.

echo(T) :- echo_on, !, write(T).
echo(_). 

select_strat(premier, P, E, R):-
	choix_premier(P, _, E, R)
.

select_strat(pondere, P, E, R):-
	choix_pondere(P, _, E, R)
.

select_strat(aleatoire, P, E, R):-
	choix_aleatoire(P, _, E, R)
.
select_strat(dernier, P, E, R):-
	choix_dernier(P, _, E, R)
.
apply_strat([], _):- true, !.
apply_strat(bottom, _):- false, !.
apply_strat(P, Strat):-
	echo("system: "),echo(P),echo("\n"),
	select_strat(Strat, P, E, R),
	echo(R),echo(": "),echo(E),echo("\n"),
	reduit(R, E, P, Q),
	apply_strat(Q, Strat), !
.

unifie(P, S):-
	clr_echo,
	apply_strat(P, S)
.

trace_unif(P,S):-
	set_echo,
	(
		apply_strat(P, S), echo("Yes"),!
	;	
		echo("No")
	)
.

select_rule([], _, _, _):- false, !.%on a parcouru la liste des règle sans en trouver une qui fonctionne
select_rule( [Next |  MasterList], [], Pbase, P, R, E):-
	select_rule(MasterList, Next, Pbase, P, R, E)
.
select_rule(MasterList, [ _ | ListRules], Pbase, [], R, E):-
	select_rule(MasterList, ListRules, Pbase, Pbase, R, E)
.
select_rule( MasterList,[FirstRule | ListRules], Pbase, [Ep|P], R, E):-
	(
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
choix_premier( [E|_], _, E, R):- %TODO
	regle(E,R),!	
.

choix_pondere(P, _, E, R):-
	liste_pondere( [FirstRules | List] ),
	select_rule(List,FirstRules, P, P, R, E)
.

choix_aleatoire(P, _, E, R):-
	random_member(E, P),
	regle(E, R),
	!
.
choix_dernier(P, _, E, R):-
	reverse(P, [E|_]),
	regle(E, R),
	!
.

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

