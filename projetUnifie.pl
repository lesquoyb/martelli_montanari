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


apply_reduce([], []):- true .
apply_reduce([E|P], Q):-
	reduce(E, [E|P], Q)
	%apply_reduce(Qp, Q)
.

unifie([]):- true.
unifie(bottom):- false.
/*unifie([E|P]):-
	write("passage dans unifie, E: "),print(E),nl,
	apply_reduce([E|P], Q),
%	unifie(Q),
	%reduce(E, [E|P], Q),
	print("finito: "), print(Q)
.
*/

unifie([E|P]) :-
	regle(E, R),
	apply(R, E, P, Q),
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
	compound(L)
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

apply(simplify, E, P, Q) :-
	split(E, X, T),
	X = T,
	delete_elem(E, P, Q)
.
apply(rename, E, P, Q):-
	split(E, X, T),
	X = T,
	delete_elem(E, P, Q)
.

/*

merge_function_args(0, _, _,  []):- 
	true,
	! %Achtung 
.
merge_function_args( I, L, R, Q):-
	succ(Index, I),
	merge_function_args(Index, L, R, Rp),
	arg(I, L, ArgL),
	arg(I, R, ArgR),
	union(Rp, [ArgL ?= ArgR], Q)
.	


replace([], _, _, []):- true. 
replace([A|List], X, T, NList):-
	%remplace X par T si X est une variable
	%TODO: tester remplacer plusieurs fois dans la même fonction / niveaux récursion sup
	occur_check(X, A),
	%real_replace(A,X,Y),
	args_in_function(A, Args),
	replace(Args, X, T, Nf),
	replace(List, X, T, R),
	union(Nf, R, NList)%TODO: nooooope, surement un delete
.

replace([_|List], X, T, [T|NList]):-
	replace(List, X, T, NList)
.



apply(rename, E, P, Q) :-
	%delete(P, E, RP),
	split(E, LT, RT),
	LT = RT
.

decomp([], [], _).
decomp([X|XTail], [Y|YTail], P) :-
	print(X),
	atom_concat(Z, Yname, W),
	decomp(XTail, YTail, S),
	P=[W|S]
.
*/


apply(expand, E, P, Q) :-
	split(E, X, T),
	X = T,
	delete_elem(E, P, Q)
.

apply(check, _, _, bottom):- false . %TODO: surement de la merde
apply(orient, E, P, [ R ?= L | Tp ]) :-
	split(E, L, R),
	delete_elem(E, P, Tp)
.
apply(decompose, E, P, S):-
	split(E, L, R),
	L =.. [_|ArgsL],
	R =.. [_|ArgsR],
	unif_list(ArgsL, ArgsR, Res),
	delete_elem(E, P, Pp),
	union(Res, Pp, S)
.
/*
apply(decompose, E, P, Q):-
	split(E, L, R),
	L =.. [_|TermLeft],
	R =.. [_|TermRight],
	decomp(TermLeft, TermRight, S)
	%TODO Remplacer P par la valeur de S
.


*/
apply(clash, _, _, bottom):- false . %TODO: surement n'imp'



delete_elem(_, [], []) :- print('on ne devrait jamais passer par ici dans ce projet'),nl , !.
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
			apply(FirstRule, E, P, Qtmp),
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




