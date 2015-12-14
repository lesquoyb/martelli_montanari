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
unifie([E|P]):-
	%length(E, 1),%pour empêcher une liste vide
	%apply_reduce(P, Q),
	write("passage dans unifie, E: "),print(E),nl,
	apply_reduce([E|P], Q),
	%reduce(E, [E|P], Q),
	print("finito: "), print(Q)
.
/*
unifie(P):-
	member(E, P),
	%length(E, 1),%pour empêcher une liste vide
	term_variables(P, Q),
	reduit(_ ,E , P,Q),
	print('======'),
	print(Q),
	print('yes')
.

*/
regle(E, decompose):-
	not(atom(E)),
	split(E, X, Y),
	compound(X),
	compound(Y),
	compound_name_arity(X, N, A),
	compound_name_arity(Y, N, A)
.

/*
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
*/	
	

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

occur_check(V,V):- true.
occur_check(V, T) :-
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

apply(simplify, E, P, Q) :-
	%delete(P, E, RP),
	split(E, LT, RT),
	LT = RT,
	Q = P
	%TODO: mais bordel non
.
apply(rename, E, P, S):-
	split(E, L, R),
	replace(P, L, R, Pb),
	%delete(P, E, Pp),
	%replace(Pp, L, R, Pt ),
	union(E, Pb, S)
	%TODO: faux ?
.

/*
apply(rename, E, P, Q) :-
	%delete(P, E, RP),
	split(E, LT, RT),
	LT = RT
.
*/

apply(expand, E, P, Q) :-
	%delete(P, E, RP),
	split(E, LT, RT),
	LT = RT,
	Q = P
	%TODO: c'est n'importe quoi ça
.

apply(check, _, _, bottom):- false . %TODO: surement de la merde
apply(orient, E, P, [ R ?= L | Tp ]) :-
	split(E, L, R),
	delete(P, E, Tp)
.
apply(decompose, E, P, S):-
	split(E, L, R),
	compound_name_arity(L, _, Arity),
	merge_function_args(Arity, L, R, Res), 
	delete(P, E, Pp),
	union(Res, Pp, S)
.

apply(clash, _, _, bottom):- false . %TODO: surement n'imp'


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




