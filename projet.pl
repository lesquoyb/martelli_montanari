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


anti_intersection([], S2, S2):-
	true
.
anti_intersection([E|S1], S2, Q):-
	member(E, S2),
	anti_intersection(S1, S2, Q)
.
anti_intersection([E|S1], S2 , Q):-
	not(member(E, S2)),
	anti_intersection(S1, S2, Qp),
	union(E, Qp, Q)
.

apply_reduce([], []):- true .
apply_reduce([E|P], Q):-
	reduce(E, [E|P], Q)
	%intersection(P, Qp, Ei),
	%anti_intersection(P, Qp, Eai),
	%write('avant: '), print(P), write(", inter: "),print(Qp), write(" => "),print(Ei),nl,
	%print(P), write(" AI: "), print(Qp), write(" => "),print(Eai),nl,
	%apply_reduce(Qp, Qz) ;
	%apply_reduce(Eai, Qz),
	%union( Eai, Qz, Q)
.

unifie([E|P]):-
	%length(E, 1),%pour empêcher une liste vide
	%apply_reduce(P, Q),
	write("passage dans unifie, E: "),print(E),nl,
	reduce(E, [E|P], Q),
	print("finito: "), print(Q)
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
	atom(R)%nope, une constante c'est pas forcément un atome, enfin je crois :)
.

regle(E, rename):-
	split(E, _, R),
	var(R)
.

regle(E, expand):-
	split(E, X, Y),
	is_function(Y, _, _),
	not(occur_check(X, Y))
.

regle(E, clash):-
	split(E, L, R),
	is_function(L, Nl, Al),
	is_function(R, Nr, Ar),
	not( (Nl == Nr, Al == Ar) )
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

replace([], _, _, []):- true. 
replace([A|List], X, T, NList):-
	%remplace X par T si X est une variable
	%TODO: tester remplacer plusieurs fois dans la même fonction / niveaux récursion sup
	occur_check(X, A),
	%real_replace(A,X,Y),
	args_in_function(A, Args),
	replace(Args, X, T, Nf),
	replace(List, X, T, R),
	union(Nf, R, NList)%TODO: nooooope, surement un set_minus
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

apply(rename, E, P, S):-
	split(E, L, R),
	replace(P, L, R, Pb),
	%set_minus(E, P, Pp),
	%replace(Pp, L, R, Pt ),
	union(E, Pb, S)
.
apply(simplify, E, P, S):-
	split(E, _, _),
	S = P
	%TODO
.
apply(expand, _, P, S):-
S = P,
true %TODO
.
apply(check, _, _, bottom):- false . %TODO: surement de la merde
apply(orient, _, P, S):-
S= P,
true %TODO
.
apply(decompose, E, P, S):-
	split(E, L, R),
	compound_name_arity(L, _, Arity),
	merge_function_args(Arity, L, R, Res), 
	set_minus(E, P, Pp),
	union(Res, Pp, S)
.

apply(clash, _, _, bottom):- false . %TODO: surement n'imp'


apply_rules(_, [], P, P):- true.
apply_rules(E, [FirstRule | ListRules], P, Q):-
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
	apply_rules(E, ListRules, P, Q),
	write("fin des réductions: "),print(Q),nl
.


set_minus(_, [], []) :- print('on ne devrait jamais passer par ici dans ce projet'),nl , !.
set_minus(Elem, [Elem|Set], Set):- ! .%Achtung
set_minus(Elem, [E|Set], Ret):-
	set_minus(Elem, Set, R),
	union(R, [E], Ret)
.


