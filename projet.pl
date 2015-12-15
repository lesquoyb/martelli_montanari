:- op(20,xfy,?=).

% Predicats d'affichage fournis

% set_echo: ce predicat active l'affichage par le predicat echo
set_echo :- assert(echo_on).

% clr_echo: ce predicat inhibe l'affichage par le predicat echo
clr_echo :- retractall(echo_on).

% echo(T): si le flag echo_on est positionne, echo(T) affiche le terme T
%          sinon, echo(T) reussit simplement en ne faisant rien.

echo(T) :- echo_on, !, write(T).
echo(_). 

%select_strat choisi une equation a l'aide de predicat correspondant a la strategie passee en parametre 
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

%le coeur du programme, essaye d'unifier P en utilisant la strategie Strat, si on ne passe pas de strategie il choisira la strategie "premiere equation"
unifie(P):- unifie(P, premier).
unifie([], _):- true, !.
unifie(bottom, _):- false, !. %Si on a bottom => c'est un echec
unifie(P, Strat):-
	echo("system: "),echo(P),echo("\n"),
	select_strat(Strat, P, E, R),
	echo(R),echo(": "),echo(E),echo("\n"),
	reduit(R, E, P, Q),
	unifie(Q, Strat), !
.
%appelle unifie apres avoir desactive les affichages
unif(P, S):-
	clr_echo,
	unifie(P, S)
.
%appelle unifie apres avoir active les affichages, affiche "Yes" si on peut unifier "No" sinon (il n'y a donc pas d'echec de la procedure.
trace_unif(P,S):-
	set_echo,
	(
		unifie(P, S), echo("Yes"),!
	;	
		echo("No")
	)
.
%sous fonction du predicat de selection d'equation avec choix pondere
select_rule([], _, _, _):- false, !.%on a parcouru la liste des règle sans en trouver une qui fonctionne
select_rule( [Next |  MasterList], [], Pbase, P, R, E):-%on a vide le sous groupe de regle qu'on etait en train de traiter, on passe au suivant
	select_rule(MasterList, Next, Pbase, P, R, E)
.
select_rule(MasterList, [ _ | ListRules], Pbase, [], R, E):-%on a parcourus toutes les equations sans pouvoir appliquer la regle voulue, on passe a la regle suivante, et on reinitialise les equations
	select_rule(MasterList, ListRules, Pbase, Pbase, R, E)
.
select_rule( MasterList,[FirstRule | ListRules], Pbase, [Ep|P], R, E):-%on essaye d'appliquer la regle FirstRule a Ep, si on echoue, on recommence avec l'equation suivante
	(
		regle(Ep, FirstRule),
		R = FirstRule,
		E = Ep,
		!
	;
		select_rule(MasterList, [FirstRule | ListRules], Pbase, P, R, E),!
	)
.
%remplie la liste pondere de regle, il s'agit d'une liste de liste. Chaque sous liste represente un groupe de regle de meme importance (oui on aurait pu faire une simple liste, mais c'est surement plus evolutif comme ça :) )
liste_pondere([ [clash, check],
				[rename, simplify],
				[orient, decompose],
				[expand]
			  ]):- 
	true 
.
%choix de la premiere equation dans P
choix_premier( [E|_], _, E, R):- 
	regle(E,R),!	
.
%choix de la premiere equation satisfaisant la regle de plus haute importance
choix_pondere(P, _, E, R):-
	liste_pondere( [FirstRules | List] ),
	select_rule(List,FirstRules, P, P, R, E)
.
%choix d'une equation aleatoire
choix_aleatoire(P, _, E, R):-
	random_member(E, P),
	regle(E, R),
	!
.
%choix de la derniere equation dans P
choix_dernier(P, _, E, R):-
	reverse(P, [E|_]),
	regle(E, R),
	!
.
%regle teste si on peut appliquer la regle R (deuxieme parametre) a l'equation E (premier parametre)
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

%Retourne L et R les arguments 1 et 2 de E (utilise pour couper une expression de type "X ?= Y" en deux)
split(E, L, R):-
	arg(1, E, L),
	arg(2, E, R)
.
%est vrai si V est une variable qui apparait dans T
occur_check(V, T) :-
	var(V),
	compound(T),
	term_variables(T, L),
	occur_check_list(V, L)
.
%sous fonction qui verifie parametre par parametre de T si V apparait
occur_check_list(_, []):-%on a parcouru tous les parametres sans rien trouver
	not(true)%parce que pourquoi avoir une constante false quand on peut faire not(true)
.
occur_check_list(V, [C|T]) :-%recursivite pour avancer dans la liste
	occur_check_list(V, T);
	V == C
.
%transforme f(x1,x2...,xn) ?= f(y1,y2,...,yn) en [x1 ?= y1, x2 ?= y2, ... , xn ?= yn] 
unif_list([], [],[]):- true.
unif_list([L|List1], [R|List2], [L ?= R| Rp]):-
	unif_list(List1, List2, Rp)
.
%reduit applique la regle R (premier argument) a l'equation E appartenant a P, et renvoie le nouvel ensemble Q
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
%supprime l'element Elem de la list en deuxieme parametre et renvoie Set le nouvel ensemble
delete_elem(_, [], []) :- !.
delete_elem(Elem, [Elem|Set], Set):- ! .
delete_elem(Elem, [E|Set], [E|R]):-
	delete_elem(Elem, Set, R)
.

