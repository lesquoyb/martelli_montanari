:-[projet].
/*
unifie([]):- true.
unifie(bottom):- false.
unifie(P) :-
	choix_premier(P, E, R, _),
	apply(R, E, P, Q),
	unifie(Q),
.
*/
unifie([], R):-true, !.
unifie(P, pondere) :-
	write("==== unifie ===="), nl,
	choix_pondere(P,_,E, R),
	write("règle: "),print(R),nl,
	reduit(R, E, P, Q),
	write("Q:"),print(Q),nl,
	unifie(Q, pondere)
.


select_rule([], _, _, _):- false, !.%on a parcouru la liste des règle sans en trouver une qui fonctionne
select_rule( [Next |  MasterList], [], E, R):-
	select_rule(MasterList, Next, E, R)
.
select_rule( MasterList,[FirstRule | ListRules], E, R):-
	(
		regle(E, FirstRule),
		R = FirstRule,!
	;
		print(FirstRule),write(" a été éliminé"),nl,
		select_rule(MasterList, ListRules, E, R),!
	)
.


liste_pondere([ [clash, check],
				[rename, simplify],
				[orient, decompose],
				[expand]
			  ]):- 
	true 
.





choix_pondere([E|P], _, E, R):-
	liste_pondere( [FirstRules | List] ),
	select_rule(List,FirstRules, E, R)
.
%TODO: proposer d'autres strats(random ?)
%TODO: en fait il faut enlever E de P dans choix


