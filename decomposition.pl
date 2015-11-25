/* Definition de l'operateur */
:- op(200, xfy, [?=]).

print(Term) :-
    current_prolog_flag(print_write_options, Options), !,
    write_term(Term, Options).

unifie(X) :-
	select(E, X),
	reduit(decomposition, E, X, []) 
.

select(E, [X|Tail]) :-
	E = X
.

reduit(decomposition, E, P, Q) :-
	regle(E, decomposition),
		
.


regle(X, decomposition) :-
	arg(1, X, Y),
	arg(2, X, Z),
	check_decomp(Y, Z)
.

check_decomp(X, Y) :-
	functor(X, NX, AX),
	functor(Y, NY, AY),
	NX == NY,
	AX == AY
.





