:-
	op(20, xfy,[?=]),
	[projet]
.

writeOK() :- write(" : ok"), nl.

tests() :-
	writeln("Debut des tests : "), nl,
	writeln("==== Test : Reduit ===="), nl,
	test_reduit_decompose,
	test_reduit_rename,
	test_reduit_simplify,
	test_reduit_expand,
	test_reduit_orient,
	test_reduit_clash,
	test_reduit_check,
	writeln("Reduit : checked"),

	nl, writeln("==== Test : Regle ===="), nl,
	test_regle_simplify,
	test_regle_rename,
	test_regle_expand,
	test_regle_orient,
	test_regle_decompose,
	test_regle_clash,
	test_regle_check,
	writeln("Regle : checked"),

	nl,writeln("====== Test: Unifie ===="),nl,
	test_unifie(),
	nl,
	write("Tous les tests sont passé avec succes"),!
.

test_unifie():-
	unif([f(X, Y) ?= f(g(Z), h(a)), Z ?= f(Y)], premier),
	write("exemple prof juste: ok"),
	not(unif([f(X, Y) ?= f(g(Z), h(a)), Z ?= f(X)], premier)),
	write("exemple prof faux: ok")
.

test_reduit_decompose():-
	writeln("==== Reduit : Decompose ===="),

	write("f(a) ?= f(b), [a ?= b]"),
	reduit(decompose, f(a) ?= f(b), [f(a)?=f(b)], [a?=b]),
	writeOK,
	
	write("f(a) ?= f(a), [a ?= a]"), 
	reduit(decompose, f(a) ?= f(a), [], [a?=a]),
	writeOK,

	write("f(a,b,c) ?= f(d, e, f), [a ?= d, b ?= e, c ?= f]"), 
	reduit(decompose, f(a, b, c) ?= f(d, e, f), [], [a?=d, b ?= e, c ?= f ]),
	writeOK,


	write("f(X) ?= f(a), [X ?= a]"), 
	reduit(decompose, f(X) ?= f(a), [], [X?=a]),
	writeOK,


	write("f(a, X , b, Y) ?= f(d, e, f, g), [a ?= d, X ?= e, b ?= f, Y?= g]"), 
	reduit(decompose, f(a, X, b, Y) ?= f(d, e, f, g), [], [a?=d, X ?= e, b ?= f, Y ?= g]),
	writeOK,


write("f(g(X), W) ?= f(A, Q), [g(X) ?= A, W ?= Q]"),
	reduit(decompose, f(g(X), W) ?= f(A, Q), [f(g(X), W) ?= f(A, Q)], [g(X)?=A,W?=Q]),
	writeOK
.

test_reduit_rename() :-
	writeln("==== Reduit : Rename ===="),
	write("X?=Y, []"),
	reduit(rename, X2?=Y2, [X2?=Y2], []),
	X2 == Y2,
	writeOK,

	write("X?=Y,[X ?= a] => [Y ?= a]"),
	reduit(rename, X1?=Y1, [X1?=Y1, X1 ?= a], [Y1 ?= a]),
	X1 == Y1,
	writeOK

.

test_reduit_simplify() :-
	writeln("==== Reduit : Simplify ===="),
	
	write("X?=a , []"),
	reduit(simplify, X?=a, [X?=a], []),
	writeOK,
	write("X?=a,[Y ?= X] => [Y ?= a]"),
	reduit(simplify, X1?=a, [Y1?=X1, Y1 ?= X1], [Y1 ?= a]),
	writeOK
.

test_reduit_expand() :-
	writeln("==== Reduit : Expand ===="),

	write("X?=f(a), []"),
	reduit(expand, X2?=f(a), [X2?=f(a)], []),
	X2 == f(a),
	writeOK,

	write("X?=f(E), []"),
	reduit(expand, X3?=f(E2), [X3?=f(E2)], []),
	X3 == f(E2),
	writeOK
.

test_reduit_orient() :-
	writeln("==== Reduit : Orient ===="),

	write("f(W)?=X, [X?=f(W)]"),
	reduit(orient, f(W)?=X, [f(W)?=X], [X?=f(W)]),
	writeOK,

	write("f(a)?=X, [X?=f(a)]"),
	reduit(orient, f(a)?=X, [f(a)?=X], [X?=f(a)]),
	writeOK
.

test_reduit_clash() :-
	writeln("==== Reduit : clash ===="),
	
	write("clash"),
	not(reduit(clash, _, _, bottom)),
	writeOK
.

test_reduit_check() :-
	writeln("==== Reduit : check ===="),

	write("check"),
	not(reduit(check, _, _, bottom)),
	writeOK
.

test_regle_simplify() :- 
	writeln("==== Regle : simplify ===="),

	write("X ?= a"),
	regle(X?=a, simplify),
	writeOK,

	write("X ?= 1"),
	regle(X44 ?= 1, simplify),
	writeOK,


	write("a ?= b"),
	not(regle(a ?= b, simplify)),
	writeOK,

	write("X ?= f(a)"),
	not(regle(X1 ?= f(a), simplify)),
	writeOK,

	write("X ?= Y"),
	not(regle(X2 ?= Y, simplify)),
	writeOK,


	write("X ?= f(Y)"),
	not(regle(X3 ?= f(Y), simplify)),
	writeOK,

	write("f(a) ?= W"),
	not(regle(f(a) ?= W1, simplify)),
	writeOK,

	write("f(a) ?= f(W)"),
	not(regle(f(a) ?= f(W2), simplify)),
	writeOK
.

test_regle_rename() :-
	writeln("==== Regle : rename ===="),

	write("X ?= a"),
	not(regle(X1?=a, rename)),
	writeOK,

	write("X ?= f(a)"),
	not(regle(X2 ?= f(a), rename)),
	writeOK,

	write("X ?= Q"),
	regle(X3?=Q1, rename),
	writeOK,

	write("a ?= W"),
	not(regle(a ?= W1, rename)),
	writeOK,

	write("a ?= b"),
	not(regle(a ?= b, rename)),
	writeOK,

	write("a ?= f(b)"),
	not(regle( a ?= f(b), rename)),
	writeOK,


	write("f(a) ?= W"),
	not(regle(f(a) ?= W2, rename)),
	writeOK,

	write("f(a) ?= f(W)"),
	not(regle(f(a) ?= f(W3), rename)),
	writeOK
.

test_regle_expand() :-
	writeln("==== Regle : expand ===="),

	write("X ?= a"),
	not(regle(X1 ?= a, expand)),
	writeOK,

	write("X ?= f(a)"),
	regle(X2 ?= f(a), expand),
	writeOK,

	write("X ?= f(Q)"),
	regle(X3 ?= f(Q1), expand),
	writeOK,


	write("X ?= f(X)"),
	not(regle(X10 ?= f(X10), expand)),
	writeOK,


	write("X ?= f(a, b, X)"),
	not(regle(X12 ?= f(a, b, X12), expand)),
	writeOK,

	write("X ?= f(a, b, c, g(X))"),
	not(regle(X11 ?= f(a, b, c,g(X11) ), expand)),
	writeOK,

	write("X ?= Q"),
	not(regle( X4 ?=Q2, expand)),
	writeOK,

	write("f(a) ?= W"),
	not(regle(f(a) ?= W1, expand)),
	writeOK,

	write("f(a) ?= f(W)"),
	not(regle(f(a) ?= f(W2), rename)),
	writeOK
.

test_regle_orient() :-
	writeln("==== Regle : orient ===="),

	write("X ?= a"),
	not(regle(X1?=a, orient)),
	writeOK,

	write("X ?= f(a)"),
	not(regle(X2 ?= f(a), orient)),
	writeOK,

	write("X ?= Q"),
	not(regle(X3 ?=Q, orient)),
	writeOK,

	write("a ?= W"),
	regle(a ?= W1, orient),
	writeOK,

	write("f(a) ?= W"),
	regle(f(a) ?= W2, orient),
	writeOK,

	write("f(X) ?= W"),
	regle(f(X) ?= W4, orient),
	writeOK,

	write("f(a) ?= f(W)"),
	not(regle(f(a) ?= f(W3), orient)),
	writeOK
.


test_regle_decompose() :-
	writeln("==== Regle : decompose ===="),

	write("X ?= a"),
	not(regle( X ?= a, decompose)),
	writeOK,

	write("X ?= f(a)"),
	not(regle(X ?= f(a), decompose)),
	writeOK,

	write("X ?= Q"),
	not(regle(X?=Q, decompose)),
	writeOK,

	write("f(X) ?= W"),
	not(regle(f(X) ?= W, decompose)),
	writeOK,

	write("f(a) ?= g(a)"),
	not(regle(f(a) ?= g(a), decompose)),
	writeOK,

	write("f(a) ?= f(a,b)"),
	not(regle(f(a) ?= f(a, b), decompose)),
	writeOK,
	
	write("f(a,b) ?= f(a)"),
	not(regle(f(a,b) ?= f(a), decompose)),
	writeOK,
	
	write("f(a,g()) ?= f(b, c())"),
	regle(f(a, g()) ?= f(b, c()), decompose),
	writeOK,

	
	write("f(a, X) ?= f(a, b)"),
	regle(f(a, X) ?= f(a,b), decompose),
	writeOK,

	write("f(a) ?= f(W)"),
	regle(f(a) ?= f(W), decompose),
	writeOK,

	write("f(a, E, q) ?= f(W, c, e)"),
	regle(f(a) ?= f(W), decompose),
	writeOK
.

test_regle_clash() :-
	writeln("==== Regle : clash ===="),

	write("X ?= a"),
	not(regle(X?=a, clash)),
	writeOK,

	write("X ?= f(a)"),
	not(regle(X ?= f(a), clash)),
	writeOK,

	write("X ?= Q"),
	not(regle(X?=Q, clash)),
	writeOK,

	write("f(X) ?= W"),
	not(regle(f(X) ?= W, clash)),
	writeOK,

	write("f(a) ?= f(W)"),
	not(regle(f(a) ?= f(W), clash)),
	writeOK,

	write("f(Q, E) ?= g(E, V)"),
	regle(f(Q, E) ?= g(E, V), clash),
	writeOK,

	write("f(Q) ?= f(E, V)"),
	regle(f(Q, E) ?= g(E, V), clash),
	writeOK,

	write("f(x, y) ?= g(x, y)"),
	regle(f(x, y) ?= g(x, y), clash),
	writeOK,

	write("f(Q, E) ?= g(E, V, e)"),
	regle(f(Q, E) ?= g(E, V), clash),
	writeOK
.

test_regle_check() :-
	writeln("==== Regle : check ===="),

	write("X ?= a"),
	not(regle(X?=a, check)),
	writeOK,

	write("X ?= f(a)"),
	not(regle(X ?= f(a), check)),
	writeOK,

	write("X ?= Q"),
	not(regle(X?=Q, check)),
	writeOK,

	write("f(X) ?= W"),
	not(regle(f(X) ?= W, check)),
	writeOK,

	write("f(a) ?= f(W)"),
	not(regle(f(a) ?= f(W), check)),
	writeOK,

	write("f(Q, E) ?= g(E, V)"),
	not(regle(f(Q, E) ?= g(E, V), check)),
	writeOK,

	write("X ?= f(g(X))"),
	regle(X ?= f(g(X)), check),
	writeOK,

	write("X ?= f(X)"),
	regle(X ?= f(X), check),
	writeOK,


	write("X ?= f(a,X)"),
	regle(X ?= f(a,X), check),
	writeOK
.
