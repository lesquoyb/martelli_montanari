:-
	op(20, xfy,[?=]),
	[projet]
.

writeOK() :- write(" : ok"), nl.

tests() :-
	writeln("Debut des tests : "), nl,
	writeln("==== Test : Apply ===="), nl,
	test_apply_decompose,
	test_apply_rename,
	test_apply_simplify,
	test_apply_expand,
	test_apply_orient,
	test_apply_clash,
	test_apply_check,

	nl, writeln("==== Test : Regle ===="), nl,
	test_regle_simplify(),

	write("Tous les tests sont passé avec succes")
.

test_apply_decompose():-
	writeln("==== Apply : Decompose ===="),

	write("f(a) ?= f(b), [a ?= b]"),
	apply(decompose, f(a) ?= f(b), [f(a)?=f(b)], [a?=b]),
	writeOK,

	write("f(a) ?= g(a), [a ?= a]"), 
	apply(decompose, f(a) ?= g(a), [f(a) ?= g(a)], [a?=a]),
	writeOK,

	write("f(g(X), W) ?= f(A, Q), [g(X) ?= A, W ?= Q]"),
	apply(decompose, f(g(X), W) ?= f(A, Q), [f(g(X), W) ?= f(A, Q)], [f(_X)?=_A,_W?=_Q]),
	writeOK
.

test_apply_rename() :-
	writeln("==== Apply : Rename ===="),

	write("X?=Y, []"),
	apply(rename, X?=Y, [X?=Y], []),
	X == Y, % Je crois que ca doit etre ca...
	writeOK
.

test_apply_simplify() :-
	writeln("==== Apply : Simplify ===="),
	
	write("X?=a , []"),
	apply(simplify, X?=a, [X?=a], []),
	writeOK
.

test_apply_expand() :-
	writeln("==== Apply : Simplify ===="),

	write("X?=f(a), []"),
	apply(expand, X?=f(a), [X?=f(a)], []),
	X == f(a),
	writeOK,

	write("X?=f(E), []"),
	apply(expand, X?=f(E), [X?=f(E)], []),
	X == f(E),
	writeOK
.

test_apply_orient() :-
	writeln("==== Apply : Orient ===="),

	write("f(W)?=X, [X?=f(W)]"),
	apply(orient, f(W)?=X, [f(W)?=X], [X?=f(W)]),
	writeOK,

	write("f(a)?=X, [X?=f(a)]"),
	apply(orient, f(a)?=X, [f(a)?=X], [X?=f(a)]),
	writeOK
.

test_apply_clash() :-
	writeln("==== Apply : clash ===="),
	
	write("clash"),
	not(apply(clash, _, _, bottom)),
	writeOK
.

test_apply_check() :-
	writeln("==== Apply : check ===="),

	write("check"),
	not(apply(check, _, _, bottom)),
	writeOK
.

test_regle_simplify() :- 
	writeln("==== Regle : simplify ===="),

	write("X ?= a"),
	regle(X?=a, simplify),
	writeOK
.
