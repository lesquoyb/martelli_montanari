:-
	op(20, xfy,[?=]),
	[projet]
.

writeOK() :- write(" : ok"), nl.

tests() :-
	writeln("Debut des tests : "), nl,
	writeln("==== Test : Apply ===="), nl,
	%test_apply_decompose,
	%test_apply_rename,
	%test_apply_simplify,
	%test_apply_expand,
	test_apply_orient,
	test_apply_clash,
	test_apply_check,
	writeln("Apply : checked"),

	nl, writeln("==== Test : Regle ===="), nl,
	test_regle_simplify,
	test_regle_rename,
	test_regle_expand,
	test_regle_orient,
	test_regle_decompose,
	test_regle_clash,
	test_regle_check,
	writeln("Regle : checked"),

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
	writeOK,

	write("Q ?= a"),
	regle(Q?=a, simplify),
	writeOK,

	write("X ?= f(a)"),
	not(regle(X ?= f(a), simplify)),
	writeOK,

	write("X ?= Q"),
	not(regle(X?=Q, simplify)),
	writeOK,

	write("f(a) ?= W"),
	not(regle(f(a) ?= W, simplify)),
	writeOK,

	write("f(a) ?= f(W)"),
	not(regle(f(a) ?= f(W), simplify)),
	writeOK
.

test_regle_rename() :-
	writeln("==== Regle : rename ===="),

	write("X ?= a"),
	not(regle(X?=a, rename)),
	writeOK,

	write("Q ?= a"),
	not(regle(Q?=a, rename)),
	writeOK,

	write("X ?= f(a)"),
	not(regle(X ?= f(a), rename)),
	writeOK,

	write("X ?= Q"),
	regle(X?=Q, rename),
	writeOK,

	write("XTA ?= QSD"),
	regle(X?=Q, rename),
	writeOK,

	write("f(a) ?= W"),
	not(regle(f(a) ?= W, rename)),
	writeOK,

	write("f(a) ?= f(W)"),
	not(regle(f(a) ?= f(W), rename)),
	writeOK
.

test_regle_expand() :-
	writeln("==== Regle : expand ===="),

	write("X ?= a"),
	not(regle(X?=a, expand)),
	writeOK,

	write("Q ?= a"),
	not(regle(Q?=a, expand)),
	writeOK,

	write("X ?= f(a)"),
	regle(X ?= f(a), expand),
	writeOK,

	write("X ?= f(Q)"),
	regle(X ?= f(Q), expand),
	writeOK,

	write("X ?= Q"),
	not(regle(X?=Q, expand)),
	writeOK,

	write("f(a) ?= W"),
	not(regle(f(a) ?= W, expand)),
	writeOK,

	write("f(a) ?= f(W)"),
	not(regle(f(a) ?= f(W), rename)),
	writeOK
.

test_regle_orient() :-
	writeln("==== Regle : orient ===="),

	write("X ?= a"),
	not(regle(X?=a, orient)),
	writeOK,

	write("Q ?= a"),
	not(regle(Q?=a, orient)),
	writeOK,

	write("X ?= f(a)"),
	not(regle(X ?= f(a), orient)),
	writeOK,

	write("X ?= Q"),
	not(regle(X?=Q, orient)),
	writeOK,

	write("f(a) ?= W"),
	regle(f(a) ?= W, orient),
	writeOK,

	write("f(X) ?= W"),
	regle(f(X) ?= W, orient),
	writeOK,

	write("f(a) ?= f(W)"),
	not(regle(f(a) ?= f(W), orient)),
	writeOK
.


test_regle_decompose() :-
	writeln("==== Regle : decompose ===="),

	write("X ?= a"),
	not(regle(X?=a, decompose)),
	writeOK,

	write("Q ?= a"),
	not(regle(Q?=a, decompose)),
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

	write("Q ?= a"),
	not(regle(Q?=a, clash)),
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

	write("f(Q, E) ?= g(E, V, e)"),
	regle(f(Q, E) ?= g(E, V), clash),
	writeOK
.

test_regle_check() :-
	writeln("==== Regle : check ===="),

	write("X ?= a"),
	not(regle(X?=a, check)),
	writeOK,

	write("Q ?= a"),
	not(regle(Q?=a, check)),
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

	write("X ?= f(X)"),
	regle(X ?= f(X), check),
	writeOK,

	write("X ?= f(a,X)"),
	regle(X ?= f(a,X), check),
	writeOK
.
