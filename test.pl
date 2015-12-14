:-
	op(20, xfy,[?=]),
	[projet]
.

test_apply():-
	apply(decompose, f(a) ?= f(a), [f(a) ?= f(a)], _),
	write("apply(decompose, f(a) ?= f(b), [f(a) ?= f(b)], [a ?= b])"),
	apply(decompose, f(a) ?= f(b), [f(a) ?= f(b)], [a ?= b]),
	write(": ok"), nl,
	apply(decompose, f(a) ?= g(a), [f(a) ?= g(a)], _),
	apply(decompose, f(a) ?= f(a, b), [f(a) ?= f(a, b)], _)
.
