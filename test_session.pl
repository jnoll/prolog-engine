:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(session).

run :-
	debug(session),
	run_tests.

run_reload :-
	debug(session),
	run_tests(reload).

:- begin_tests(session).
test(basic) :-
	session:load('test.db'),
	session:save_fact(foo, bar, 1),
	session:get_fact(foo, V, S),
	V == bar,
	S == 1,
	session:save.

test(done) :- halt.
:- end_tests(session).


:- begin_tests(reload).

test(reload) :-
	session:load('test.db'),
	session:get_fact(foo, V, S),
	V == bar,
	S == 1.

test(reload2) :-
	session:load('test.db'),
	session:save_fact(foo, baz, 2),
	session:get_fact(foo, V, S),
	V == baz,
	S == 2,
	session:save.	

	
test(done) :- halt.
:- end_tests(reload).
