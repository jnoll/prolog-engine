:- use_module(library(plunit)).
:- begin_tests(engine).
:- use_module(engine).


run :-
	debug(engine),
	engine:reset_seq(0),
	engine:load_rules('load_test.rules'),
	engine:load_rules('load_test2.rules'),
	run_tests.


test(perform_load) :-
	engine:assert_fact(kerm, true)
	,engine:assert_fact(nonce, true)
	,engine:assert_fact(load_test, true)             % trigger rule 'load_test.rules:load_test'
	,engine:assert_fact(test_load_action2, true)    % trigger rule 'load_test2.rules:test_load_action2'
	,engine:run
	,engine:rules_loaded('load_test.rules')        % verify 'test.rules:test_load_action' fired 
	,engine:rules_loaded('load_test2.rules')        % verify 'test.rules:test_load_action' fired 
	.

test(perform_load2) :-
	engine:fact(recommendation(load_test), true, _) % verify 'load_test.rules:load_test' rule fired.
	.

test(perform_load3) :-
	engine:fact(recommendation(test_load_action2), true, _) % verify 'load_test2.rules:test_load_action2' fired
	.

	
:- end_tests(engine).
