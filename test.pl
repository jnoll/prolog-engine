%% Test the engine with some simple assertions.
:- use_module(engine).


main :-
	debug(engine),
	format('Initializing...~n'),
	%engine:initialize('test.rules', 'test_facts.db'),
	engine:initialize('test.rules', 'test_facts.db'),
	format('Initialization done.  Asserting test facts...~n'),
	%engine:assert_list([annoying(aorange), color(orange, aorange)]),
	engine:assert_fact(actionable(drink)),
	engine:assert_fact(effort(drink), 1),
	engine:assert_fact(steps(drink), 1),
	engine:assert_fact(actionable(eat)),
	engine:assert_fact(effort(eat), 4),
	engine:assert_fact(steps(eat), 1),
	engine:assert_fact(actionable(clean_garage)),
	engine:assert_fact(steps(clean_garage), 5),
	format('Test facts asserted.~n'),
	run.


run :-
	format('Running...~n'),
	engine:run,
	format('Showing...~n'),
	emit,
	format('Done~n'),
	engine:cleanup.



%% This little gem comes from
%% http://objectmix.com/prolog/183896-trying-out-prolog-building-lists-predicates.html.
%% '\+' succeeds if the following predicate doesn NOT.
find_recs(SoFar, Result) :-
	(engine:fact(recommendation(X), _),
	 debug(test,'recommendation ~p found ~n', [X]),
	 debug(test,'explanation ~p found ~n', [X]),
	 \+ memberchk(X, SoFar) % if X is not in SoFar
	->
	 find_recs([X|SoFar], Result)	% then add it and go again.
	;
	 Result = SoFar).	% else done

emit :-
        debug(test,'emit: finding recs~n', []),
	find_recs([], Recs),
	format('Recommendations:~n~p~n', [Recs]).
