%% Test the engine with some simple assertions.
:- use_module(engine).


main :-
	debug(engine),
	format('Initializing...~n'),
	engine:initialize('test.rules', 'test_facts.db'),
	format('Initialization done.  Asserting test facts...~n'),
	engine:assert_list([actionable(eat), less_than_2_min(eat)]), %annoying(aorange), color(orange, aorange)]),
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
find_recs(SoFar, All, Result) :-
	(engine:recommendation(X),
	 debug(test,'recommendation ~p found ~n', [X]),
	 debug(test,'explanation ~p found ~n', [X]),
	 \+ memberchk(X, SoFar) % if X is not in SoFar
	->
	 find_recs([X|SoFar],
		   [element(recommendation, [id=X],
			    [
			     element(desc, [],
				     [
				      'description here'
				     ]),
			     element(details, [],
				     [
				      'details here'
				     ])
			    ])
		   |All],
		   Result)	% then add it and go again.
	;
	 Result = All).		% else done

emit :-
        debug(test,'emit: finding recs~n', []),
	find_recs([], [], Recs),
        debug(test,'emit: writing xml~n', []),
	length(Result, L),
	debug(test, 'The questions are ~p long: ~p~n', [L, Result]),
	xml_write(user,
		  element(result, [], [element(recommendations, [], Recs)|Result]),
		  [header(true)]
		 ).
