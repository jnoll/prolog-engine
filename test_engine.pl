:- use_module(library(plunit)).
:- use_module(library(debug)).
:- begin_tests(engine).
:- use_module(engine).



run :-
%	debug(engine),
	session:load('test.db'),
	engine:load_rules('test.rules'),
						  %engine:load_rules('gtm.okb'),
	run_tests.

test(seq) :-
	engine:reset_seq(0),
	engine:get_seq(N),
	N is 0.

test(assert_fact) :-
	engine:reset_seq(0),
	engine:assert_fact(foo(bar)),
	engine:fact(foo(bar), V, Seq),
%	V == true,
	Seq == 0.

test(assert_fact2) :-
	engine:assert_fact(fro, 2),
	engine:fact(fro, V2, Seq2),
	assertion(V2 == 2),
	assertion(Seq2 == 1).

test(eval) :-
	engine:eval(2 == 2),
	assertion(engine:eval(3 > 2)),
	assertion(engine:eval(2 \= 3)),
	assertion(engine:eval(2 < 3)),
	assertion(engine:eval(2 =< 2)),
%XXX	assertion(engine:eval(2 <= 2)),
	assertion(engine:eval(2 >= 2)).

test(eval_fact) :-
	engine:reset_seq(0),
	engine:assert_fact(x, 2),
	engine:assert_fact(y, 2),
	engine:assert_fact(z, 3),
	engine:assert_fact(nan, foo),
	assertion(engine:eval(x == y)),
	assertion(engine:eval(x == 2)),
	assertion(engine:eval(nan == foo)),
	assertion(engine:eval(x \= z)),
	assertion(engine:eval(x \= 3)),
	assertion(engine:eval(nan \= bar)),
	assertion(engine:eval(z >  x)),
	assertion(engine:eval(z >  2)),
	assertion(engine:eval(z >= x)),
	assertion(engine:eval(z >= 3)),
	assertion(engine:eval(x >= y)),
	assertion(engine:eval(x <  z)),
	assertion(engine:eval(x <  3)),
%XXX	assertion(engine:eval(x <= z)),
%XXX	assertion(engine:eval(x <= 2)),
%XXX	assertion(engine:eval(x <= y)),
%XXX	assertion(engine:eval(x <= 2)),
	assertion(engine:eval(x =< z)),
	assertion(engine:eval(x =< 2)),
	assertion(engine:eval(x =< y)),
	assertion(engine:eval(x =< 2)).

test(eval_fact_fail, [fail]) :-
	engine:reset_seq(0),
	engine:assert_fact(x, 2),
	engine:assert_fact(y, 2),
	engine:assert_fact(z, 3),
	engine:eval(x == z).
test(eval_fact_fail2, [fail]) :-
	engine:eval(x == 3).
test(eval_fact_fail3, [fail]) :-
	engine:eval(x \= y).
test(eval_fact_fail4, [fail]) :-
	engine:eval(x \= 2).
test(eval_fact_fail5, [fail]) :-
	engine:eval(z <  x).
test(eval_fact_fail6, [fail]) :-
	engine:eval(z <  2).
test(eval_fact_fail7, [fail]) :-
	engine:eval(z =< x).
test(eval_fact_fail8, [fail]) :-
	engine:eval(z >= 4).
test(eval_fact_fail9, [fail]) :-
	engine:eval(x >= z).
test(eval_fact_fail10, [fail]) :-
	engine:eval(x >  z).
test(eval_fact_fail11, [fail]) :-
	engine:eval(x >  3).
test(eval_fact_fail12, [fail]) :-
	engine:eval(x >= 3).
%%% XXX This does not fully exercise eval.
test(eval_fact_fail13, [fail]) :-
	engine:reset_seq(0),
	engine:assert_fact(foo(x), 2),
	engine:assert_fact(foo(y), 2),
	engine:assert_fact(foo(z), 3),
	engine:assert_fact(effort(eat), 3),
	engine:eval(effort(eat) =< 2).
test(eval_fact_fail15, [fail]) :-
	engine:eval(foo(x) == foo(z)).
test(eval_fact_fail16, [fail]) :-
	engine:eval(foo(x) == 3).
test(eval_fact_fail17, [fail]) :-
	engine:eval(foo(x) \= y).
test(eval_fact_fail18, [fail]) :-
	engine:eval(foo(x) \= 2).
test(eval_fact_fail19, [fail]) :-
	engine:eval(foo(z) <  foo(x)).
test(eval_fact_fail20, [fail]) :-
	engine:eval(foo(z) <  2).
test(eval_fact_fail21, [fail]) :-
	engine:eval(foo(z) =< foo(x)).
test(eval_fact_fail22, [fail]) :-
	engine:eval(foo(z) =< 2).
test(eval_fact_fail23, [fail]) :-
	engine:eval(foo(x) >= foo(z)).
test(eval_fact_fail24, [fail]) :-
	engine:eval(foo(x) >  foo(z)).
test(eval_fact_fail25, [fail]) :-
	engine:eval(foo(x) >  3).
test(eval_fact_fail26, [fail]) :-
	engine:eval(foo(x) >= 3).


test(eval_fact_nonnumeric, [fail]) :-
	engine:eval(nan < 5).
test(eval_fact_nonnumeric2, [fail]) :-
	engine:eval(nan > 5).


test(eval_not) :-
	engine:eval(not(baz)),
	engine:eval(not(bar)),
	engine:eval(not(foo)),
	engine:assert_fact(foo, yes),
	engine:eval(not(foo, no)).

test(eval_not_fail, [fail]) :-
	engine:eval(not(foo)),
	engine:eval(not(foo, yes)).

test(instantiated_facts) :-
	engine:reset_seq(0),
	engine:assert_fact(annoying(orange)),
	engine:assert_fact(midget(apple)),
	engine:instantiated_facts([midget(_), annoying(_)], Result),
						  %write(Result), nl, 
	Result == [i(midget(apple), 1),i(annoying(orange),0)].

test(instantiated_facts_eval) :-
	engine:reset_seq(0),
	engine:assert_fact(color, orange),
	engine:assert_fact(age, 2),
	engine:instantiated_facts([color==orange, age > 1], Result),
	%format('instantiated_facts_eval: Result = ~p~n', [Result]),
	Result == [i(color==orange, 0), i(age>1, 0)].

test(find_ready_rules) :-
	engine:find_ready_rules(Rules),
						  % Only annoying(orange) and midget(apple) were asserted as
						  % possibly releveant facts above, so at this point no rules s.b. ready.
	Rules == [],
	engine:assert_fact(color(orange,orange)),
	engine:find_ready_rules(R2),
	%format('find_ready_rules: R2 = ~p~n', [R2]),
	R2 == [r([i(annoying(orange),0),i(color(orange,orange),2)],a,[annoying(orange),color(orange,orange)],[assert(isa(orange,orange)),recommend(avoid(orange))])].



test(remove_instantiated) :-
	engine:find_ready_rules(Rs), 
	engine:remove_instantiated(Rs, Result),
	%format('remove_instantiated: Result = ~p~n', [Result]),
	Result == [r([i(annoying(orange),0),i(color(orange,orange),2)],a,[annoying(orange),color(orange,orange)],[assert(isa(orange,orange)),recommend(avoid(orange))])].


test(select_rule) :-
	engine:find_ready_rules(Rs), 
	engine:select_rule(Rs, Result),
	%format('select_rule: Result = ~p~n', [Result]),
	Result == r([i(annoying(orange),0),i(color(orange,orange),2)],a,[annoying(orange),color(orange,orange)],[assert(isa(orange,orange)),recommend(avoid(orange))]).


test(perform) :-
	engine:perform(ask(can_you_do_this)),
	assertion(engine:fact(query(can_you_do_this), true, _)),
	engine:perform(recommend(try_it)),
	assertion(engine:fact(recommendation(try_it), true, _)).

test(perform_load) :-
	engine:assert_fact(nonce, true)
	,engine:perform(load('load_test.rules'))
	,engine:rules_loaded('load_test.rules')
	,engine:assert_fact(load_test, true)             % trigger rule 'load_test.rules:load_test'
	,engine:assert_fact(test_load_action, true)     % trigger rule 'test.rules:test_load_action'
	,engine:assert_fact(test_load_action2, true)    % trigger rule 'load_test2.rules:test_load_action2'
	,engine:assert_fact(test_load_action_alt, true) % trigger rule 'test.rules:test_load_action_alt'
	,engine:assert_fact(test_post_load_action)
	,engine:run
	,engine:rules_loaded('load_test2.rules')        % verify 'test.rules:test_load_action' fired 
	.
%test(perform_load2) :-
%	engine:fact(recommendation(load_test), true, _) % verify 'load_test.rules:load_test' rule fired.
%	.
test(perform_load3) :-
	engine:fact(recommendation(test_load_action2), true, _) % verify 'load_test2.rules:test_load_action2' fired
	.


test(perform_load4) :-
	engine:fact(query(test_load_action), true, _) % verify 'test.rules:test_load_action_alt' fired.
	.

test(perform_load4a) :-
	engine:assert_fact(test_load_action_alt),
	engine:run,
	engine:fact(query(test_load_action), true, _)
	.


test(perform_load5) :-
	engine:assert_fact(test_post_load_action),
	engine:run,
	engine:fact(recommendation(post_load_action), true, _)
	.


test(perform_load6) :-
	engine:fact(recommendation(post_load_action), true, _)
	.


test(inference_seq) :-
	engine:assert_fact(sequence_1, true),
	engine:run,
	engine:fact(recommendation(end_sequence), true, _). 

% fanout from one fact.
test(inference_branch) :-
	engine:assert_fact(branch_start, true),
%	engine:assert_fact(branch_startb, true), 
	engine:run,
	engine:fact(recommendation(branch_end_2), true, _),
	engine:fact(recommendation(branch_end_3), true, _). 

    
	
test(done) :- halt.
:- end_tests(engine).
