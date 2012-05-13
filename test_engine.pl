:- use_module(library(plunit)).
:- begin_tests(engine).
:- use_module(engine).


run :-
	debug(engine),
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
	engine:fact(foo(bar), Seq),
	Seq is 0,
	engine:assert_fact(fro, 2),
	engine:fact(val(fro, 2), Seq2),
	Seq2 is 1.

test(eval) :-
	engine:eval(2 == 2),
	engine:eval(2 \= 3),
	engine:eval(3 > 2),
	engine:eval(2 < 3),
	engine:eval(2 =< 2),
	engine:eval(2 >= 2).

test(eval_fact) :-
	engine:reset_seq(0),
	engine:assert_fact(x, 2),
	engine:assert_fact(y, 2),
	engine:assert_fact(z, 3),
	engine:eval(x == y),
	engine:eval(x == 2),
	engine:eval(x \= z),
	engine:eval(x \= 3),
	engine:eval(z >  x),
	engine:eval(z >  2),
	engine:eval(z >= x),
	engine:eval(z >= 3),
	engine:eval(x >= y),
	engine:eval(x <  z),
	engine:eval(x <  3),
	engine:eval(x =< z),
	engine:eval(x =< 2),
	engine:eval(x =< y),
	engine:eval(x =< 2).

test(eval_not) :-
	engine:eval(not(baz)),
	engine:eval(not(bar)),
	engine:eval(not(foo)),
	engine:assert_fact(foo, yes),
	engine:eval(not(foo, no)).

test(eval_fail, [fail]) :-
	engine:eval(not(foo)),
	engine:eval(not(foo, yes)).

test(instantiations_fact) :-
	engine:reset_seq(0),
	engine:assert_fact(annoying(orange)),
	engine:assert_fact(midget(apple)),
	engine:instantiations([midget(_), annoying(_)], Result),
	%write(Result), nl, 
	Result == [i(midget(apple), 1),i(annoying(orange),0)].

test(instantiations_eval) :-
	engine:reset_seq(0),
	engine:assert_fact(color, orange),
	engine:assert_fact(age, 2),
	engine:instantiations([color==orange, age > 1], Result),
	format('instantiations_eval: Result = ~p~n', [Result]),
	Result == [i(color==orange, 0), i(age>1, 0)].

test(find_ready_rules) :-
	engine:find_ready_rules(Rules),
	% Only annoying(orange) and midget(apple) were asserted as
	% possibly releveant facts above, so at this point no rules s.b. ready.
	Rules == [],
	engine:assert_fact(color(orange,orange)),
	engine:find_ready_rules(R2),
	format('find_ready_rules: R2 = ~p~n', [R2]),
	R2 == [r([i(annoying(orange),0),i(color(orange,orange),2)],a,[annoying(orange),color(orange,orange)],[assert_fact(isa(orange,orange))])].

					      

test(remove_instantiated) :-
	engine:find_ready_rules(Rs), 
	engine:remove_instantiated(Rs, Result),
	format('remove_instantiated: Result = ~p~n', [Result]),
	Result == [r([i(annoying(orange),0),i(color(orange,orange),2)],a,[annoying(orange),color(orange,orange)],[assert_fact(isa(orange,orange))])].


test(select_rule) :-
	engine:find_ready_rules(Rs), 
	engine:select_rule(Rs, Result),
	format('select_rule: Result = ~p~n', [Result]),
	Result == r([i(annoying(orange),0),i(color(orange,orange),2)],a,[annoying(orange),color(orange,orange)],[assert_fact(isa(orange,orange))]).

:- end_tests(engine).
