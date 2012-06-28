%%% -*- Prolog -*-
%%% Forward-chaining inference engine.
%%% (c) 2012 John Noll - Lero
:-module(engine, []).
:- use_module(library(debug)).


%% Rules look like this:
%% rule NAME: [COND1, COND2, ...] ==> [ACTION1, ACTION2, ...]
%% Example: rule orange: [annoying(X), color(X, orange)] ==> [assert_fact(isa(orange, X))].
%% The ':' operator is already defined in SWI.
:-op(600, xfx, ==>).
:-op(610, fx, rule).
%:-op(500, xfy, #).		% Unification.
%:-op(50, xfy, :).

%% XXX for compatibility with old engine, a second argument for a persistent facts db is accepted.
initialize(Rules, _) :-
	load_rules(Rules),
	reset_seq(0).
%% XXX for compatibility with old engine.
cleanup :- debug(engine, 'cleaning up.', []).

%% XXX This must be the first engine predicate invoked, otherwise it
%% will fail with a syntax error :(.
load_rules(F) :-
	reconsult(F),
	assert(rules_loaded(F)).

%% Entry point.
run :-
	find_ready_rules(Rs),
	debug(engine, 'Selecting rule from: ~p~n', [Rs]),
	select_rule(Rs, r(InstantiatedFacts, Name, Conds, Actions)),
	fire(Actions),
	assert(instantiation(InstantiatedFacts)), !,
	run.

run :- debug(engine, 'run done.~n', []).
	
%% Find all rules that are ready to fire.
find_ready_rules(Result) :-
	findall(r(InstantiatedFacts, Name, Conds, Actions),
		(rule Name: Conds ==> Actions,
		 instantiated_facts(Conds, InstantiatedFacts)),
		Result).

%% Select a rule structure from ready rule structures.
%% XXX It's just the first uninstantiated rule found.
select_rule(Rs, R) :-
	remove_instantiated(Rs, [R|Uninstantiated]).

%% Remove instantiated rule structures from a list.  Rule structures
%% get instantiated when a rule 'fires', indicating that the rule has
%% already fired and shouldn't be considered again with the same
%% facts.
remove_instantiated([], []).
remove_instantiated([r(InstantiatedFacts, Name, Conds, Actions)|Rest], Result) :-
	instantiation(InstantiatedFacts), !, % rule instantiated: leave head out,
				% continue on remaining rules.
	remove_instantiated(Rest, Result).
remove_instantiated([Uninstantiated|Rest], [Uninstantiated|Result]) :-
	% If we get here, head of 1st list is uninstantiated, so cons
	% it to Result after removing instantiations from Rest.
	remove_instantiated(Rest, Result).


%% Facts and recommendations get asserted on the fly.  A fact has two
%% components: the fact, and a sequence number indicating when the
%% fact was asserted.  A recommendation is just like a fact without a
%% timestamp.
:- dynamic fact/2.
:- dynamic recommendation/1.
%% Instantiations are records that record rules that have fired, by
%% saving the values bound to conditions that cause the rule to be
%% ready.
:- dynamic instantiation/1.


%% Facts are either traditional prolog facts, as in 'isa(socrates, man)', or
%% pairs (Factname, Value), as in '(length, 2)'.  This enables rules like
%% rule r1: [length > 2] ==> [recommend(bookshelf)].
%% as well as
%% rule r2: [isa(X, man)] ==> [mortal(X)].
assert_fact(Fact, Val) :-
	debug(engine, 'assert_fact: ~p, ~p', [Fact, Val]),
	get_seq(Seq), 
	debug(engine, 'assert_fact: asserting fact(~p, ~p, ~p)', [Fact, Val, Seq]),
	asserta(fact(Fact, Val, Seq)).
assert_fact(Fact) :-
	debug(engine, 'assert_fact: ~p', [Fact]),
	get_seq(Seq), 
	debug(engine, 'assert_fact: asserting fact(~p, ~p)', [Fact, Seq]),
	asserta(fact(Fact, true, Seq)).
assert_fact(not(Fact)) :-
	debug(engine, 'assert_fact: ~p', [Fact]),
	get_seq(Seq), 
	debug(engine, 'assert_fact: asserting fact(~p, ~p)', [Fact, Seq]),
	asserta(fact(Fact, false, Seq)).

%% Assert a list of facts, to initialize database before running inference.
assert_list([]) :-
	debug(engine, 'No list to assert.', []),!.

assert_list([H|T]) :-
	debug(engine, 'Asserting head of list: ~p', [H]),
	assert_fact(H),
	!,assert_list(T).

%% Sequence numbers for "time stamping" facts.
reset_seq(N) :-
       retract(seq_num(_)),
       assert(seq_num(N)),
       !.

reset_seq(N) :-
       assert(seq_num(N)).

get_seq(N) :-
       retract(seq_num(N)),
       NN is N + 1,
       assert(seq_num(NN)),
       !.


%% Create a list of instantiated facts that match conditions of rules.
%% Each rule has a list of conditions on its LHS.  If condition Cond
%% of a rule matches some fact in working memory, "i(Cond,Seq)" is
%% created as an "instantiation" of the condition.  If Cond is a fact,
%% Seq will be the sequence number when it was asserted; if Cond is a
%% comparison for eval/1, Seq will be set to 0.
instantiated_facts([], []).
instantiated_facts([Cond|Conds], [i(Cond, Seq)|InstantiatedFacts]) :-
	debug(engine, "Cond: ~p", [Cond]),
	!,
	(fact(Cond, true, Seq); % a fact matching Cond was asserted at Seq, or
	 eval(Cond), Seq = 0), % Cond evaluates to true, create dummy Seq = 0.
	instantiated_facts(Conds, InstantiatedFacts),
	debug(engine, "InstantiatedFacts: ~p", [InstantiatedFacts]).

%% The old implementation allowed conditions to be identified by an ID number.
instantiated_facts([ID:Cond|Conds], [i(Cond, Seq)|InstantiatedFacts]) :-
	debug(engine, "Cond: ~p", [Cond]),
	!,
	(fact(Cond, true, Seq); % a fact matching Cond was asserted at Seq, or
	 eval(Cond), Seq = 0), % Cond evaluates to true, create dummy Seq = 0.
	instantiated_facts(Conds, InstantiatedFacts),
	debug(engine, "InstantiatedFacts: ~p", [InstantiatedFacts]).

%% Tests for individual conditions.
eval(X == Y) :-
	fact(X, Vx, _),
	fact(Y, Vy, _), !,
	Vx == Vy.
eval(X == Y) :-
	fact(X, Vx, _), !,
	Vx == Y.
eval(X == Y) :- X == Y, !.

eval(X \= Y) :-
	fact(X, Vx, _),
	fact(Y, Vy, _), !,
	Vx \= Vy.
eval(X \= Y) :-
	fact(X, Vx, _),
	!,
	Vx \= Y.
eval(X \= Y) :-
	number(X), !,
	X \= Y.

eval(X > Y)  :-
	fact(X, Vx, _),
	fact(Y, Vy, _), !,
	Vx >  Vy, !.
eval(X > Y)  :-
	fact(X, Vx, _), !,
	Vx >  Y.
eval(X > Y)  :-
	number(X), !,
	X >  Y.

eval(X >= Y) :-
	fact(X, Vx, _),
	fact(Y, Vy, _), !,
	Vx >= Vy.
eval(X >= Y) :-
	fact(X, Vx, _), !,
	Vx >= Y.
eval(X >= Y) :-
	number(X), !,
	X >= Y.

eval(X < Y)  :-
	fact(X, Vx, _),
	fact(Y, Vy, _), !,
	Vx <  Vy.
eval(X < Y)  :-
	fact(X, Vx, _), !,
	Vx <  Y.
eval(X < Y)  :-
	number(X), !,
	X <  Y.

eval(X =< Y) :-
	debug(engine, 'eval1 ~p =< ~p', [X, Y]),
	fact(X, Vx, _),
	fact(Y, Vy, _), !,
	Vx =< Vy.
eval(X =< Y) :-
	debug(engine, 'eval2 ~p =< ~p', [X, Y]),
	fact(X, Vx, _), !,
	Vx =< Y.
eval(X =< Y) :-
	debug(engine, 'eval3 ~p =< ~p', [X, Y]),
	number(X), !,
	X =< Y.

% Test existence of a fact.
eval(not(X, V)) :-
	fact(X, Y, _), !, V \= Y.
eval(not(X)) :-
	fact(X, V,  _), !, V == false.
eval(not(X, V)) :- true.
eval(not(X)) :- true.


%% "Fire" a rule by performing each of its actions.
fire([]) :-
	debug(engine, 'fire: nothing to fire?', []),
	!.
fire([Action|Rest]) :-
	debug(engine, 'fire: firing ~p', [Action]),
	perform(Action),
	debug(engine, '...fired ~p', [Action]),
	fire(Rest).
fire([Action|Rest]) :-
	debug(engine, 'fire: ~p failed.', [Action]).

%% Perform an action.
%% The actions are:
%% unify ('#'), XXX not implemented
%% compare ('='),
%% assert_fact
%% recommend - a convenience function to assert a recommendation fact.
%% ask - a convenience function to assert a query fact.

perform(assert(X)) :-
	debug(engine, 'perform: assert_fact: asserting ~p', [X]),
	assert_fact(X),
	!.
perform(recommend(X)) :-
	debug(engine, 'perform: recommend: ~p', [X]), 
	assert_fact(recommendation(X)),
	!.
perform(ask(X)) :-
	debug(engine, 'perform: query: ~p', [X]), 
	assert_fact(query(X)),
	!.
perform(load(X)) :-
	debug(engine, 'perform: load: ~p', [X]), 
	load_rules(X),
	!.
	

perform(X = Y) :-
	X is Y,
	!.
perform(member(X,Y)) :- member(X,Y), !.
member(X,[X|Y]).
member(X,[Y|Z]) :- member(X,Z).

