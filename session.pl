%% Procedures for persistent sessions.
:- module(session,
	  [load/1	% +File
	  ,reload/0
	  ,save/0	% just syncs.
	  ,close/0
	  ,save_fact/3		% +Name, +Value
	  ,get_fact/3		% +Name, -Value
	  ,clear_fact/1
	  ]).
:- use_module(library(persistency)).

:- persistent
	fact(name:callable, value:nonvar, seq:nonneg).

load(File) :-
	db_attach(File, flush).
%	db_sync(reload),
%	db_sync(gc).

reload :-
	db_sync(reload).

save :-
	db_sync(gc).

close :-
	db_sync(close).

save_fact(Name, Value, Seq) :-
	fact(Name, Value, Seq), !.	% already there.

save_fact(Name, Value, Seq) :-
	with_mutex(session_db,
		   (retractall_fact(Name, _, _),
		    assert_fact(Name, Value, Seq))).

get_fact(Name, Value, Seq) :-
	with_mutex(session_db,
		   (fact(Name, Value, Seq))).

% This is mostly useful for testing.
clear_fact(Name) :-
	retractall_fact(Name, _, _).