test: unit_test 

unit_test: test_engine test_session

test_engine:
	rm -f test.db
	prolog  -s test_engine.pl -g plunit_engine:run

test_session:
	rm -f test.db
	prolog  -s test_session.pl -g plunit_session:run
	prolog  -s test_session.pl -g plunit_session:run_reload

load_test:
#	prolog  -s load_test.pl -g main
	prolog  -s test_load.pl -g plunit_engine:run

e2e_test:
	prolog  -s test.pl -g main