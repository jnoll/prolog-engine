test: unit_test 

unit_test:
	prolog  -s test_engine.pl -g plunit_engine:run

load_test:
#	prolog  -s load_test.pl -g main
	prolog  -s test_load.pl -g plunit_engine:run

e2e_test:
	prolog  -s test.pl -g main