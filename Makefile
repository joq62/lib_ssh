all:
	rm -rf  *~ */*~ src/*.beam test/*.beam test_ebin erl_cra*;
	rm -rf _build;
	rm -rf ebin
	rm -rf rebar.lock;
	mkdir ebin;		
	rebar3 compile;	
	rm -rf _build*;
	rm -rf ebin;
	git add  *;
	git commit -m $(m);
	git push;
	echo Ok there you go!
build:
	rm -rf  *~ */*~ src/*.beam test/*.beam test_ebin erl_cra*;
	rm -rf _build;
	rm -rf ebin
	rm -rf rebar.lock;
	mkdir ebin;		
	rebar3 compile;	
	cp _build/default/lib/*/ebin/* ebin;
	rm -rf _build test_ebin;
clean:
	rm -rf  *~ */*~ src/*.beam test/*.beam test_ebin erl_cra*;
	rm -rf _build;
	rm -rf ebin
	rm -rf rebar.lock

eunit:
#	Standard
	rm -rf  *~ */*~ src/*.beam test/*.beam test_ebin erl_cra*;
	rm -rf _build;
	rm -rf ebin
	rm -rf rebar.lock
#	Application speciic
#	test 
	mkdir test_ebin;
	erlc -I include -I /home/joq62/erlang/include  -o test_ebin test/*.erl;
#  	dependencies
	erlc -I include -I /home/joq62/erlang/include  -o test_ebin ../lib_sd/src/*.erl;
#	Applications
	mkdir ebin;		
	rebar3 compile;	
	cp _build/default/lib/*/ebin/* ebin;
	rm -rf _build*;
#	Application specific
	erl -pa ebin -pa test_ebin\
	    -sname do_test\
	    -run $(m) start\
	    -setcookie test_cookie
