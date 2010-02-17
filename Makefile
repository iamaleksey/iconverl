all: ebin/iconv.beam priv/iconv.so


ebin/iconv.beam: src/iconv.erl
	$(ERL_TOP)/bin/erlc -o ebin src/iconv.erl


priv/iconv.so: c_src/iconv.c
	gcc -fPIC -shared -o priv/iconv.so c_src/iconv.c -I $(ERL_TOP)/erts/emulator/beam/


shell:
	$(ERL_TOP)/bin/erl -pa ../iconverl/ebin


clean:
	rm -f priv/iconv.so ebin/iconv.beam
