# OS X
#GCCFLAGS = -fPIC -shared -liconv -flat_namespace -undefined suppress -fno-common -Wall
# Linux
GCCFLAGS = -fPIC -shared


all: ebin/iconv.beam priv/iconv.so


ebin/iconv.beam: src/iconv.erl
	$(ERL_TOP)/bin/erlc -o ebin src/iconv.erl


priv/iconv.so: c_src/iconv.c
	gcc $(GCCFLAGS) -I $(ERL_TOP)/erts/emulator/beam/ -o priv/iconv.so c_src/iconv.c


shell:
	$(ERL_TOP)/bin/erl -pa ../iconverl/ebin


clean:
	rm -f priv/iconv.so ebin/iconv.beam
