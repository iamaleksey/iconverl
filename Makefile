ifndef OSTYPE
OSTYPE=$(shell uname -s|awk '{print tolower($$0)}')
endif
ifeq "$(OSTYPE)" "linux"
GCCFLAGS = -fPIC -shared -Wall
endif
ifeq "$(OSTYPE)" "darwin"
GCCFLAGS = -fPIC -shared -liconv -flat_namespace -undefined suppress -fno-common -Wall
endif


all: ebin/iconverl.beam priv/iconverl.so


ebin/iconverl.beam: src/iconverl.erl
	$(ERL_TOP)/bin/erlc -o ebin src/iconverl.erl


priv/iconverl.so: c_src/iconverl.c
	gcc $(GCCFLAGS) -I $(ERL_TOP)/erts/emulator/beam/ -o priv/iconverl.so c_src/iconverl.c


shell:
	$(ERL_TOP)/bin/erl -pa ../iconverl/ebin


clean:
	rm -f priv/*.so ebin/*.beam
