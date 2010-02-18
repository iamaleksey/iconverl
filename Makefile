ifndef OSTYPE
OSTYPE=$(shell uname -s|awk '{print tolower($$0)}')
endif
ifeq "$(OSTYPE)" "linux"
GCCFLAGS = -fPIC -shared
endif
ifeq "$(OSTYPE)" "darwin"
GCCFLAGS = -fPIC -shared -liconv -flat_namespace -undefined suppress -fno-common
endif


all: ebin/iconv.beam priv/iconv.so


ebin/iconv.beam: src/iconv.erl
	$(ERL_TOP)/bin/erlc -o ebin src/iconv.erl


priv/iconv.so: c_src/iconv.c
	gcc $(GCCFLAGS) -I $(ERL_TOP)/erts/emulator/beam/ -o priv/iconv.so c_src/iconv.c


shell:
	$(ERL_TOP)/bin/erl -pa ../iconverl/ebin


clean:
	rm -f priv/iconv.so ebin/iconv.beam
