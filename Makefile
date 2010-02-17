compile:
	gcc -fPIC -shared -o priv/iconv.so c_src/iconv.c -I ../otp/erts/emulator/beam/
	../otp/bin/erlc -o ebin src/*.erl

clean:
	rm priv/*.so ebin/*.beam

run:
	../otp/bin/erl -pa ../iconverl/ebin
