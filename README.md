About
=====

Erlang libiconv binding (uses the new NIF api).

Compiling
=========

Set ERL_TOP environment variable and run make:

<pre>
export ERL_TOP=/path/to/otp/clone
make
</pre>

Using
=====

<pre>
make shell

Eshell V5.7.5  (abort with ^G)
1> CD = iconv:open(<<"ucs-2be">>, <<"utf-8">>).
<<96,31,138,2,0,0,0,0>>
2> Conv = iconv:iconv(CD, <<"text">>).
{ok,<<0,116,0,101,0,120,0,116>>}
3> iconv:close(CD).
ok
</pre>
