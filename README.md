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
1> CD = iconv:open("ucs-2be", "utf-8").
&lt;&lt;&gt;&gt;
2> iconv:conv(CD, &lt;&lt;"text"&gt;&gt;).
{ok,&lt;&lt;0,116,0,101,0,120,0,116&gt;&gt;}
</pre>
