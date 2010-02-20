About
=====

Erlang libiconv binding (uses the new NIF api).

Compilation
===========

Set ERL_TOP environment variable and run make:

<pre>
export ERL_TOP=/path/to/otp/clone
make
</pre>

Usage
=====

<pre>
make shell

Eshell V5.7.5  (abort with ^G)
1> CD = iconv:open("ucs-2be", "utf-8").
&lt;&lt;&gt;&gt;
2> iconv:conv(CD, &lt;&lt;"text"&gt;&gt;).
{ok,&lt;&lt;0,116,0,101,0,120,0,116&gt;&gt;}
3> iconv:conv(CD, &lt;&lt;"more text to convert"&gt;&gt;).
{ok,&lt;&lt;0,109,0,111,0,114,0,101,0,32,0,116,0,101,0,120,0,
      116,0,32,0,116,0,111,0,32,0,...&gt;&gt;}
4> iconv:conv("ucs-4", "latin1", &lt;&lt;"convert with a single function call"&gt;&gt;).
{ok,&lt;&lt;0,0,0,99,0,0,0,111,0,0,0,110,0,0,0,118,0,0,0,101,0,
      0,0,114,0,0,0,...&gt;&gt;}
</pre>
