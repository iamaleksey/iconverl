<pre>
  ┌─────────────────────────────────────────────────────────────┐
  │▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓│
  │▓                                                           ▓│
  │▓  ██  ██▀▀██  ██▀██  ███ ██ ██    ██ ██████ ██▀▀██ ██      ▓│
  │▓  ██ ██    ▀ ██   ██ ██████  ██  ██  ██▄▄   ██▄▄██ ██      ▓│
  │▓  ██ ██      ██   ██ ██ ███   ████   ██▀▀   ██▀██  ██   ▄  ▓│
  │▓  ██  ██▄▄██  ██▄██  ██  ██    ██    ██████ ██  ██ ██▄▄██  ▓│
  │▓                                    NOW WITH STREAMING !!! ▓│
  │▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓│
  └─────────────────────────────────────────────────────────────┘
</pre>

About
=====

I needed access to iconv from glibc. After trying the very buggy erlang-iconv (it segfaults). I tried iconverl it seemed to work great, until we (my colleague and I) ran into some problems where it would sometimes return {error, eilseq} instead of the result for the same input (one that was previously successful). 

This prompted me to write a replacement. I tried to get it pulled by the original author since it implements an almost identical interface. But he decided that there was no bug since he couldn't reproduce it on his system!? This has now occured on two completely different environments (Ubuntu 12.04 LTS 64bit and Gentoo). 

This new version adds support for streaming. You can now have low level access to iconverl:iconv/2 in case you really need it or you can use the iconverl:chunk/2 function for a simpler interface.

Compilation
===========

<pre>
make
</pre>

Usage
=====

<pre>
1> CD = iconverl:open("ucs-2be", "utf-8").
{ok, &lt;&lt;&gt;&gt;}
2> iconverl:conv(CD, &lt;&lt;"text"&gt;&gt;).
{ok,&lt;&lt;0,116,0,101,0,120,0,116&gt;&gt;}
3> iconverl:conv(CD, &lt;&lt;"more text to convert"&gt;&gt;).
{ok,&lt;&lt;0,109,0,111,0,114,0,101,0,32,0,116,0,101,0,120,0,
      116,0,32,0,116,0,111,0,32,0,...&gt;&gt;}
4> iconverl:conv("ucs-4", "latin1", &lt;&lt;"convert with a single function call"&gt;&gt;).
{ok,&lt;&lt;0,0,0,99,0,0,0,111,0,0,0,110,0,0,0,118,0,0,0,101,0,
      0,0,114,0,0,0,...&gt;&gt;}

</pre>

Or using the standard iconv interface (recommended):
<pre>
1&gt; l(iconv).<br />{module,iconv}<br />2&gt; {ok, Cd} = iconv:open("latin1//translit", "utf-8").<br />{ok,&lt;&lt;&gt;&gt;}<br />3&gt; iconv:conv(Cd, unicode:characters_to_binary("Test&forall;", utf8)).<br />{ok,&lt;&lt;"Test?"&gt;&gt;}<br />4&gt; iconv:close(Cd).<br />ok<br />5&gt;
</pre>



