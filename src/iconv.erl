-module(iconv).

-on_load(load_nif/0).

-export([open/2, conv/2, conv/3]).

open(To, From) ->
  erlang:error(function_clause, [To, From]).

conv(CD, Binary) ->
  erlang:error(function_clause, [CD, Binary]).

conv(To, From, Binary) ->
  conv(open(To, From), Binary).

load_nif() ->
  Path = filename:join(code:priv_dir(iconverl), "iconv"),
  erlang:load_nif(Path, 0).
