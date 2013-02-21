%%% Copyright (c) 2013 Eric des Courtis <eric.des.courtis@benbria.ca>
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.

-module(iconverl).

-on_load(load_nif/0).

-export([open/2, iconv/2, reset/1]).

-opaque cd() :: binary().
-export_type([cd/0]).

%% -------------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------------
-spec open(string(), string()) -> {ok, cd()}.
open(_To, _From) ->
    erlang:nif_error(not_loaded).

-spec iconv(cd(), binary()) -> 
    {ok, e2big, integer(), binary()} |
    {ok, eilseq, integer(), binary()} |
    {ok, einval, integer(), binary()} |
    {ok, integer(), binary()} |
    {error, atom()} | 
    {error, integer()}.
iconv(_Cd, _Binary) ->
    erlang:nif_error(not_loaded).

-spec reset(cd()) -> ok | {error, integer()}.
reset(_Cd) ->
    erlang:nif_error(not_loaded).

%% -------------------------------------------------------------------------
%% on_load callback
%% -------------------------------------------------------------------------

load_nif() ->
    erlang:load_nif(filename:join(code:priv_dir(iconverl), "iconverl"), 0).
