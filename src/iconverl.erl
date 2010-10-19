%%% Copyright (c) 2010 Aleksey Yeschenko <aleksey@yeschenko.com>
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


-export([open/2, conv/2, conv/3]).


-opaque cd() :: binary().


%% -------------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------------


-spec open/2 :: (string(), string()) -> cd().

open(To, From) ->
    erlang:error(function_clause, [To, From]).


-spec conv/2 :: (cd(), binary()) -> {'ok', binary()} | {'error', atom()}.

conv(CD, Binary) ->
    erlang:error(function_clause, [CD, Binary]).


-spec conv/3 :: (string(), string(), binary()) -> {'ok', binary()} | {'error', atom()}.

conv(To, From, Binary) ->
    conv(open(To, From), Binary).


%% -------------------------------------------------------------------------
%% on_load callback
%% -------------------------------------------------------------------------


load_nif() ->
    Path = filename:join(code:priv_dir(iconverl), "iconverl"),
    erlang:load_nif(Path, 0).
