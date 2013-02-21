%%Standard interface for iconv libraries (to be used as a drop in replacement)
-module(iconv).

-export([open/2, conv/2, close/1]).

-spec open(string() | binary(), string() | binary()) -> {ok, term()}.
open(ToCode, FromCode) when is_list(ToCode) or is_binary(ToCode),
    is_list(FromCode) or is_binary(FromCode) ->
    iconverl:open(ToCode, FromCode).

-spec conv(term(), binary()) -> {ok, binary} | {error, atom()}.
conv(Cd, Input) ->
    iconverl:conv(Cd, Input).

-spec close(term()) -> ok | {error, atom()}.
close(_Cd) ->
    ok.
