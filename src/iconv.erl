%%Standard interface for iconv libraries (to be used as a drop in replacement)
-module(iconv).

-export([]).
% -export([open/2, conv/2, close/1]).

% open(ToCode, FromCode) when is_binary(ToCode), is_binary(FromCode) ->
%     open(unicode:characters_to_list(ToCode, latin1), unicode:characters_to_list(FromCode, latin1));
% open(ToCode, FromCode) when is_list(ToCode), is_list(FromCode) ->
%     Cd = iconverl:open(ToCode, FromCode),
%     case element(1, Cd) of
%         error ->
%             Cd;
%         _ ->
%             {ok, Cd}
%     end.

% conv(Cd, Input) ->
%     iconverl:conv(Cd, Input).

% close(_Cd) ->
%     ok.
