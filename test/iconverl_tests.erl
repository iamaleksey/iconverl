-module(iconverl_tests).


-include_lib("eunit/include/eunit.hrl").


open_test() ->
    ?assertError(badarg, iconverl:open("utf-99", "ucs-34")),
    ?assert(is_binary(iconverl:open("utf-8", "ucs-2be"))).


conv2_test() ->
    CD = iconverl:open("ucs-2be", "utf-8"),
    ?assertEqual(
        {ok, <<0,$t,0,$e,0,$s,0,$t>>},
        iconverl:conv(CD, <<"test">>)
    ),
    ?assertEqual(
        {error, eilseq},
        iconverl:conv(CD, <<129,129>>)
    ).


conv3_test() ->
    ?assertError(badarg, iconverl:open("utf-99", "ucs-34")),
    ?assertEqual(
        {ok, <<0,$t,0,$e,0,$s,0,$t>>},
        iconverl:conv("ucs-2be", "utf-8", <<"test">>)
    ),
    ?assertEqual(
        {error, eilseq},
        iconverl:conv("ucs-2be", "utf-8", <<129,129>>)
    ).
