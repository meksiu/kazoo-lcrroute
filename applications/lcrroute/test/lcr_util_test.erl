%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(lcr_util_test).

-include_lib("eunit/include/eunit.hrl").

build_keys_test() ->
    ?assertEqual([1], lcr_util:build_keys(<<"1">>)),
    ?assertEqual([12, 1], lcr_util:build_keys(<<"12">>)),
    ?assertEqual([123, 12, 1], lcr_util:build_keys(<<"123">>)).
