%%%-------------------------------------------------------------------
%%% @copyright (C) 2015-2035, Open Phone Net
%%% @doc
%%% Helper functions for users to inspect how lcrroute is running
%%% @end
%%% @contributors
%%%   Urs Rueedi
%%%-------------------------------------------------------------------
-module(lcrroute_maintenance).

-export([local_summary/0
         ,lcrrates_for_did/1, lcrrates_for_did/2
         ,lcrrates_for_did/3, lcrrates_for_did/4
         ,lcrrates_between/2
        ]).

-export([to_e164/1, to_e164/2]).

-include("lcrroute.hrl").

-define(LOCAL_SUMMARY_ROW_FORMAT, " ~35.s | ~9.s | ~35.s | ~9.s | ~9.s | ~9.s | ~9.s | ~9.s | ~9.s |~n").
-define(LOCAL_SUMMARY_HEADER, io:format(?LOCAL_SUMMARY_ROW_FORMAT, [<<"RATE NAME">>, <<"COST">>, <<"CARRIERID">>, <<"INCREMENT">>, <<"MINIMUM">>
                                                                    ,<<"SURCHARGE">>, <<"WEIGHT">>, <<"PREFIX">>, <<"DIRECTION">>
                                                                   ])).

-spec to_e164(ne_binary()) -> 'ok'.
-spec to_e164(ne_binary(), api_binary()) -> 'ok'.
to_e164(DID) -> 
    wnm_util:to_e164(DID).

to_e164(DID, AccountId) ->
    wnm_util:to_e164(DID, AccountId).

-spec local_summary() -> 'ok'.
local_summary() ->
    io:format("use lcrrates_for_did/1 to see what lcrrates would be used for a DID").

-spec lcrrates_for_did(ne_binary()) -> 'ok'.
-spec lcrrates_for_did(ne_binary(), api_binary()) -> 'ok'.
-spec lcrrates_for_did(ne_binary(), ne_binary(), api_binary()) -> 'ok'.
-spec lcrrates_for_did(ne_binary(), ne_binary(), api_binary(), trunking_options()) -> 'ok'.
lcrrates_for_did(DID) ->
    lcrrates_for_did(DID, <<"outbound">>).
lcrrates_for_did(DID, Direction) ->
    lcrrates_for_did(DID, Direction, []).
lcrrates_for_did(DID, Direction, AccountId) ->
    lcrrates_for_did(DID, Direction, AccountId, []).
lcrrates_for_did(DID, Direction, AccountId, RouteOptions) when is_list(RouteOptions) ->
    lager:debug("DID:~p, Dir:~p, Account:~p, Opt:~p", [DID, Direction, AccountId, RouteOptions]),
    case lcr_util:candidate_lcrrates(DID, <<>>, AccountId, Direction) of
        {'ok', []} -> io:format("lcrrate lookup had no results~n");
        {'error', _E} -> io:format("lcrrate lookup error: ~p~n", [_E]);
        {'ok', Rates} ->
            io:format("Candidates:~n", []),
            ?LOCAL_SUMMARY_HEADER,
            _ = [print_lcrrate(R) || R <- Rates],
%%            lager:debug("go next matching Rates:~p", [Rates]),
            print_matching(lcr_util:matching_lcrrates(Rates, DID, RouteOptions))
    end;
lcrrates_for_did(AccountId, DID, Direction, Opt) ->
    lcrrates_for_did(AccountId, DID, Direction, [Opt]).

-spec print_matching(wh_json:objects()) -> 'ok'.
print_matching([]) ->
    io:format("no lcrrates matched~n", []);
print_matching(Matching) ->
    io:format("Matching:~n", []),
    ?LOCAL_SUMMARY_HEADER,

    [[Winning|Sorted]|ResourceList] = lcr_util:sort_lcrrates(Matching),
    Name = wh_json:get_value(<<"rate_name">>, Winning),

    _ = [print_lcrrate(R)
         || R <- [wh_json:set_value(<<"rate_name">>, <<"1 ", Name/binary>>, Winning)
                  | Sorted]
        ],
            print_carrier_matching(ResourceList),
    'ok'.

-spec lcrrates_between(ne_binary(), ne_binary()) -> 'ok'.
lcrrates_between(Pre, Post) ->
    ViewOpts = [{'startkey', wh_util:to_binary(Pre)}
                ,{'endkey', wh_util:to_binary(Post)}
               ],
    case couch_mgr:get_results(?WH_LCRRATES_DB, <<"lcrrates/lookup">>, ViewOpts) of
        {'ok', []} -> io:format("lcrrate lookup had no results~n");
        {'error', _E} -> io:format("lcrrate lookup error: ~p~n", [_E]);
        {'ok', Rates} ->
            io:format("lcrrates between:~n", []),
            ?LOCAL_SUMMARY_HEADER,
            _ = [print_lcrrate(wh_json:get_value(<<"value">>, R)) || R <- Rates],
            'ok'
    end.

-spec print_lcrrate(wh_json:object()) -> 'ok'.
print_lcrrate(JObj) ->
    io:format(?LOCAL_SUMMARY_ROW_FORMAT, [wh_json:get_binary_value(<<"rate_name">>, JObj)
                                          ,wh_json:get_binary_value(<<"rate_cost">>, JObj)
                                          ,wh_json:get_binary_value(<<"carrierid">>, JObj)
                                          ,wh_json:get_binary_value(<<"rate_increment">>, JObj)
                                          ,wh_json:get_binary_value(<<"rate_minimum">>, JObj)
                                          ,wh_json:get_binary_value(<<"rate_surcharge">>, JObj, <<"0.0">>)
                                          ,wh_json:get_binary_value(<<"weight">>, JObj)
                                          ,wh_json:get_binary_value(<<"prefix">>, JObj)
                                          ,wh_json:get_binary_value(<<"direction">>, JObj)
                                         ]).

-spec print_carrier_matching(wh_json:objects()) -> 'ok'.
print_carrier_matching(Cmatching) ->
    io:format("Carrier sorted resourcelist: ~n", []),
    io:format("                        ~p~n", [Cmatching]).
