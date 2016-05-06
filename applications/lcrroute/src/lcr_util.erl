%%%-------------------------------------------------------------------
%%% @copyright (C) 2015-2035, Open Phone Net
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Urs Rueedi
%%%-------------------------------------------------------------------
-module(lcr_util).

-export([candidate_lcrrates/1, candidate_lcrrates/2, candidate_lcrrates/3, candidate_lcrrates/4
         ,matching_lcrrates/2, matching_lcrrates/3
         ,sort_lcrrates/1
         ,sort_lcrrate/2
         ,find_lcrrates_dbs/2
         ,lcrrates_for_did/1, lcrrates_for_did/2
         ,lcrrates_for_did/3, lcrrates_for_did/4
        ]).

-ifdef(TEST).
-export([build_keys/1]).
-endif.

-include("lcrroute.hrl").

-define(MIN_PREFIX_LEN, 1). % how many chars to strip off the e164 DID
-define(BOTH_DIRECTIONS, [<<"inbound">>, <<"outbound">>]).

-spec candidate_lcrrates(ne_binary()) ->
                             {'ok', wh_json:objects()} |
                             {'error', atom()}.
-spec candidate_lcrrates(ne_binary(), binary()) ->
                             {'ok', wh_json:objects()} |
                             {'error', atom()}.
-spec candidate_lcrrates(ne_binary(), binary(), api_binary()) ->
                            {'ok', wh_json:objects()} |
                            {'error', atom()}.
-spec candidate_lcrrates(ne_binary(), binary(), api_binary(), api_binary()) ->
                            {'ok', wh_json:objects()} |
                            {'error', atom()}.
candidate_lcrrates(ToDID) ->
    candidate_lcrrates(ToDID, <<>>).
candidate_lcrrates(ToDID, FromDID) ->
    candidate_lcrrates(ToDID, FromDID, <<>>).
candidate_lcrrates(ToDID, FromDID, AccountId) ->
    E164 = wnm_util:to_e164(ToDID, AccountId),
    find_candidate_lcrrates(E164, FromDID, AccountId, <<>>).
candidate_lcrrates(ToDID, FromDID, AccountId, Direction) ->
    E164 = wnm_util:to_e164(ToDID, AccountId),
    find_candidate_lcrrates(E164, FromDID, AccountId, Direction).

%% ----------- Check first Account-lcrratedeck then global lcrratedeck ---------------------

find_candidate_lcrrates(E164, _FromDID, AccountId, Direction) when byte_size(E164) > ?MIN_PREFIX_LEN ->
    Keys = build_keys(Direction, E164),
    ResellerId = wh_services:find_reseller_id(AccountId),
    lager:debug("Acc:~s,Res:~s,Keys:~p", [AccountId,ResellerId,Keys]),
    RateDbs = find_lcrrates_dbs(AccountId, ResellerId),
    lager:debug("candidate this RateDbs:~p", [RateDbs]),
    case lists:foldl(fun acc_candidate_lcrrates/2, {'ok', Keys, 0, []}, RateDbs) of
        {'ok', _, _, Rates} -> {'ok', Rates};
        E -> lager:debug("Error:~p ", [E]), E
    end;
find_candidate_lcrrates(_, _, DID, _) ->
    lager:debug("DID ~s is too short", [DID]),
    {'error', 'did_too_short'}.

build_keys(Direction, <<"+", E164/binary>>) ->
    build_keys(Direction, E164);
build_keys(Direction, <<D:1/binary, Rest/binary>>) ->
    build_keys(Direction, Rest, D, [[D, Direction]]).

build_keys(Direction, <<D:1/binary, Rest/binary>>, Prefix, Acc) ->
    build_keys(Direction, Rest, <<Prefix/binary, D/binary>>, [[<<Prefix/binary, D/binary>>, Direction] | Acc]);
build_keys(_, <<>>, _, Acc) -> Acc.

-spec find_lcrrates_dbs(ne_binary(), ne_binary()) -> list().
find_lcrrates_dbs([], _) ->
    [?WH_LCRRATES_DB];
find_lcrrates_dbs(AccountId, ResellerId) ->
    AccDb = <<?WH_LCRRATES_DB/binary, "-", AccountId/binary>>,
    ResDb = <<?WH_LCRRATES_DB/binary, "-", ResellerId/binary>>,
    case couch_mgr:db_exists(AccDb) of
        'true' ->
            lager:debug("Db ~s is found, check also ResDb:~s", [AccDb, ResDb]),
            case couch_mgr:db_exists(ResDb) of
                'true' ->
                    lager:debug("ResDb ~s is found return:~p", [ResDb,[?WH_LCRRATES_DB,ResDb,AccDb]]),
                    [?WH_LCRRATES_DB,ResDb,AccDb];
                'false' ->
                    lager:debug("ResDb ~s is not found take: ~p", [ResDb, [?WH_LCRRATES_DB,AccDb]]),
                    [?WH_LCRRATES_DB,AccDb]
            end;
        'false' ->
            lager:debug("Db ~s not found", [AccDb]),
            case couch_mgr:db_exists(ResDb) of
                'true' ->
                    lager:debug("ResDb ~s is found return:~p", [ResDb,[?WH_LCRRATES_DB,AccDb]]),
                    [?WH_LCRRATES_DB,AccDb];
                'false' ->
                    lager:debug("ResDb ~s is not found take: ~p", [ResDb, [?WH_LCRRATES_DB]]),
                    [?WH_LCRRATES_DB]
            end
    end.

-spec acc_candidate_lcrrates(ne_binary(), {'ok', list(), integer(), list()} | {'error', any()}) ->
    {'ok', list(), integer(), list()} | {'error', any()}.
acc_candidate_lcrrates(RateDb, {'ok', Keys, Relevance, Acc}) ->
    case couch_mgr:get_results(RateDb, <<"lcrrates/lookup">>, [{'keys', Keys}, 'include_docs']) of
        {'ok', ViewRows} ->
            {'ok', Keys, Relevance + 1, Acc ++ [normalize_lcrrate(Rate, Relevance) || Rate <- ViewRows]};
        {'error', _}=E -> E
    end;
acc_candidate_lcrrates(_Db, State) ->
    State.

normalize_lcrrate(Rate, Relevance) ->
    Rate1 = wh_json:get_value(<<"value">>, Rate),
    Rate2 = wh_json:merge_jobjs(Rate1, wh_json:get_value(<<"doc">>, Rate)),
    wh_json:set_value(<<"relevance">>, Relevance, Rate2).

%% Given a list of lcrrates, return the list of lcrrates whose routes regexes match the given E164
%% Optionally include direction of the call and options from the client to match against the lcrrate
-spec matching_lcrrates(wh_json:objects(), ne_binary()) ->
                            wh_json:objects().
-spec matching_lcrrates(wh_json:objects(), ne_binary(), trunking_options()) ->
                            wh_json:objects().
matching_lcrrates(Rates, DID) ->
    matching_lcrrates(Rates, DID, []).

matching_lcrrates(Rates, DID, RouteOptions) ->
    E164 = wnm_util:to_e164(DID),
    [Rate || Rate <- Rates,
             matching_lcrrate(Rate, E164, RouteOptions)
    ].

-spec sort_lcrrates(wh_json:objects()) -> wh_json:objects().
sort_lcrrates(AllRates) ->
    SortRates = lists:usort(fun sort_lcrrate/2, AllRates),
    Resources = lists:map(fun(Rate) -> wh_json:get_binary_value([<<"carrierid">>], Rate) end, SortRates),
    ResourceList = remove_dups(Resources),
    lager:debug("List:~p",[ResourceList]),
    [SortRates|ResourceList].

    remove_dups([])    -> [];
    remove_dups([H|T]) -> [H | [X || X <- remove_dups(T), X /= H]].

%% Private helper functions
%% Return whether the given lcrrate is a candidate for the given DID
%% taking into account direction of the call and options the DID
%% needs to have available
-spec matching_lcrrate(wh_json:object(), ne_binary(), trunking_options()) -> boolean().
matching_lcrrate(Rate, E164, RouteOptions) ->
        matching_options(Rate, RouteOptions)
        andalso matching_routes(Rate, E164).

-spec matching_routes(wh_json:object(), ne_binary()) -> boolean().
matching_routes(Rate, E164) ->
        lists:any(fun(Regex) -> 
%%lager:debug("Regex:~s",[Regex]), 
                re:run(E164, Regex) =/= 'nomatch' end
              ,wh_json:get_value([<<"routes">>], Rate, [])
             ).

%% Return true if RateA has lower weight than RateB
-spec sort_lcrrate(wh_json:object(), wh_json:object()) -> boolean().
sort_lcrrate(RateA, RateB) ->
    RouteA = byte_size(wh_json:get_binary_value(<<"prefix">>, RateA)),
    RouteB = byte_size(wh_json:get_binary_value(<<"prefix">>, RateB)),
    RaA = wh_json:get_value(<<"rate_cost">>, RateA),
    RaB = wh_json:get_value(<<"rate_cost">>, RateB),
%%    lager:debug("Sort RaA:~s RaB:~s",[RaA,RaB]),
    case RouteA =:= RouteB of
        'true' ->
            case wh_json:get_integer_value(<<"weight">>, RateA, 100) =:= wh_json:get_integer_value(<<"weight">>, RateB, 100) of
                'true' ->
                    RaA < RaB;
                'false' ->
                    wh_json:get_integer_value(<<"weight">>, RateA, 100) > wh_json:get_integer_value(<<"weight">>, RateB, 100)
            end;
        'false' ->
            RouteA > RouteB
    end.

%% Rate options come from the carrier providing the trunk
%% All Route options must exist in a carrier's options to keep the carrier
%% in the list of carriers capable of handling the call
-spec matching_options(wh_json:object(), trunking_options()) -> boolean().
matching_options(Rate, RouteOptions) ->
    options_match(wh_json:get_value([<<"options">>], Rate, []), RouteOptions).

-spec options_match(trunking_options(), trunking_options()) -> boolean().
options_match([], []) -> 'true';
options_match([], _) -> 'true';
options_match(RateOptions, RouteOptions) ->
    lists:all(fun(RouteOption) ->
                      props:get_value(RouteOption, RateOptions, 'false') =/= 'false'
              end
              ,RouteOptions
             ).

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
        {'ok', []} -> lager:debug("lcrrate lookup had no results~n");
        {'error', _E} -> lager:debug("lcrrate lookup error: ~p~n", [_E]);
        {'ok', Rates} ->
%%            lager:debug("go next matching Rates:~p", [Rates]),
            print_matching(lcr_util:matching_lcrrates(Rates, DID, RouteOptions))
    end;
lcrrates_for_did(AccountId, DID, Direction, Opt) ->
    lcrrates_for_did(AccountId, DID, Direction, [Opt]).

print_matching(Matching) ->
    [[_Winner|_Others]|ResourceList] = lcr_util:sort_lcrrates(Matching),
    ResList = lists:map(fun(Id) ->
                    case couch_mgr:open_doc('offnet', Id) of
                        {'ok', Doc} -> Doc;
                        {'error', _}=E -> E
                    end
                end, ResourceList),
    ResList.
