%%%-------------------------------------------------------------------
%%% @copyright (C) 2015-2035, Open Phone Net
%%% @doc
%%% Given a lcrrate_req, find appropriate lcrrate for the call
%%% @end
%%% @contributors
%%%   Urs Rueedi
%%%-------------------------------------------------------------------
-module(lcr_rater).

-export([init/0, handle_req/2]).
-export([get_lcrrate_data/1]).

-define(MOD_CONFIG_CAT, <<"notify">>).
-include("lcrroute.hrl").

init() -> whapps_maintenance:refresh(?WH_LCRRATES_DB).

-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = wapi_lcrrate:req_v(JObj),
    _ = wh_util:put_callid(JObj),
    lager:debug("valid lcr-rating request"),
    case get_lcrrate_data(JObj) of
        {'error', 'no_lcrrate_found'} ->
            maybe_publish_no_lcrrate_found(JObj);
        {'ok', Resp} ->
            wapi_lcrrate:publish_resp(wh_json:get_value(<<"Server-ID">>, JObj)
                                   ,props:filter_undefined(Resp)
                                  ),
            wapi_lcrrate:broadcast_resp(props:filter_undefined(Resp))
    end.

-spec maybe_publish_no_lcrrate_found(wh_json:object()) -> 'ok'.
maybe_publish_no_lcrrate_found(JObj) ->
    case wh_json:is_true(<<"Send-Empty">>, JObj, 'false') of
        'true' -> publish_no_lcrrate_found(JObj);
        'false' -> 'ok'
    end.

-spec publish_no_lcrrate_found(wh_json:object()) -> 'ok'.
publish_no_lcrrate_found(JObj) ->
    MsgId = wh_json:get_value(<<"Msg-ID">>, JObj),
    ServerId = wh_json:get_value(<<"Server-ID">>, JObj),

    Resp = [{<<"Msg-ID">>, MsgId}
            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    lager:debug("publishing empty lcrrate resp for ~s(~s)", [ServerId, MsgId]),
    wh_amqp_worker:cast(Resp, fun(P) -> wapi_lcrrate:publish_resp(ServerId, P) end).

-spec get_lcrrate_data(wh_json:object()) ->
                           {'ok', wh_proplist()} |
                           {'error', 'no_lcrrate_found'}.
get_lcrrate_data(JObj) ->
    ToDID = wh_json:get_value(<<"To-DID">>, JObj),
    FromDID = wh_json:get_value(<<"From-DID">>, JObj),
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    Direction = wh_json:get_value(<<"Direction">>, JObj, <<"any">>),
    lager:debug("candidate lcrrates to:~s from:~s Acc:~s Dir:~s", [ToDID, FromDID, AccountId, Direction]),
    case lcr_util:candidate_lcrrates(ToDID, FromDID, AccountId, Direction) of
        {'ok', []} ->
            case whapps_config:get_is_true(?MOD_CONFIG_CAT, <<"suppress_lcrrater_system_alert">>, 'false') of
                'true' -> {'error', 'no_lcrrate_found'};
                'false' ->
                    wh_notify:system_alert("no lcrrate found for ~s to ~s", [FromDID, ToDID]),
                    lager:debug("no lcrrates found for ~s to ~s", [FromDID, ToDID]),
                    {'error', 'no_lcrrate_found'}
            end;
        {'error', _E} ->
            case whapps_config:get_is_true(?MOD_CONFIG_CAT, <<"suppress_lcrrater_system_alert">>, 'false') of
                'true' -> {'error', 'no_lcrrate_found'};
                'false' ->
                    wh_notify:system_alert("no lcrrate found for ~s to ~s", [FromDID, ToDID]),
                    lager:debug("lcrrate lookup error for ~s to ~s: ~p"
                            ,[FromDID, ToDID, _E]
                           ),
                    {'error', 'no_lcrrate_found'}
            end;
        {'ok', Rates} ->
            get_lcrrate_data(JObj, ToDID, FromDID, Rates)
    end.

-spec get_lcrrate_data(wh_json:object(), ne_binary(), api_binary(), wh_json:objects()) ->
                           {'ok', api_terms()} |
                           {'error', 'no_lcrrate_found'}.
get_lcrrate_data(JObj, ToDID, FromDID, Rates) ->
    lager:debug("candidate lcrrates found, filtering"),
    RouteOptions = wh_json:get_value(<<"Options">>, JObj, []),
    RouteFlags   = wh_json:get_value(<<"Outbound-Flags">>, JObj, []),
    Direction    = wh_json:get_first_defined([<<"Direction">>], JObj),
    lager:debug("Rates:~p N:~p D:~p RO~p",[Rates, ToDID, Direction, RouteOptions++RouteFlags]),
    Matching     = lcr_util:matching_lcrrates(Rates, ToDID, RouteOptions++RouteFlags),
    case lcr_util:sort_lcrrates(Matching) of
        [] ->
            case whapps_config:get_is_true(?MOD_CONFIG_CAT, <<"suppress_lcrrater_system_alert">>, 'false') of
                'true' -> {'error', 'no_lcrrate_found'};
                'false' ->
                    wh_notify:system_alert("no lcrrate found after filter/sort for ~s to ~s", [FromDID, ToDID]),
                    lager:debug("no lcrrates left for ~s to ~s after filter"
                            ,[FromDID, ToDID]
                           ),
                    {'error', 'no_lcrrate_found'}
            end;
        [SortRates|_] ->
            Resources = lists:map(fun(Rate) -> 
                    lager:debug("Using LCR-Rate:~s CarrierId:~s for ~s to ~s"
                        ,[ wh_json:get_binary_value(<<"rate_cost">>, Rate)
                          ,wh_json:get_binary_value(<<"carrierid">>, Rate)
                          ,FromDID
                          ,ToDID
                         ]
                       ),
                        wh_json:get_binary_value([<<"carrierid">>], Rate) end, SortRates
            ),
            ResourceList = remove_dups(Resources),
            lager:debug("ResourceList:~p",[ResourceList]),

            {'ok', lcrrate_resp(ResourceList, JObj)}
    end.

    remove_dups([])    -> [];
    remove_dups([H|T]) -> [H | [X || X <- remove_dups(T), X /= H]].

-spec lcrrate_resp(wh_json:object(), wh_json:object()) -> wh_proplist().
lcrrate_resp(ResourceList, JObj) ->
    [
     {<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
     ,{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
     ,{<<"Gateways">>, [ResourceList]}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)].
