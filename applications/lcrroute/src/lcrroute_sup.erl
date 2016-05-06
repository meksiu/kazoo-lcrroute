%%%-------------------------------------------------------------------
%%% @copyright (C) 2015-2035, Open Phone Net AG
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Urs Rueedi
%%%-------------------------------------------------------------------
-module(lcrroute_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, upgrade/0]).

%% Supervisor callbacks
-export([init/1]).

-include("lcrroute.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({'local', ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {'ok', {_, Specs}} = init([]),

    Old = sets:from_list([Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    lists:foreach(fun (Id) ->
                          _ = supervisor:terminate_child(?MODULE, Id),
                          supervisor:delete_child(?MODULE, Id)
                  end, sets:to_list(Kill)),
    lists:foreach(fun(Spec) -> supervisor:start_child(?MODULE, Spec) end, Specs),
    'ok'.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    wh_util:set_startup(),
    {'ok', { {'one_for_one', 2, 5}
             ,[
               ?CACHE('lcrroute_cache')
               ,?WORKER('lcrroute_listener')
              ]
           } }.
