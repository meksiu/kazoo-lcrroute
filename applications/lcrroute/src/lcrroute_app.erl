%%%-------------------------------------------------------------------
%%% @copyright (C) 2015-2035, Open Phone Net
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Urs Rueedi
%%%-------------------------------------------------------------------
-module(lcrroute_app).

-behaviour(application).

-include("lcrroute.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
%%--------------------------------------------------------------------
%% @public
%% @doc
%% Implement the application start behaviour
%% @end
%%--------------------------------------------------------------------
-spec start(any(), any()) ->
                   {'ok', pid()} |
                   {'error', startlink_err()}.
start(_StartType, _StartArgs) -> lcrroute:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Implement the application stop behaviour
%% @end
%%--------------------------------------------------------------------
-spec stop(any()) -> 'ok'.
stop(_State) -> lcrroute:stop().
