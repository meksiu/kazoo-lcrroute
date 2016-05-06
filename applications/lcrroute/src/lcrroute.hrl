-ifndef(LCRROUTE_HRL).
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-define(APP_NAME, <<"lcrroute">>).
-define(APP_VERSION, <<"0.0.1">>).

-type trunking_options() :: ne_binaries().

-define(LCRROUTE_HRL, 'true').
-endif.
