%% Copyright
-module(tb_perf).
-author("palkan").
-include_lib("deliverly/include/log.hrl").
-include_lib("tb_perf/include/priv.hrl").

-define(APPS, [influx_udp]).

%% ------------------------------------------------------------------
%% Common Application Function Exports
%% ------------------------------------------------------------------

-export([start/0, stop/0, upgrade/0]).
-export([deliverly_handler/0]).

start() ->
  ulitos_app:ensure_started(?APPS),
  application:start(?APP).

stop() ->
  application:stop(?APP).

upgrade() ->
 ulitos_app:reload(?APP),
 ok.

deliverly_handler() -> tb_perf_server.