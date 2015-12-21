%% Copyright
-module(tb_meetings).
-include_lib("deliverly/include/log.hrl").
-include_lib("tb_meetings/include/priv.hrl").

-define(APPS, []).

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

deliverly_handler() -> tb_meetings_server.