-module(tb_visits_app).
-include_lib("deliverly/include/log.hrl").
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  deliverly:register_handler(tb_visits, tb_visits_server),
  tb_visits_sup:start_link().

stop(_State) ->
    ok.
