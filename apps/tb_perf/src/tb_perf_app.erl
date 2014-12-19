-module(tb_perf_app).
-include_lib("tb_perf/include/priv.hrl").
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  tb_perf_sup:start_link().

stop(_State) ->
  ok.