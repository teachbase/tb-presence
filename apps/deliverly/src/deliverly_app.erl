-module(deliverly_app).
-include_lib("deliverly/include/deliverly.hrl").
-include_lib("deliverly/include/log.hrl").
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  ?I("Starting application: deliverly"),
  ConfigPath = case ?Config(config,undefined) of
    undefined -> "deliverly.config";
    Else -> Else
  end,
  ulitos_app:load_config(?APP, ConfigPath, ["etc"]),
  deliverly_sup:start_link().

stop(_State) ->
  ok.
