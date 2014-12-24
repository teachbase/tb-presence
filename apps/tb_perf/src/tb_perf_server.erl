-module(tb_perf_server).
-behaviour(gen_server).
-behaviour(deliverly_handler).
-include_lib("deliverly/include/deliverly.hrl").
-include_lib("deliverly/include/log.hrl").
-include_lib("tb_perf/include/priv.hrl").
-define(SERVER, ?MODULE).

%% API Function Exports
-export([start_link/0]).
%% gen_server Function Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
%%% Deverly Handler callbacks.
-export([authorize/2, handle_message/2, handle_client_message/2, client_disconnected/1]).

-record(state, {
  started_at =0 ::non_neg_integer()
}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  ?D(<<"Staring application: tb_perf">>),
  self() ! register,
  {ok, #state{started_at = ulitos:timestamp()}}.

authorize(Client,_) -> {ok, Client}.

handle_message(_,_) -> ok.

handle_client_message(_Client, {text, Message}) -> gen_server:call(?SERVER, {handle_client_message, Message});

handle_client_message(_, _) -> ok.

client_disconnected(_) -> ok.

handle_call({handle_client_message, Data}, _, State) ->
  influx_udp:write(Data),
  {reply, ok, State};

handle_call(_Request, _From, State) ->
  {reply, unknown, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(register, State) ->
  ok = deliverly:register_handler(tb_perf, tb_perf_server),
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.