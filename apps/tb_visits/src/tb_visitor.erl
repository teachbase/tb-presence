-module(tb_visitor).
-behaviour(gen_server).
-include_lib("deliverly/include/deliverly.hrl").
-include_lib("deliverly/include/log.hrl").
-include_lib("tb_visits/include/priv.hrl").
-define(SERVER, ?MODULE).

%% API Function Exports
-export([start_link/1]).
%% gen_server Function Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([write_message/2]).

-record(state, {
  series ::atom(),
  active = false ::boolean(),
  last_ts =0 ::non_neg_integer(),
  msg = undefined ::any()
}).

start_link(Client) ->
  gen_server:start_link(?MODULE, [Client], []).

init([_Client]) ->
  {ok, #state{series=?Config(visits_series, visits)}}.

handle_call(_Request, _From, State) ->
  {reply, unknown, State}.

handle_cast({handle_message, Message}, #state{msg=undefined}=State) ->
  {noreply, State#state{msg=Message, last_ts=ulitos:timestamp()}};

handle_cast({handle_message, #{<<"active">> := Active} = Message}, #state{active=ActiveWas}=State) when Active /= ActiveWas ->
  handle_cast({handle_message, Message}, State#state{active = Active});

handle_cast({handle_message, Message}, #state{series=Series}=State) ->
  write_message(Series, Message),
  {noreply, State#state{msg=Message, last_ts=ulitos:timestamp()}};

handle_cast(disconnected, #state{msg=undefined}=State) ->
  {stop, normal, State};

handle_cast(disconnected, #state{active=false}=State) ->
  {stop, normal, State};

handle_cast(disconnected, #state{series=Series, msg=Msg, last_ts=Time, active=true}=State) ->
  TimeSpent = ulitos:timestamp() - Time,
  NewMsg = maps:update(<<"time_spent">>, TimeSpent, Msg),
  write_message(Series, NewMsg),
  {stop, normal, State};  

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(register, State) ->
  ok = deliverly:register_handler(tb_visits, tb_visits_server),
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%%% internal

write_message(Series, Msg) ->
  influx_udp:write(Series, Msg).