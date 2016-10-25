-module(tb_visitor).
-behaviour(gen_server).
-include_lib("deliverly/include/deliverly.hrl").
-include_lib("deliverly/include/log.hrl").
-include_lib("tb_visits/include/priv.hrl").
-define(SERVER, ?MODULE).

-define(TAGS, [
  <<"quiz_id">>, 
  <<"course_session_id">>, 
  <<"course_id">>,
  <<"account_id">>,
  <<"material_id">>, 
  <<"meeting_id">>,
  <<"user_id">>
]).

-define(INT_VALUES, [
  <<"question_id">>, 
  <<"document_id">>, 
  <<"field_user_id">>,
  <<"field_account_id">>
]).

-define(FLOAT_VALUES, [
  <<"time_spent">>
]).

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

write_message(Measurement, Msg) ->
  M = prepare_msg(Measurement, Msg),
  ?D(M),
  influx_udp:write(M).

prepare_msg(Measurement, Msg) ->
  NewMsg = #{measurement => Measurement},
  AddKey = 
    fun(Key, Map) ->
      NewMap = 
        case maps:is_key(Key, Msg) of
          true -> Map#{Key => maps:get(Key, Msg)};
          _ -> Map
        end,
      NewMap
    end,
  Tags = lists:foldl(AddKey, #{}, ?TAGS),
  IntValues = lists:foldl(AddKey, #{}, ?INT_VALUES),
  FloatValues = maps:map(fun(_, Value) -> float(Value) end, lists:foldl(AddKey, #{}, ?FLOAT_VALUES)),
  NewMsg#{tags => Tags, fields => maps:merge(IntValues, FloatValues)}.
