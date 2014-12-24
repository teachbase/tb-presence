-module(tb_visits_server_SUITE).
-include_lib("deliverly/include/deliverly.hrl").
-include_lib("deliverly/include/log.hrl").
-include_lib("tb_visits/include/priv.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-define(Client(Id), #de_client{app=?APP, module=tb_visits_server, socket=list_to_pid("<0.0."++Id++">")}).
-define(Pid(Id), list_to_pid(Id)).

-define(ETS, test_tb_visits_ets).

-define(config(K,P), proplists:get_value(K,P)).

-record(point,{
  ts,
  series,
  data
}).

ets_owner() ->
  receive
      stop -> exit(normal);
      _Any -> ets_owner()
  end.

init_per_suite(Config) ->
  Pid = spawn(fun ets_owner/0),
  TabId = ets:new(?ETS, [public, ordered_set, {heir, Pid, []}, {keypos, #point.ts}]),
  lager:start(),
  ulitos_app:set_var(deliverly, http_port, 8888),  
  ulitos_app:set_var(?APP, visits_series, visits),
  [{table,TabId},{table_owner, Pid} | Config].

end_per_suite(Config) ->
  application:stop(lager),
  ?config(table_owner, Config) ! stop.

init_per_group(_, Config) ->
  deliverly:start(),
  tb_visits:start(),
  timer:sleep(200),
  Config.

end_per_group(_,Config) ->
  ets:delete_all_objects(?config(table, Config)),
  tb_visits:stop(),
  deliverly:stop(),
  ok.

init_per_testcase(_, Config) ->
  TabId = ?config(table, Config),
  %% mock influx_udp
  meck:new(influx_udp),
  meck:expect(influx_udp, write, fun(S, D) -> ets:insert(TabId, #point{ts=ulitos:timestamp(), series=S, data=D}) end),
  Config.

end_per_testcase(_,_) ->
  meck:unload(influx_udp),
  ok.


all() ->
  [
    {group, simple},
    {group, client_disconnected_without_messages},
    {group, client_disconnected_with_messages}
  ].

groups() ->
  [
    {simple, [], [reg_app]},
    {
      client_disconnected_without_messages, [sequence], 
      [
        add_client,
        client_disconnected
      ]
    },
    {
      client_disconnected_with_messages, [sequence, {repeat, 10}],
      [
        add_client,
        client_send_message,
        client_disconnected_2
      ]
    }
  ].


reg_app(_) ->
  List = deliverly:apps_list(),
  true = lists:member(tb_visits, List),
  ok.


add_client(_) ->
  {ok, Client} = deliverly_server:auth_client(?Client("11"),[]),
  true = is_pid(Client#de_client.data),
  true = (undefined =/= erlang:process_info(Client#de_client.data)),
  json_encoder = Client#de_client.encoder,
  ok.

client_disconnected(Config) ->
  [Client] = deliverly:connections_list(?APP),
  deliverly_server:client_disconnected(Client),
  timer:sleep(200),
  undefined = erlang:process_info(Client#de_client.data),
  0 = length(ets:tab2list(?config(table, Config))),
  ok.

client_send_message(Config) ->
  [Client] = deliverly:connections_list(?APP),
  deliverly_server:handle_client_message(Client, #{<<"time_spent">> => 0, <<"id">> => 1}),
  timer:sleep(200),
  %% first message is not writable, so
  0 = length(ets:tab2list(?config(table, Config))),
  deliverly_server:handle_client_message(Client, #{<<"time_spent">> => 100, <<"id">> => 1}),
  timer:sleep(200),
  1 = length(ets:tab2list(?config(table, Config))),
  deliverly_server:handle_client_message(Client, #{<<"time_spent">> => 200, <<"id">> => 2}),
  timer:sleep(200),
  Key = ets:last(?config(table, Config)),
  [Msg] = ets:lookup(?config(table, Config), Key),
  visits = Msg#point.series,
  #{<<"time_spent">> := 200} = Msg#point.data,
  #{<<"id">> := 2} = Msg#point.data,
  ok.


client_disconnected_2(Config) ->
  [Client] = deliverly:connections_list(?APP),
  timer:sleep(500),
  %% ensure that one message have already been there 
  2 = length(ets:tab2list(?config(table, Config))),
  deliverly_server:client_disconnected(Client),
  timer:sleep(200),
  3 = length(ets:tab2list(?config(table, Config))),
  
  Key = ets:last(?config(table, Config)),
  [Msg] = ets:lookup(?config(table, Config), Key),
  undefined = erlang:process_info(Client#de_client.data),
  2 = maps:get(<<"id">>, Msg#point.data),
  true = (maps:get(<<"time_spent">>, Msg#point.data, 0) > 500),   
  true = (maps:get(<<"time_spent">>, Msg#point.data, 0) < 1000),   
  ok.