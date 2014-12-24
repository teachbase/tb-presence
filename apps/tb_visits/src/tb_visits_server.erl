-module(tb_visits_server).
-behaviour(gen_server).
-behaviour(deliverly_handler).
-include_lib("deliverly/include/deliverly.hrl").
-include_lib("deliverly/include/log.hrl").
-include_lib("tb_visits/include/priv.hrl").
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
  ?D(<<"Staring application: tb_visits">>),
  self() ! register,
  {ok, #state{started_at = ulitos:timestamp()}}.

authorize(Client,_) -> gen_server:call(?SERVER, {authorize, Client}).

handle_message(_,_) -> ok.

handle_client_message(Client, Message) -> gen_server:call(?SERVER, {handle_client_message, Client, Message}).

client_disconnected(#de_client{data=Pid}) -> gen_server:cast(Pid, disconnected).

handle_call({authorize, Client}, _, State) ->
  {ok, Pid} = tb_visits_sup:start_visitor(Client),
  erlang:monitor(process,Pid),
  {reply, {ok, Client#de_client{encoder=json_encoder, data=Pid}}, State};

handle_call({handle_client_message, #de_client{data=Pid}, Data}, _, State) ->
  gen_server:cast(Pid, {handle_message, Data}),
  {reply, ok, State};

handle_call(_Request, _From, State) ->
  {reply, unknown, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(register, State) ->
  ok = deliverly:register_handler(tb_visits, tb_visits_server),
  {noreply, State};

handle_info(_Info, State) ->
  ?D({unknown, _Info}),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.