-module(tb_meetings_server).
-behaviour(gen_server).
-behaviour(deliverly_handler).
-include_lib("deliverly/include/deliverly.hrl").
-include_lib("deliverly/include/log.hrl").
-define(SERVER, ?MODULE).

%% API Function Exports
-export([start_link/0]).

%% gen_server Function Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%% Deverly Handler callbacks.
-export([authorize/2, handle_message/2, handle_client_message/2, client_disconnected/1]).

-record(state, {
  clients = #{} :: #{non_neg_integer() => de_client:client()}
}).

%% API Function Definitions

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% deliverly_handler Function Definitions

authorize(Client, Data) ->
  case proplists:get_value(<<"meeting_id">>, Data) of
    undefined ->
      {error, 3401};
    Id ->
      case deliverly_utils:auth_from_config(tb_meetings, Client, Data) of
        true -> gen_server:call(?SERVER, {authorize, binary_to_integer(Id), Client});
        false -> {error, 3401}
      end
  end.

handle_message(Data, #{id := Id, type := chat}) ->
  gen_server:call(?SERVER, {handle_message, Id, Data});

handle_message(_, _) -> ok.

handle_client_message(_, _) -> ok.

client_disconnected(Client) -> gen_server:call(?SERVER, {client_disconnected, Client}).

%% gen_server Function Definitions

init(_Args) ->
  ?D(<<"Staring application: tb_meetings">>),
  {ok, #state{}}.

handle_call({authorize, Id, #de_client{} = Client}, _From, #state{clients = Clients} = State) ->
  NewClient = Client#de_client{data = #{id => Id}},
  NewClients = add_client(NewClient, Id, Clients),
  ?D(NewClient),
  {reply, {ok, NewClient}, State#state{clients = NewClients}};

handle_call({client_disconnected, Client}, _From, #state{clients = Clients} = State) ->
  NewClients = delete_client(Client, Clients),
  {reply, ok, State#state{clients = NewClients}};

handle_call({handle_message, Id, Data}, _From, #state{clients = Clients} = State) ->
  de_client:broadcast_to(maps:get(Id, Clients, []), Data),
  {reply, ok, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal Function Definitions

add_client(Client, Id, Clients) ->
  maps:put(Id, [Client | maps:get(Id, Clients, [])], Clients).

delete_client(#de_client{socket = Socket, data = #{id := Id}}, Clients) ->
  case lists:keydelete(Socket, #de_client.socket, maps:get(Id, Clients, [])) of
    [] ->
      maps:remove(Id, Clients);
    ClientList ->
      maps:put(Id, ClientList, Clients)
  end.