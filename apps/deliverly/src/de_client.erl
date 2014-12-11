%%% @doc 
%%% This module is used to simply send data to clients simply using Client:reply(Data),
%%% where Client is #de_client{} (client()).
%%% @end
-module(de_client).
-include_lib("deliverly/include/deliverly.hrl").
-export([reply/3]).

-spec reply(pid(), Data::any(), Client::client()) -> ok.

reply(_, Data, #de_client{module = M} = Client) ->
  M:reply(Client,Data).