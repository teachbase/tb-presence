-module(tb_visits_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_visitor/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_visitor(Client) ->
  supervisor:start_child(visitor_sup, [Client]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([visitor]) ->
    {ok, {{simple_one_for_one, 5, 10}, [
        {undefined, {tb_visitor, start_link, []},
            transient, 2000, supervisor, [tb_visitor]}
    ]}};

init([]) ->
  Children = [
    ?CHILD(tb_visits_server, worker),
    {visitor_sup,
      {supervisor,start_link,[{local, visitor_sup}, ?MODULE, [visitor]]},
          permanent,                               
          infinity, 
          supervisor,
          []
    }
  ],
  {ok, { {one_for_one, 5, 10}, Children} }.

