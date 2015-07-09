-module(ssdb_pool_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD_SUP(I), {I, {I, start_link, []}, permanent, 5000, supervisor, [I]}).
-define(CHILD(M, Args), {M, {ssdb_conn_manager, start_link, [M|Args]}, permanent, 5000, worker, [ssdb_conn_manager]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    UserHost = "192.168.1.70",
    UserPort = 8888,
    ShowHost = "192.168.1.24",
    ShowPort = 8900,
    {ok, { {one_for_one, 5, 10},
           [?CHILD_SUP (ssdb_conn_sup),
            ?CHILD (ssdb_user_conn, [UserHost, UserPort, 1]),
            ?CHILD (ssdb_show_conn, [ShowHost, ShowPort, 1])]} }.

