-module (ssdb_conn_sup).

-behaviour (supervisor).

%% API
-export ([start_link/1]).

%% Supervisor callbacks
-export ([init/1]).

%% Helper macro for declaring children of supervisor
-define (CHILD(Mod, Args), {Mod, {Mod, start_link, Args}, permanent, brutal_kill, worker, [Mod]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link (ServerName) ->
    supervisor:start_link({local, ServerName}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init ([]) ->
    {ok, { {simple_one_for_one, 10, 5}, [?CHILD(ssdb_conn, [])] } }.