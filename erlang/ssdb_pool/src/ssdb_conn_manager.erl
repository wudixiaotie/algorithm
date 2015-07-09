-module (ssdb_conn_manager).

-behaviour (gen_server).

% APIs
-export ([start_link/4, get/1, expand/2]).

% gen_server callbacks
-export ([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).

-record (state, {index :: integer (),
                 pool :: pid (),
                 pool_size :: integer (),
                 host :: list (),
                 port :: integer ()}).



%% ===================================================================
%% APIs
%% ===================================================================

start_link (ServerName, Host, Port, PoolSize) ->
    gen_server:start_link ({local, ServerName}, ?MODULE, [ServerName, Host, Port, PoolSize], []).

get (ServerName) ->
    gen_server:call (ServerName, get_conn).

expand (ServerName, N) ->
    gen_server:call (ServerName, {expand_pool, N}).


%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init ([ServerName, Host, Port, PoolSize]) ->
    Pool = ets:new (ServerName, [public]),
    new_connection (Pool, Host, Port, PoolSize),
    State = #state{index = 1, pool = Pool, pool_size = PoolSize, host = Host, port = Port},
    {ok, State}.


handle_call (get_conn, _From, State) ->
    Index = State#state.index rem State#state.pool_size + 1,
    C = ets:lookup_element (State#state.pool, Index, 2),
    {reply, {ok, C}, State#state{index = Index + 1}};
handle_call ({expand_pool, N}, _From,
    #state{pool = Pool, pool_size = PoolSize, host = Host, port = Port} = State) ->
    NewPoolSize = PoolSize + N,
    new_connection (Pool, Host, Port, NewPoolSize, PoolSize + 1),
    NewState = State#state{pool_size = NewPoolSize},
    {reply, ok, NewState};
handle_call (_Request, _From, State) -> {reply, nomatch, State}.


handle_cast (_Msg, State) -> {noreply, State}.
handle_info (_Info, State) -> {noreply, State}.
terminate (_Reason, _State) -> ok.
code_change (_OldVer, State, _Extra) -> {ok, State}.


%% ===================================================================
%% Internal functions
%% ===================================================================
new_connection (Pool, Host, Port, N) ->
    new_connection (Pool, Host, Port, N, 1).
new_connection (Pool, Host, Port, N, Index) when Index =< N ->
    supervisor:start_child (ssdb_conn_sup, [Pool, Host, Port, Index]),
    new_connection (Pool, Host, Port, N, Index + 1);
new_connection (_, _, _, _, _) -> ok.