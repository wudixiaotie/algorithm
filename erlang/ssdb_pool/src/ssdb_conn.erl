-module (ssdb_conn).

-export ([start_link/4]).

start_link (Pool, Host, Port, Index) ->
    {ok, C} = eredis:start_link (Host, Port),
    ets:insert (Pool, {Index, C}),
    {ok, C}.