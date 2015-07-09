rebar compile
erl -pa ebin -pa deps/*/ebin -eval "application:start (ssdb_pool)." -eval "observer:start ()."
