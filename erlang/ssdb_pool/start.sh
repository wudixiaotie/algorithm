rebar compile

case $1 in
    "o" )
        erl -pa ebin/ -pa deps/*/ebin -eval "observer:start ()."
        ;;
    "a" )
        erl -pa ebin/ -pa deps/*/ebin -eval "application:start (ssdb_pool)."
        ;;
    "oa" | "ao" )
        erl -pa ebin/ -pa deps/*/ebin -eval "observer:start ()." -eval "application:start (ssdb_pool)."
        ;;
    "" )
        erl -pa ebin/ -pa deps/*/ebin
        ;;
    * )
        echo "unknown args!"
        ;;
esac
