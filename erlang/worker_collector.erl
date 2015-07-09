-module (worker_collector).

-export ([start/0]).

-record (point, {x :: integer (), y :: integer ()}).

start () ->
    Count = 1000,
    CollectorPid = self (),
    spawn_worker (Count, CollectorPid),
    collector (Count).

get_info_from_db () ->
    <<A:32, B:32, C:32>> = crypto:strong_rand_bytes (12),
    random:seed (A, B, C),
    Random = random:uniform(1000),
    {ok, Random}.

spawn_worker (Count, CollectorPid) ->
    io:format ("=============start spawning worker~n"),
    spawn_worker (0, Count, CollectorPid).
spawn_worker (Current, Count, CollectorPid) when Current < Count ->
    spawn_link (fun () -> worker (CollectorPid) end),
    spawn_worker (Current + 1, Count, CollectorPid);
spawn_worker (_, _, _) ->
    io:format ("=============finish spawning worker~n"),
    ok.

worker (CollectorPid) ->
    {ok, X} = get_info_from_db (),
    {ok, Y} = get_info_from_db (),
    Data = #point{x = X, y = Y},
    CollectorPid ! {ok, Data}.

collector (Count) ->
    io:format ("=============start collecting data~n"),
    collector (0, Count, []).
collector (Receive, Count, Result) when Receive < Count ->
    receive
        {ok, Data} ->
            collector (Receive + 1, Count, [Data|Result]);
        _ ->
            ok
    end;
collector (_Receive, _Count, Result) ->
    F = fun (Point1, Point2) ->
        (Point1#point.x + Point1#point.y) < (Point2#point.x + Point2#point.y)
    end,
    SortedResult = lists:sort (F, Result),
    io:format ("=============finish collecting data~n"),
    {ok, SortedResult}.