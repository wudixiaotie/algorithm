-module (tc).

-compile (export_all).

t (M, F, A) ->
    t (M, F, A, 5).


t (M, F, A, N) ->
    {Max, Min, Aver} = loop ({M, F, A}, N),
    io:format ("=====================~n"),
    io:format ("execute {~p, ~p ~p} for [~p] times:~n", [M, F, A, N]),
    io:format ("Maximum: ~p(ms)\t~p(s)~n", [Max, Max / 1000000]),
    io:format ("Minimum: ~p(ms)\t~p(s)~n", [Min, Min / 1000000]),
    io:format ("Average: ~p(ms)\t~p(s)~n", [Aver, Aver / 1000000]),
    io:format ("=====================~n").


tc (M, F, A) ->
    {MS, _} = timer:tc (M, F, A),
    MS.


loop ({M, F, A}, N) ->
    loop ({M, F, A}, N, 1, 0, 0, 0).

loop ({M, F, A}, N, I, Max, Min, Sum) when N >= I ->
    MS = tc (M, F, A),
    NewSum = Sum + MS,
    if
        Max == 0 ->
            NewMax = NewMin = MS;
        Max < MS ->
            NewMax = MS,
            NewMin = Min;
        Min > MS ->
            NewMax = Max,
            NewMin = MS;
        true ->
            NewMax = Max,
            NewMin = Min
    end,
    loop ({M, F, A}, N, I + 1, NewMax, NewMin, NewSum);
loop ({_M, _F, _A}, N, _I, Max, Min, Sum) ->
    {Max, Min, Sum / N}.
