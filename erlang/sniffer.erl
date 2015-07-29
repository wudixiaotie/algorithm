-module (sniffer).

-export ([get/2, get_greater_than/2, get_top/2]).

%% ===================================================================
%% apis
%% ===================================================================


get(Pid, pmql) ->
    pmql(Pid);
get(Pid, reductions) ->
    reductions(Pid);
get(_, _) ->
    nofunc.

% list process who's message queue length greater than N
get_greater_than(Name, N) ->
    PList = erlang:processes(),
    get_greater_than(Name, PList, [], N).
get_greater_than(Name, [H|T], Result, N) ->
    Len = get(H, Name),
    case Len >= N of
        true ->
            get_greater_than(Name, T, [{H, Name, Len}|Result], N);
        false ->
            get_greater_than(Name, T, Result, N)
    end;
get_greater_than(_, [], Result, _) ->
    Result.

% list top N process message queue length
get_top(Name, N) ->
    PList = erlang:processes(),
    PmqlList = [{Pid, Name, get(Pid, Name)} || Pid <- PList],
    SortedPmqlList = lists:reverse(lists:keysort(3, PmqlList)),
    lists:sublist(SortedPmqlList, N).

%% ===================================================================
%% internal functions
%% ===================================================================

% process message queue length
pmql(Pid) ->
    case erlang:process_info(Pid) of
        undefined ->
            0;
        List ->
            case lists:keyfind(message_queue_len, 1, List) of
                false ->
                    0;
                {message_queue_len, Len} ->
                    Len
            end
    end.

% process reductions
reductions(Pid) ->
    case erlang:process_info(Pid) of
        undefined ->
            0;
        List ->
            case lists:keyfind(reductions, 1, List) of
                false ->
                    0;
                {reductions, Len} ->
                    Len
            end
    end.