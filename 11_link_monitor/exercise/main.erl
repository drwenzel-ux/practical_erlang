-module(main).

-export([parse/1, wait_results/2]).

-define(TIMEOUT, 1000).

parse(Files) ->
    Proc = lists:foldl(
        fun
            (File, Acc) ->
            Pid = spawn(worker, parse_file, [self(), File]),
            Ref = monitor(process, Pid),
            Acc#{Pid => {Ref, File}}
        end,
        #{},
        Files
    ),
    wait_results(Proc, {#{}, #{}}).

wait_results(Proc, Result) when map_size(Proc) =:= 0 -> Result;

wait_results(Proc, {Result, Error}) ->
    receive
        {ok, Data} ->
            wait_results(Proc, {reduce(Data, Result), Error});

        {'DOWN', _, process, Pid, Reason} ->
            NewProc = maps:remove(Pid, Proc),
            case Reason of
                normal ->
                    wait_results(NewProc, {Result, Error});

                _ ->
                    {ok, {_, File}} = maps:find(Pid, Proc),
                    wait_results(NewProc, {Result, Error#{File => Reason}})
            end
    after
        ?TIMEOUT -> {error, timeout}
    end.

reduce(Data, Init) ->
    Fun =
        fun
            (Name, NewValue, DataAcc) ->
                case maps:find(Name, DataAcc) of
                    {ok, OldValue} ->
                        DataAcc#{ Name => NewValue + OldValue };

                    error ->
                        DataAcc#{ Name => NewValue }
                end
        end,
    maps:fold(Fun, Init, Data).