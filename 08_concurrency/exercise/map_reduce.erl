-module(map_reduce).

-export([pprint/1, reduce/1, read_file/1, start/1, word_map/1,
     mapper/2]).

start(Files) ->
    [spawn(?MODULE, mapper, [File, self()]) || File <- Files],
    Result = [
        receive
            {map, _, Map} -> Map;
            {error, Pid, _} -> 
                io:fwrite("~p failed!~n", [Pid]), fail
        after
            2000 -> {error, no_reply}
        end || _ <- lists:seq(1, length(Files))
    ],
    reduce(lists:delete(fail, Result)).

reduce(Maps) -> 
    List = lists:flatten(
        lists:map(fun(V) -> maps:to_list(V) end, Maps)
    ),
    lists:foldl(
        fun({Word, Count}, Acc) ->
            case maps:find(Word, Acc) of
                error -> Acc#{Word => Count};
                {ok, OldCount} -> Acc#{Word := Count + OldCount}
            end
        end,
        #{},
        List
    ).

read_file(Name) ->
    case file:read_file(Name) of
        {ok, Text} -> Text;
        {error, Reason} ->
        io:fwrite("error: read ~p~n", [Reason]), error
    end.

mapper(Name, ReducePid) ->
    case read_file(Name) of
        error -> ReducePid ! {error, self(), #{}};
        Text -> 
            Delims = [<<" "/utf8>>, <<"\n"/utf8>>],
            Tokens = binary:split(Text, Delims, [global, trim]),
            ReducePid ! {map, self(), word_map(Tokens)}
    end.


word_map(List) ->
    Counter = fun (Key, Acc) ->
        case maps:find(Key, Acc) of
            error -> Acc#{Key => 1};
            {ok, Num} -> Acc#{Key := Num + 1}
        end
    end,
    lists:foldl(Counter, #{}, List).

pprint(Map) ->
    Printer = fun ({Word, Count}) ->
        io:fwrite("~ts -> ~p~n", [Word, Count])
    end,
    lists:foreach(Printer, maps:to_list(Map)).
