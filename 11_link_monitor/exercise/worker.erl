-module(worker).
-export([parse_file/2, parse_line/1]).

parse_file(Root, File) ->
    {ok, Text} = file:read_file(File),
    List = lists:foldl(
        fun
            (<<>>, Acc) -> Acc;
            (L, Acc) -> [parse_line(L)|Acc]
        end,
        [], binary:split(Text, <<"\n">>, [global])),
    Root ! {ok, maps:from_list(List)},
    ok.


parse_line(Line) ->
    [_, Name, Count, _] =
        binary:split(Line, <<",">>, [global]),
    {Name, binary_to_integer(Count)}.
