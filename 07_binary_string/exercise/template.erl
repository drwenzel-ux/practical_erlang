-module(template).

-export([parse/2]).

parse(Str, Data) when is_binary(Str) ->
    unicode:characters_to_binary(
        lists:map(
            fun(Token) ->
                case binary:split(Token, [<<"}}">>]) of
                    [NoKey] -> [NoKey];
                    [Key | T] -> 
                        case maps:find(Key, Data) of
                            error -> T;
                            {ok, Value} when is_integer(Value) -> [integer_to_binary(Value), T];
                            {ok, Value} -> [Value, T]
                        end
                end
            end,
            binary:split(Str, [<<"{{">>], [global])
        )
    ).
    

