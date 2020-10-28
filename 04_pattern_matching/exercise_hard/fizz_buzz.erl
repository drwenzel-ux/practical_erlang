%% Реализовать Fizz Buzz
%% https://habr.com/ru/post/298134/

-module(fizz_buzz).

-export([app/0]).

app() -> 
  List = lists:seq(1, 100),
  lists:foreach(
    fun(V) ->
    if 
      V rem 3 =:= 0 andalso V rem 5 =:= 0 -> io:fwrite("FizzBuzz ~w~n", [V]);
      V rem 3 =:= 0 -> io:fwrite("Fizz ~w~n", [V]);
      V rem 5 =:= 0 -> io:fwrite("Buzz ~w~n", [V]);
      true -> skip end end,
    List
  ).