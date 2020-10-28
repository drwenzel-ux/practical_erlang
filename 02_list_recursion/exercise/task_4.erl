-module(task_4).

-export([dropwhile/2, takewhile/2]).

-include_lib("eunit/include/eunit.hrl").


%% implement lists:dropwhile/2
%% http://www.erlang.org/doc/man/lists.html#dropwhile-2
dropwhile(_, []) -> [];
dropwhile(Pred, List) ->
    [H | T] = List, 
    case Pred(H) of 
        true -> dropwhile(Pred, T);
        false -> List
    end.


dropwhile_test() ->
    F = fun(Val) -> Val =:= 32 end,
    ?assertEqual("hello", dropwhile(F, "   hello")),
    ?assertEqual([], dropwhile(F, [])),
    ?assertEqual([1,2,3], dropwhile(F, [1,2,3])),
    ?assertEqual([3,4], dropwhile(F, [32,3,4])),
    ?assertEqual([3,4], dropwhile(F, [32,32,3,4])),
    ?assertEqual([3,32,4,32], dropwhile(F, [32,32,32,32,32,32,3,32,4,32])),
    ok.

% dropwhile(_, []) -> [];
% dropwhile(Pred, List) -> dropwhile(Pred, List, start, []).
% dropwhile(Pred, [H|T], start, Result) -> dropwhile(Pred, [H|T], Pred(H), Result);
% dropwhile(Pred, [H|T], true, Result) -> dropwhile(Pred, T, Pred(H), [H|Result]);
% dropwhile(_, _, false, Result) -> Result.


% takewhile(_, []) -> [];
% takewhile(Pred, List) -> takewhile(Pred, List, check, []).

% takewhile(Pred, [H|T], check, Acc) -> Case = Pred(H), takewhile(Pred, [H|T], Case, Acc);
% takewhile(Pred, [H|T], true, Acc) -> takewhile(Pred, T, check, [H|Acc]);
% takewhile(_, _, false, Acc) -> task_2:reverse(Acc).



%% implement lists:takewhile/2
%% http://www.erlang.org/doc/man/lists.html#takewhile-2
takewhile(_, []) -> [];
takewhile(Pred, List) -> 
    [H | T] = List, 
    case Pred(H) of
        true -> [H | takewhile(Pred, T)];
        false -> takewhile(Pred, [])
    end.

takewhile_test() ->
    F = fun(Val) -> Val =:= 32 end,
    ?assertEqual("   ", takewhile(F, "   hello")),
    ?assertEqual([], takewhile(F, [])),
    ?assertEqual([], takewhile(F, [1,2,3])),
    ?assertEqual([32], takewhile(F, [32,3,4])),
    ?assertEqual([32,32], takewhile(F, [32,32,3,4])),
    ?assertEqual([32,32,32,32,32,32], takewhile(F, [32,32,32,32,32,32,3,32,4,32])),
    F2 = fun(Val) -> Val < 5 end,
    ?assertEqual([1,2,3,4], takewhile(F2, [1,2,3,4,5,6,7,8])),
    ok.
