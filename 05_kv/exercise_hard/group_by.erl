%% Реализовать group_by
-module(group_by).

-export([
  group_by1/1, 
  group_by1_test/0, 
  group_by2/2, 
  group_by2_test/0, 
  group_by2_task1/0,
  group_by2_task2/0,
  users/0, 
  client_sessions/0]).

%% Первый этап -- решение частной задачи

%% У нас есть список пользователей вида:

users() -> [
 {user, "Bob", 21, male},
 {user, "Bill", 23, male},
 {user, "Buba", 23, female},
 {user, "Helen", 17, female},
 {user, "Baba", 17, male},
 {user, "Kate", 25, female},
 {user, "John", 20, male}
].

%% Нужно реализовать функцию group_by(Users) -> Groups
%% которая сгруппирует пользователей по полу, и вернет map
%% где ключами будет пол, а значениям будет список пользователей этого пола.

%% Например:
%% #{
%%   male => [{user, "Bob", 21, male}, {user, "Bill", 23, male}, {user, "John", 20, male}],
%%   female => [{user, "Kate", 25, female}, {user, "Helen", 17, female}]
%% }.

group_by([], Map) -> Map;

group_by([Head|Rest], #{male := Males, female := Females} = Map) ->
  case Head of
    {user, _, _, male} -> 
      group_by(Rest, Map#{male => [Head|Males], female => Females});
    {user, _, _, female} -> 
      group_by(Rest, Map#{male => Males, female => [Head|Females]})
  end.

group_by1(Users) -> 
  group_by(Users, #{male => [], female => []}).

group_by1_test() -> 
  group_by1(users()).


%% Другая версия
% group_by(Users) ->
%   #{
%     male => lists:filter(fun({user, _, _, Sex}) -> Sex =:= male end, Users),
%     female => lists:filter(fun({user,_,_, Sex}) -> Sex =:= female end, Users)
%   }.

%% Второй этап -- обобщенное решение

%% Нужно реализовать функцию, которая может сгруппировать любой список по любому критерию,
%% даже не зная, что за элементы находятся внутри списка.

%% Она должна принимать два аргумента:
%% - функцию, которая для данного элемента списка может сказать, к какому критерию отностися этот элемент
%% - и сам список

%% Применяя разные CriteriaFun, можно делать разные группировки списка одной и той же функцией group_by.

%% Например, мы хотим сгруппировать пользователей по возрасту:
%% CriteriaFun = fun(User) -> ??? end.
%% group_by(CriteriaFun, Users) ->
%%     #{21 => [{user, "Bob", 21, male}], 23 => [{user, "Bill", 23, male}} ...

%% Или мы можем определить некие диапазоны возрастов:
%% 0-12 - child
%% 13-18 - teeneage
%% 18-25 - young
%% 26-60 - adult
%% > 10 - old
%% и сгруппировать пользователей по этим диапазонам:
%% CriteriaFun = fun(User) -> ??? end.
%% group_by(CriteriaFun, Users) ->
%%    #{teenage => [{user, "Helen", 17, female}], young => [{user, "Bob", 21, male}, {user, "Bill", 23, male} ...

group_by(_, [], Map) -> Map;

group_by(Fun, [H|T], Map) ->
  case maps:find(Fun(H), Map) of
    {ok, List} -> group_by(Fun, T, Map#{Fun(H) := [H|List]});
    error -> group_by(Fun, T, Map#{Fun(H) => [H]})
  end.

group_by2(CriteriaFun, List) -> group_by(CriteriaFun, List, #{}).

group_by2_test() -> 
  group_by2(fun({user, _, Age, _}) ->  Age end, users()).

%% Третий пример:
%% У нас есть распределенная система, кластер из нескольких узлов.
%% В этой системе есть клиентские соединения разных типов, подключенные к разным узлам:
%% [
%% {session, type_a, node_1, SocketId1},
%% {session, type_b, node_1, SocketId2},
%% {session, type_a, node_2, SocketId3},
%% {session, type_b, node_2, SocketId4}
%% ]
%% Мы хотим сгруппировать эти сессии по узлу:
%% #{
%%  node_1 => [{session, type_a, node_1, SocketId1}, {session, type_a, node_2, SocketId3}],
%%  node_2 => [{session, type_a, node_2, SocketId3}, {session, type_b, node_2, SocketId4}]
%% }
%% А потом мы хотим сгруппировать их по типу:
%% #{
%%   type_a => [{session, type_a, node_1, SocketId1}, {session, type_a, node_2, SocketId3}],
%%   type_b => [{session, type_b, node_1, SocketId2}, {session, type_b, node_2, SocketId4}]
%% }
%% И во всех случаях мы применяем одну и ту же функцию group_by,
%% передавая ей разные CriteriaFun и разные списки.

client_sessions() ->
  [
    {session, type_a, node_1, socketId1},
    {session, type_b, node_1, socketId2},
    {session, type_a, node_2, socketId3},
    {session, type_b, node_2, socketId4}
  ].

group_by2_task1() ->
  group_by2(fun({session, _, NodeN, _}) -> NodeN end, client_sessions()).


group_by2_task2() ->
  group_by2(fun({session, Type, _, _}) -> Type end, client_sessions()).

