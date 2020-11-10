-module(chat_room).
-behavior(gen_server).

-export([start_link/0, add_user/3, remove_user/2, get_users/1, add_message/3, get_history/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

start_link() ->
  gen_server:start_link(?MODULE, [], []).

init([]) ->
  {ok, {state, [], #{}}}.

add_user(Room, Name, Pid) ->
  gen_server:cast(Room, {add_user, {Name, Pid}}), ok.

remove_user(Room, Pid) ->
  gen_server:call(Room, {remove_user, Pid}).

get_users(Room) -> 
  gen_server:call(Room, get_users).

add_message(Room, User, Text) -> 
  gen_server:cast(Room, {add_message, {User, Text}}), ok.

get_history(Room) -> 
  gen_server:call(Room, get_history).

handle_cast({add_user, {Name, Pid}}, {state, _History, Map}) ->
  NewState = {state, _History, Map#{Pid => Name}},
  {noreply, NewState};

handle_cast({add_message, {Name, Text}}, {state, History, _Map}) ->
  Users = maps:keys(_Map),
  lists:foreach(
    fun(User) -> chat_user:add_message(User, Name, Text) end, Users),
  {noreply, {state, [{Name, Text} | History], _Map}}.

handle_call({remove_user, Pid}, _From, {state, _History, Map} = State) ->
  case maps:find(Pid, Map) of
    error ->
      {reply, {error, user_not_found}, State};
    
    {ok, _} -> 
      NewState = {state, _History, maps:remove(Pid, Map)},
      {reply, ok, NewState}
  end;

handle_call(get_users, _From, {state, _History, Map} = State) -> 
  Reply = maps:to_list(
    maps:fold(
      fun(K, V, Acc) ->  Acc#{V => K} end,
      #{}, 
      Map
    )
  ),
  {reply, Reply, State};

handle_call(get_history, _From, {state, History, _Map} = State) ->
  {reply, lists:reverse(History), State}.


handle_info(_Request, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.
