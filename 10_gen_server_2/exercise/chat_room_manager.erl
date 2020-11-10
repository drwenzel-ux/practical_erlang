-module(chat_room_manager).
-behavior(gen_server).

-export([start_link/0,
         create_room/1, get_rooms/0,
         add_user/3, remove_user/2, get_users/1,
         send_message/3,  get_history/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) -> {ok, #{}}.

create_room(Name) -> 
  gen_server:call(?MODULE, {create_room, Name}).

get_rooms() ->
  gen_server:call(?MODULE, get_rooms).

add_user(Room, Name, Pid) -> 
  gen_server:call(?MODULE, {add_user, {Room, Name, Pid}}).

remove_user(Room, Pid) -> 
  gen_server:call(?MODULE, {remove_user, {Room, Pid}}).

get_users(Room) -> 
  gen_server:call(?MODULE, {get_users, Room}).

send_message(Room, Name, Text) ->
  gen_server:call(?MODULE, {send_message, {Room, Name, Text}}).

get_history(Room) -> 
  gen_server:call(?MODULE, {get_history, Room}).


handle_call({create_room, Name}, _From, State) ->
  {ok, Room} = chat_room:start_link(),
  {reply, {Name, Room}, State#{Room => Name}};

handle_call(get_rooms, _From, State) ->
  Reply = maps:to_list(
    maps:fold(
      fun(K, V, Acc) ->  Acc#{V => K} end,
      #{}, 
      State
    )
  ),
  {reply, Reply, State};

handle_call({add_user, {Room, Name, Pid}}, _From, State) ->
  case maps:find(Room, State) of
    error -> 
      {reply, {error, room_not_found}, State};
  
    {ok, _RoomName} ->
      {reply, chat_room:add_user(Room, Name, Pid), State}
  end;

handle_call({remove_user, {Room, Pid}}, _From, State) ->
  case maps:find(Room, State) of
    error -> 
      {reply, {error, room_not_found}, State};
  
    {ok, _RoomName} ->
      {reply, chat_room:remove_user(Room, Pid), State}
  end;

handle_call({get_users, Room}, _From, State) ->
  case maps:find(Room, State) of
    error -> 
      {reply, {error, room_not_found}, State};
  
    {ok, _RoomName} ->
      {reply, {ok, chat_room:get_users(Room)}, State}
  end;

handle_call({send_message, {Room, Name, Text}}, _From, State) ->
  case maps:find(Room, State) of
    error -> 
      {reply, {error, room_not_found}, State};
  
    {ok, _RoomName} ->
      {reply, chat_room:add_message(Room, Name, Text), State}
  end;

handle_call({get_history, Room}, _From, State) ->
  case maps:find(Room, State) of
    error -> 
      {reply, {error, room_not_found}, State};
  
    {ok, _RoomName} ->
      {reply, {ok, chat_room:get_history(Room)}, State}
  end.

handle_cast(_, State) ->
  {noreply, State}.

handle_info(_Request, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

