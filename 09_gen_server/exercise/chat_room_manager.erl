-module(chat_room_manager).

-export([
    loop/1, 
    call/2, 
    handle_call/2,
    start/0,
    create_room/2, 
    remove_room/2, 
    get_rooms/1,
    add_user/3,
    remove_user/3,
    get_users_list/2,
    send_message/4,
    get_messages_history/2
]).


start() ->
    InitialState = #{},
    spawn(?MODULE, loop, [InitialState]).


loop(State) -> 
    io:format("start main loop ~p~n", [self()]),
    receive
        {call, Ref, From, Msg} -> 
            {Reply, NewState} = handle_call(Msg, State),
            From ! {reply, Ref, Reply},
            ?MODULE:loop(NewState);
        
        stop ->
            io:format("~p stops now ~n", [self()]);
        
        Msg -> 
            io:format("ERROR: ~p receive unknown msg ~p~n", [self(), Msg]),
            ?MODULE:loop(State)
    end,
    io:format("end main loop~n").


handle_call({create_room, RoomName}, State) ->
    List = maps:to_list(State),
    case length(List) < 5 of
        true -> 
            RoomId = make_ref(),
            NewState = State#{RoomId => {RoomName, [], []}},
            {{ok, RoomId}, NewState};
        
        false -> 
            {{error, room_limit}, State}
    end;


handle_call(get_rooms, State) -> 
    Rooms = maps:to_list(State),
    Ret = 
        lists:foldl(
            fun({RoomId, {RoomName, _, _}}, Acc) -> 
                [{RoomId, RoomName} | Acc] end,
            [], Rooms),
    {lists:reverse(Ret), State};


handle_call({remove_room, RoomId}, State) ->
    case maps:find(RoomId, State) of
        {ok, _} -> 
            NewState = maps:remove(RoomId, State), 
            {ok, NewState};

        error -> 
            {{error, room_not_found}, State}
    end;


handle_call({add_user, RoomId, UserName}, State) ->
    case maps:find(RoomId, State) of
        error -> 
            {{error, room_not_found}, State};
    
        {ok, {RoomName, UserList, History}} ->
            case lists:member(UserName, UserList) of
                true -> 
                    {{error, user_is_in_room}, State};
                
                false -> 
                    NewUserList = [UserName | UserList],
                    NewState = State#{ RoomId := {RoomName, NewUserList, History}},
                    {ok, NewState}
            end
    end;


handle_call({remove_user, RoomId, UserName}, State) ->
    case maps:find(RoomId, State) of
        error ->
            {{error, room_not_found}, State};

        {ok, {RoomName, UserList, History}} ->
            case lists:member(UserName, UserList) of
                false -> 
                    {{error, user_not_in_room}, State};
                
                true ->
                    NewUserList = lists:delete(UserName, UserList),
                    NewState = State#{ RoomId := {RoomName, NewUserList, History}},
                    {ok, NewState}
            end
    end;


handle_call({get_users_list, RoomId}, State) ->
    case maps:find(RoomId, State) of
        {ok, {_, UserList, _}} -> 
            {{ok, UserList}, State};

        error -> 
            {{error, room_not_found}, State}
    end;


handle_call({send_message, RoomId, UserName, Message}, State) -> 
    case maps:find(RoomId, State) of
        error ->
            {{error, room_not_found}, State};
        
        {ok, {RoomName, UserList, History}} ->
            case lists:member(UserName, UserList) of
                false -> 
                    {{error, user_not_in_room}, State};
                
                true ->
                    NewHistory = [{UserName, Message} | History],
                    NewState = State#{ RoomId := {RoomName, UserList, NewHistory}},
                    {ok, NewState}
            end
    end;


handle_call({get_messages_history, RoomId}, State) ->
    case maps:find(RoomId, State) of
        {ok, {_, _, History}} -> 
            {{ok, History}, State};

        error -> 
            {{error, room_not_found}, State}
    end.


call(Pid, Msg) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {call, Ref, self(), Msg},
    receive
        {reply, Ref, Reply} ->
            erlang:demonitor(Ref, [flush]),
            Reply;
        {'DOWN', Ref, process, Pid, Reason} ->
            {error, Reason}
    after 5000 ->
            erlang:demonitor(Ref, [flush]),
            noreply
    end.


create_room(Server, RoomName) ->
    call(Server, {create_room, RoomName}).


remove_room(Server, RoomId) ->
    call(Server, {remove_room, RoomId}).


get_rooms(Server) ->
    call(Server, get_rooms).


add_user(Server, RoomId, UserName) ->
    call(Server, {add_user, RoomId, UserName}).


remove_user(Server, RoomId, UserName) ->
    call(Server, {remove_user, RoomId, UserName}).


get_users_list(Server, RoomId) ->
    call(Server, {get_users_list, RoomId}).


send_message(Server, RoomId, UserName, Message) ->
    call(Server, {send_message, RoomId, UserName, Message}).


get_messages_history(Server, RoomId) ->
    call(Server, {get_messages_history, RoomId}).
