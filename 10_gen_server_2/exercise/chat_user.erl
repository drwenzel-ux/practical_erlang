-module(chat_user).
-behavior(gen_server).

-export([start_link/0, add_message/3, get_messages/1, get_count/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {count :: integer(), mailbox :: list()}).

start_link() ->
  gen_server:start_link(?MODULE, [], []).

init([]) ->
  {ok, #state{count = 0, mailbox = []}}.

add_message(User, Name, Text) ->
  gen_server:cast(User, {add_message, {Name, Text}}), ok.

get_messages(User) ->
  gen_server:call(User, get_messages).

get_count(User) ->
  gen_server:call(User, get_count).

handle_cast({add_message, {Name, Text}}, State) ->
  #state{count = Count, mailbox = Mailbox} = State,
  NewState = #state{count = Count + 1, mailbox = [{Name, Text} | Mailbox]},
  {noreply, NewState}.

handle_call(get_messages, _From, #state{mailbox = Mailbox} = State) ->
  {reply, lists:reverse(Mailbox), State};

handle_call(get_count, _From, #state{count = Count} = State) ->
  {reply, Count, State}.

handle_info(_Request, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.
