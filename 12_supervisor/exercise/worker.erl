-module(worker).
-behavior(gen_server).

-export([start_link/1, ping/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

start_link(Id) ->
  gen_server:start_link(?MODULE, [Id], []).

init([Id]) -> {ok, {Id, self()}}.

ping(Pid) ->
  gen_server:call(Pid, ping).

handle_call(ping, _From, State) ->
  {reply, State, State}.

handle_cast(_, State) ->
  {noreply, State}.

handle_info(_Request, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.
