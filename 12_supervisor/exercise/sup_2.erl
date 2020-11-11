-module(sup_2).
-behaviour(supervisor).

-export([start_link/0, init/1, add_worker/1, remove_worker/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
  SuperSpec = #{
    stategy => one_for_one,
    intensity => 10,
    period => 60
  },

  ChildSpec = [
    #{
      id => worker_3,
      start => {worker, start_link, [worker_3]},
      restart => permanent,
      shutdown => 2000,
      type => worker,
      modules => [worker]
    },
    #{
      id => worker_4,
      start => {worker, start_link, [worker_4]},
      restart => permanent,
      shutdown => 2000,
      type => worker,
      modules => [worker]
    }
  ],
  {ok, {SuperSpec, ChildSpec}}.


add_worker(Term) ->
  ChildSpec = #{
    id => Term,
    start => {worker, start_link, [Term]},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => [worker]
  },
  supervisor:start_child(?MODULE, ChildSpec).

remove_worker(Term) -> 
  supervisor:terminate_child(?MODULE, Term),
  supervisor:delete_child(?MODULE, Term).