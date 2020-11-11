-module(sup_1).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link(?MODULE, []).

init(_) ->
  SuperSpec = #{
    stategy => one_for_one,
    intensity => 10,
    period => 60
  },

  ChildSpec = [
    #{
      id => worker_1,
      start => {worker, start_link, [worker_1]},
      restart => permanent,
      shutdown => 2000,
      type => worker,
      modules => [worker]
    },
    #{
      id => worker_2,
      start => {worker, start_link, [worker_2]},
      restart => permanent,
      shutdown => 2000,
      type => worker,
      modules => [worker]
    }
  ],
  {ok, {SuperSpec, ChildSpec}}.


