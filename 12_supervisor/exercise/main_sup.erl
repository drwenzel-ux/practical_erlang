-module(main_sup).
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
      id => super_1,
      start => {sup_1, start_link, []},
      restart => permanent,
      shutdown => 2000,
      type => supervisor,
      modules => [sup_1]
    },
    #{
      id => super_2,
      start => {sup_2, start_link, []},
      restart => permanent,
      shutdown => 2000,
      type => supervisor,
      modules => [sup_2]
    }
  ],
  {ok, {SuperSpec, ChildSpec}}.
