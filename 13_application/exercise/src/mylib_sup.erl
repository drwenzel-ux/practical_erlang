-module(mylib_sup).

-behaviour(supervisor).
-export([start_link/0, init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    SuperSpec = #{
        stategy => one_for_one,
        intensity => 10,
        period => 60
    },
    ChildSpec = #{
        id => 1,
        start => {mylib_worker, start_link, []},
        restart => permanent,
        shutdown => 2000,
        type => worker,
        modules => [mylib_worker]
    },
    {ok, {SuperSpec, [ChildSpec]}}.