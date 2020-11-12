-module(mylib_worker).
-behavior(gen_server).

-export([start_link/0, get_version/0, get_modules/0, get_min_val/0, get_connection_timeout/0, all_apps/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) -> {ok, empty_state}.

get_version() ->
    {ok, App} = application:get_application(?MODULE),
    gen_server:call(?MODULE, {key, vsn, App}).

get_modules() -> 
    {ok, App} = application:get_application(?MODULE),
    gen_server:call(?MODULE, {key, modules, App}).

get_min_val() -> 
    {ok, App} = application:get_application(?MODULE),
    gen_server:call(?MODULE, {env, min_val, App}).

get_connection_timeout() -> 
    {ok, App} = application:get_application(?MODULE),
    gen_server:call(?MODULE, {env, connection_timeout, App}).

all_apps() ->
    gen_server:call(?MODULE, all_apps).

handle_call(all_apps, _From, State) ->
    Reply = lists:foldl(
        fun
            ({Name, Descr, Vsn}, Acc) ->
                Acc#{Name => #{description => Descr, version => Vsn}}
        end, #{}, application:loaded_applications()),
    {reply, Reply, State};

handle_call({Type, Param, App}, _From, State) ->
    case Type of
        key -> 
            {ok, Value} = application:get_key(App, Param),
            {reply, Value, State};
        env ->
            {ok, Value} = application:get_env(App, Param),
            {reply, Value, State}
    end.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

