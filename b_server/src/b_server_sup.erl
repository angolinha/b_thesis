-module(b_server_sup).
-export([start_link/0, init/1]).
-behaviour(supervisor).

start_link() ->
    supervisor:start_link({local, b_server_sup}, ?MODULE, []).

init([]) ->
    {ok, MaxRestart} = application:get_env(b_master, sup_maxrestart),
    {ok, MaxTime} = application:get_env(b_master, sup_maxtime),
    {
        ok,
        {
            {one_for_one, MaxRestart, MaxTime},
            []
        }
    }.