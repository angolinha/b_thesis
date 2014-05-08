-module(b_cache_sup).
-export([start_link/0, init/1]).
-behaviour(supervisor).

start_link() ->
    % io:format("Starting [CacheSup].~n"),
    supervisor:start_link({local, b_cache_sup}, ?MODULE, []).

init([]) ->
    {ok, MaxRestart} = application:get_env(b_master, sup_maxrestart),
    {ok, MaxTime} = application:get_env(b_master, sup_maxtime),
    {ok, Shutdown} = application:get_env(b_master, subsup_shutdown),
    {
        ok,
        {
            {one_for_all, MaxRestart, MaxTime},
            [
                {
                    b_cache,
                    {b_cache, start_link, []},
                    permanent,
                    Shutdown,
                    worker,
                    [b_cache]
                },
                {
                    b_cache_cleaner,
                    {b_cache_cleaner, start_link, []},
                    permanent,
                    Shutdown,
                    worker,
                    [b_cache_cleaner]
                }
            ]
        }
    }.