-module(b_cache_sup).
-export([start_link/0, init/1]).
-behaviour(supervisor).

start_link() ->
    io:format("Starting [CacheSup].~n"),
    supervisor:start_link({local, b_cache_sup}, ?MODULE, []).

init([]) ->
    io:format("Requesting Cache start in [CacheSup].~n"),
    {
        ok,
        {
            {one_for_all, application:get_env(b_master, sup_maxrestart, nil), application:get_env(b_master, sup_maxtime, nil)},
            [
                {
                    b_cache,
                    {b_cache, start_link, []},
                    permanent,
                    application:get_env(b_master, subsup_shutdown, nil),
                    worker,
                    [b_cache]
                },
                {
                    b_cache_cleaner,
                    {b_cache_cleaner, start_link, []},
                    permanent,
                    application:get_env(b_master, subsup_shutdown, nil),
                    worker,
                    [b_cache_cleaner]
                }
            ]
        }
    }.