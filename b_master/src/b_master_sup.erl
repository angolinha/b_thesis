-module(b_master_sup).
-export([start_link/0, init/1]).
-behaviour(supervisor).

start_link() ->
    io:format("Starting [MasterSupervisor]~n"),
    supervisor:start_link({local, b_master_sup}, ?MODULE, []).

init([]) ->
    {
        ok,
        {
            {one_for_one, application:get_env(b_master, sup_maxrestart, nil), application:get_env(b_master, sup_maxtime, nil)},
            [
                {
                    b_cache_sup,
                    {b_cache_sup, start_link, []},
                    permanent,
                    application:get_env(b_master, sup_shutdown, nil),
                    supervisor,
                    [b_cache_sup]
                },
                {
                    b_service_broker,
                    {b_service_broker, start_link, []},
                    permanent,
                    application:get_env(b_master, sup_shutdown, nil),
                    worker,
                    [b_service_broker]
                }
            ]
        }
    }.