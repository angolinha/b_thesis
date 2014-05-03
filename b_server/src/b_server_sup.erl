-module(b_server_sup).
-export([start_link/0, init/1]).
-behaviour(supervisor).

start_link() ->
    supervisor:start_link({local, b_server_sup}, ?MODULE, []).

init([]) ->
    {
        ok,
        {
            {one_for_one, application:get_env(b_server, sup_maxrestart, nil), application:get_env(b_server, sup_maxtime, nil)},
            []
        }
    }.