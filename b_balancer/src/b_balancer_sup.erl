-module(b_balancer_sup).
-export([start_link/0, init/1]).
-behaviour(supervisor).

start_link() ->
    supervisor:start_link({global, b_balancer_sup}, ?MODULE, []).

init([]) ->
    {
        ok,
        {
            {one_for_one, application:get_env(b_balancer, sup_maxrestart, nil), application:get_env(b_balancer, sup_maxtime, nil)},
            []
        }
    }.