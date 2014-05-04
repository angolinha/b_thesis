-module(b_balancer).
-behaviour(application).
-export([stop/1, start/2, add_load_balancer/1]).

start(normal, _Args) ->
    erlang:set_cookie(xsustekm_b_thesis, node()),
    b_balancer_sup:start_link();
start({takeover, _OtherNode}, []) ->
    erlang:set_cookie(xsustekm_b_thesis, node()),
    b_balancer_sup:start_link();
start({failover, _OtherNode}, []) ->
    erlang:set_cookie(xsustekm_b_thesis, node()),
    b_balancer_sup:start_link().

stop(_State) ->
    ok.

add_load_balancer(Service) ->
    LbName = list_to_atom(atom_to_list(b_lb_) ++ atom_to_list(Service)),
    case global:whereis_name(LbName) of
        undefined ->
            {ok, _Pid} = supervisor:start_child(
                            {global, b_balancer_sup},
                            {
                                LbName,
                                {b_load_balancer, start_link, [{Service, LbName}]},
                                permanent,
                                application:get_env(b_balancer, sup_shutdown, nil),
                                worker,
                                [b_load_balancer]
                            }
                        );
        _Pid ->
            ok
    end.