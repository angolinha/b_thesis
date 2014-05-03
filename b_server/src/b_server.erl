-module(b_server).
-behaviour(application).
-export([stop/1, start/2, add_service_server/1, add_service_server/2, check_registered/1]).

start(normal, _Args) ->
    erlang:set_cookie(xsustekm_b_thesis, node()),
    b_server_sup:start_link();
start(takeover, _OtherNode) ->
    erlang:set_cookie(xsustekm_b_thesis, node()),
    b_server_sup:start_link().

stop(_State) ->
    ok.

add_service_server(Service) ->
    ServerName = list_to_atom(atom_to_list(b_service_) ++ atom_to_list(Service)),
    LbName = list_to_atom(atom_to_list(b_lb_) ++ atom_to_list(Service)),
    add_server(Service, ServerName, LbName).

add_service_server(manual, Service) ->
    ServerName = list_to_atom(atom_to_list(b_service_) ++ atom_to_list(Service)),
    LbName = list_to_atom(atom_to_list(b_lb_) ++ atom_to_list(Service)),
    case global:whereis_name(LbName) of
        undefined ->
            timer:sleep(net_kernel:get_net_ticktime()),
            case global:whereis_name(LbName) of
                undefined ->
                    [Balancer|_] = lists:filter(fun(X) ->
                            case string:str(atom_to_list(X), "b_balancer") of
                                1 -> true;
                                0 -> false;
                                _ -> false
                            end
                        end, nodes()),
                    rpc:call(Balancer, b_balancer, add_load_balancer, [Service]),
                    add_server(Service, ServerName, LbName);
                _LbPid ->
                    add_server(Service, ServerName, LbName)
            end;
        _LbPid ->
            add_server(Service, ServerName, LbName)
    end.

add_server(Service, ServerName, LbName) ->
    case whereis(ServerName) of
        undefined ->
            {ok, Pid} = supervisor:start_child(
                            b_server_sup,
                            {
                                ServerName,
                                {b_service_server, start_link, [{Service, LbName, ServerName}]},
                                permanent,
                                application:get_env(b_server, sup_shutdown, nil),
                                worker,
                                [b_service_server]
                            }
                        ),
            Pid;
        Pid ->
            Pid
    end.

check_registered(Service) ->
    ServerName = list_to_atom(atom_to_list(b_service_) ++ atom_to_list(Service)),
    whereis(ServerName).