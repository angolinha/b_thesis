%% @author Martin Šustek <xsustek.martin@gmail.com>
%% @doc Load balancer
%%
%% The module contains an implementation of the Erlang framework Load balancer
%% which balances requests between service servers
%% @copyright 2014 Martin Šustek

-module(b_load_balancer).
-behaviour(gen_server).
-author("Martin Sustek").
-date({2013,03,25}).
-version("1.0").

-export([init/1, handle_call/3, handle_cast/2, code_change/3, handle_info/2, start_link/1, terminate/2]).

-record(state, {service,
                load,
                milestone,
                servers=[]}).

%%% Gen server functions

start_link({Service, LbName}) ->
    % io:format("Starting [Load balancer] for service: --~p-- at node: --~p--.~n", [Service, node()]),
    gen_server:start_link({global, LbName}, ?MODULE, Service, []).

init(Service) ->
    Nodes = lists:filter(fun(X) -> case string:str(atom_to_list(X), "b_server") of 1 -> true; 0 -> false end end, nodes()),
    % io:format("Node layout has these Server nodes: ~p~n", [Nodes]),
    Servers = lists:foldl(fun(Node, Acc) ->
            case rpc:call(Node, b_server, check_registered, [Service]) of
                undefined -> Acc;
                Pid -> [{Pid, Node, 0}|Acc]
            end
        end, [], Nodes),
    % io:format("These nodes: ~p are running --~p-- service.~n", [Servers, Service]),
    {Mega, Sec, Micro} = now(),
    Timestamp = Mega * 1000000 * 1000000 + Sec * 1000000 + Micro,
    {ok, #state{service=Service, servers=Servers, load=0, milestone=Timestamp}}.

handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, State}.

handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.

handle_cast({server_pid, Node, Pid, Load}, S=#state{servers=Servers, load=Overall}) ->
    io:format("Registering started or restarted ServiceServer instance in [Load balancer].~n"),
    case lists:keysearch(Node, 2, Servers) of
        {value, {_, Node, OldLoad}} ->
            NewServers = lists:keyreplace(Node, 2, Servers, {Pid, Node, Load}),
            {noreply, S#state{servers=NewServers, load=(Overall+(Load-OldLoad))}};
        false ->
            {noreply, S#state{servers=[{Pid, Node, Load}|Servers], load=(Overall+Load)}}
    end;

handle_cast({get_result, {Ref, Arg, WorkerPid}, InsToCache}, S=#state{service=Service, servers=[]}) ->
    io:format("Handling Worker's content request in [Load balancer] with EMPTY ServiceServers list.~n"),
    [Node|_] = lists:filter(fun(X) -> case string:str(atom_to_list(X), "b_server") of 1 -> true; 0 -> false end end, nodes()),
    Pid = rpc:call(Node, b_server, add_service_server, [Service]),
    gen_server:cast(Pid, {serve, {Ref, Arg, WorkerPid}, InsToCache}),
    NewState = S#state{servers=[{Pid, Node, 1}], load=1},
    Final = check_milestone(NewState),
    {noreply, Final};

handle_cast({get_result, {Ref, Arg, WorkerPid}, InsToCache}, S=#state{servers=Servers, load=Overall}) ->
    io:format("Handling Worker's content request in [Load balancer]. LOAD is: ~p~n", [Overall]),
    [{Pid, _, Load}|_] = lists:sort(fun({_, _, LoadA}, {_, _, LoadB}) -> (LoadA =< LoadB) end, Servers),
    case node(Pid) of
        nonode@nohost ->
            gen_server:cast(Pid, stop),
            NewServers = lists:keydelete(Pid, 1, Servers),
            {noreply, S#state{servers=NewServers, load=(Overall-Load)}};
        _ ->
            gen_server:cast(Pid, {serve, {Ref, Arg, WorkerPid}, InsToCache}),
            NewS = add_running_service(Pid, S#state{load=(Overall+1)}),
            Final = check_milestone(NewS),
            {noreply, Final}
    end;

handle_cast({service_instance_finished, Pid}, S=#state{servers=Servers, load=Overall}) ->
    io:format("ServiceServer FINISHED new task. LOAD is: ~p~n", [Overall-1]),
    case lists:keysearch(Pid, 1, Servers) of
        {value, {Pid, _, 0}} ->
            {noreply, S};
        {value, {Pid, Node, Load}} ->
            NewServers = lists:keyreplace(Pid, 1, Servers, {Pid, Node, (Load-1)}),
            {noreply, S#state{servers=NewServers, load=(Overall-1)}};
        false ->
            {noreply, S}
    end;

handle_cast(stop, State) ->
    {stop, normal, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

check_milestone(S=#state{milestone=Milestone}) ->
    % io:format("[Load balancer] reached interval for replication check.~n"),
    {Mega, Sec, Micro} = now(),
    Timestamp = Mega * 1000000 * 1000000 + Sec * 1000000 + Micro,
    {ok, ReplicationInterval} = application:get_env(b_balancer, replication_interval),
    case ( (Timestamp - Milestone) > ReplicationInterval ) of
        true ->
            check_load(S#state{milestone=Timestamp});
        false ->
            S
    end.

check_load(S=#state{service=Service, load=Load, servers=Servers}) ->
    io:format("[Load balancer] checking whether to add replica or send terminate Msg to one of active servers.~n"),
    {ok, RequestsPerInterval} = application:get_env(b_balancer, srv_requests_per_interval),
    ReplicationTreshold = ((Load/length(Servers))/RequestsPerInterval),
    {ok, ReplicationTop} = application:get_env(b_balancer, replication_top),
    case ( ReplicationTreshold > ReplicationTop ) of
        true ->
            Nodes = lists:filter(fun(X) ->
                case lists:keysearch(X, 2, Servers) of
                    {value, _} -> false;
                    _ ->
                        case string:str(atom_to_list(X), "b_server") of
                            1 -> true;
                            0 -> false;
                            _ -> false
                        end
                    end
                end, nodes()),
            case Nodes of
                [] -> S;
                [Node|_] ->
                    io:format("[Load balancer] REPLICATING service --~p-- to node: --~p--~n", [Service, Node]),
                    Pid = rpc:call(Node, b_server, add_service_server, [Service]),
                    S#state{servers=[{Pid, Node, 0}|Servers]}
            end;
        false ->
            S
    end.

add_running_service(Pid, S=#state{servers=Servers}) ->
    % io:format("Updating service servers list in [Load balancer].~n"),
    case lists:keysearch(Pid, 1, Servers) of
        {value, {Pid, Node, Load}} ->
            NewServers = lists:keyreplace(Pid, 1, Servers, {Pid, Node, (Load+1)}),
            S#state{servers=NewServers};
        false ->
            S
    end.