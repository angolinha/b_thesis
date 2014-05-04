%% @author Martin Šustek <xsustek.martin@gmail.com>
%% @doc Worker
%%
%% The module contains an implementation of the Erlang framework Worker
%% which handles requests to any page, breaks them into separate services,
%% collects results from them and returns whole page content
%% @copyright 2014 Martin Šustek

-module(b_worker).
-behaviour(gen_fsm).
-author("Martin Sustek").
-date({2013,04,17}).
-version("1.0").

-export([start_link/1, init/1, handle_info/3, handle_event/3, handle_sync_event/4,
    code_change/4, terminate/3, waiting/2, finished/1]).

%% Record holding pending and completed services alongside with Pid of requester

-record(state, {pid,
                ref,
                timestamp,
                pending=[],
                act,
                completed=[]}).

%% API to start worker and begin collecting services results

start_link({Pid, Ref, Services}) ->
    io:format("Starting [Worker].~n"),
    gen_fsm:start_link(?MODULE, {Pid, Ref, Services}, []).

%% Gen_fsm functions

handle_info(_Info, StateName, Data) ->
    % io:format("[Worker] handled unexpected info.~n"),
    {next_state, StateName, Data}.

handle_event(stop, _State, Data) ->
    {stop, normal, Data};

handle_event(_Event, StateName, Data) ->
    % io:format("[Worker] handled unexpected event.~n"),
    {next_state, StateName, Data}.

handle_sync_event(_Event, _From, StateName, Data) ->
    % io:format("[Worker] handled unexpected synchronous event.~n"),
    {next_state, StateName, Data}.

code_change(_OldVsn, StateName, Data, _Extra) ->
    {ok, StateName, Data}.

init({Pid, Ref, [First|Services]}) ->
    gen_server:cast({global, b_cache}, {peek, {First, self()}}),
    {Mega, Sec, Micro} = now(),
    Timestamp = Mega * 1000000 * 1000000 + Sec * 1000000 + Micro,
    {ok, waiting, #state{pid=Pid, ref=Ref, timestamp=Timestamp, act=First, pending=Services}}.

%% Custom states

%% Waiting for partial result
waiting({cache_result, {false, {Service, Ref}}, InsToCache}, S = #state{act={Service, Arg, Ref}, pending=[], completed=Completed}) ->
    io:format("[Worker] received NEGative cache result for: --~p--.~n", [Service]),
    LbName = list_to_atom(atom_to_list(b_lb_) ++ atom_to_list(Service)),
    case global:whereis_name(LbName) of
        undefined ->
            second_lb_check(Service, LbName, {get_result, {Ref, Arg, self()}, InsToCache});
        LbPid ->
            gen_server:cast(LbPid, {get_result, {Ref, Arg, self()}, InsToCache})
    end,
    check_timeout({next_state, waiting, S#state{act=nil, completed=[{pending, Service, Ref, []}|Completed]}});

waiting({cache_result, {false, {Service, Ref}}, InsToCache}, S = #state{act={Service, Arg, Ref}, pending=[Act|Pending], completed=Completed}) ->
    io:format("[Worker] received NEGative cache result for: --~p--.~n", [Service]),
    LbName = list_to_atom(atom_to_list(b_lb_) ++ atom_to_list(Service)),
    case global:whereis_name(LbName) of
        undefined ->
            second_lb_check(Service, LbName, {get_result, {Ref, Arg, self()}, InsToCache});
        LbPid ->
            gen_server:cast(LbPid, {get_result, {Ref, Arg, self()}, InsToCache})
    end,
    gen_server:cast({global, b_cache}, {peek, {Act, self()}}),
    check_timeout({next_state, waiting, S#state{act=Act, pending=Pending, completed=[{pending, Service, Ref, []}|Completed]}});

waiting({cache_result, {Result, {Service, Ref}}}, S = #state{completed=Completed, act={Service, _, Ref}, pending=[Act|Pending]}) ->
    io:format("[Worker] received POSitive cache result for: --~p--.~n", [Service]),
    test(S#state{act=Act, pending=Pending, completed=[{done, Service, Ref, Result}|Completed]}, waiting, true);

waiting({cache_result, {Result, {Service, Ref}}}, S = #state{completed=Completed, act={Service, _, Ref}, pending=[]}) ->
    io:format("[Worker] received POSitive cache result for last request for: --~p--.~n", [Service]),
    test(S#state{act=nil, completed=[{done, Service, Ref, Result}|Completed]}, waiting, true);

waiting({server_result, {Result, {Service, Ref}}}, S = #state{completed=Completed}) ->
    io:format("[Worker] received asynchronous Server result for: --~p--.~n", [Service]),
    case lists:keysearch(Ref, 3, Completed) of
        {value, {pending, Service, Ref, []}} ->
            NewCompleted = lists:keyreplace(Ref, 3, Completed, {done, Service, Ref, Result}),
            test(S#state{completed=NewCompleted}, waiting, false);
        false ->
            check_timeout({next_state, waiting, S})
    end;

waiting(_Event, State) ->
    % io:format("[Worker] in state --WAITING-- handled UNexpected event: ~p~n", [Event]),
    {next_state, waiting, State}.

%% Test if collected all partial results
testService({pending, _, _, []}) ->
    false;
testService({done, _, _, _}) ->
    true.

test(S = #state{pending=[], act=nil, completed=Completed}, NextState, _) ->
    io:format("[Worker] got partial result and is Testing if it's last.~n"),
    case lists:all(fun testService/1, Completed) of
        true ->
            finished(S);
        false ->
            check_timeout({next_state, NextState, S})
    end;
test(S = #state{pending=[], act=Act}, NextState, true) ->
    io:format("[Worker] Tested partial result that wasn't last.~n"),
    gen_server:cast({global, b_cache}, {peek, {Act, self()}}),
    check_timeout({next_state, NextState, S});
test(S, NextState, false) ->
    io:format("[Worker] Tested partial result that wasn't last.~n"),
    check_timeout({next_state, NextState, S});
test(S = #state{act=Act}, NextState, true) ->
    io:format("[Worker] Tested partial result that wasn't last.~n"),
    gen_server:cast({global, b_cache}, {peek, {Act, self()}}),
    check_timeout({next_state, NextState, S});
test(S, NextState, false) ->
    io:format("[Worker] Tested partial result that wasn't last.~n"),
    check_timeout({next_state, NextState, S}).

check_timeout({next_state, NextState, S = #state{timestamp=Timestamp}}) ->
    {Mega, Sec, Micro} = now(),
    Current = Mega * 1000000 * 1000000 + Sec * 1000000 + Micro,
    case ( abs(Current-Timestamp) > application:get_env(b_master, worker_timeout, nil) ) of
        true ->
            io:format("[Worker] reached timeout and is sending incomplete result to requester.~n"),
            timeouted(S);
        false ->
            {next_state, NextState, S}
    end.

%% Got all partial results, so it's possible to combine them and send them to requester
finished(S = #state{pid=Pid, ref = Ref, completed=Completed}) ->
    io:format("[Worker] combining partial results into Final one.~n"),
    Result = lists:foldr(fun({done, _, _, X}, Acc) -> Acc++X end, "", Completed),
    Pid ! { Ref, "<html><head></head><body bgcolor='#e8e8e8'>"++Result },
    {stop, normal, S}.

timeouted(S = #state{pid=Pid, ref=Ref, completed=Completed, pending=Pending, act=Act}) ->
    io:format("[Worker] combining INCOMPLETE partial results into Final one.~n"),
    Comp = lists:foldr(fun(X, Acc) -> case X of {done, _, _, Val} -> Acc++Val; {pending, Service, _, _} -> Acc++"<div id='"++Service++"-service-container'></div>" end end, "", Completed),
    Pend = lists:foldl(fun({Service, _, _}, Acc) -> Acc++"<div id='"++Service++"-service-container'></div>" end, "", Pending),
    case Act of
        {Service, _, _} -> Rem = Pend++"<div id='"++Service++"-service-container'></div>";
        nil -> Rem = Pend
    end,
    Result = Comp++Rem,
    Pid ! { Ref, "<html><head></head><body>"++Result },
    {stop, normal, S}.

%% All results collected or another reason to terminate worker.

terminate(normal, ready, _State) ->
    % io:format("[Worker] finished task.~n"),
    ok;
terminate(_Reason, _StateName, _StateData) ->
    ok.

%% Functionality checking existence of load balancer and possibly spawning one

second_lb_check(Service, LbName, Request) ->
    timer:sleep(net_kernel:get_net_ticktime()),
    case global:whereis_name(LbName) of
        undefined ->
            case global:whereis_name(b_balancer_sup) of
                undefined -> nil;
                Pid ->
                    case node(Pid) of
                        nonode@nohost -> nil;
                        Node ->
                            rpc:call(Node, b_balancer, add_load_balancer, [Service]),
                            gen_server:cast({global, LbName}, Request)
                    end
            end;
        LbPid ->
            gen_server:cast(LbPid, Request)
    end.
