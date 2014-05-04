%% @author Martin Šustek <xsustek.martin@gmail.com>
%% @doc Header service
%%
%% The module contains an implementation of the Erlang framework Header service
%% which renders page header with logo and random advertising
%% @copyright 2014 Martin Šustek

-module(service_infected).
-author("Martin Sustek").
-date({2013,03,25}).
-version("1.0").

-export([run/1]).

%%% Gen server functions

run({{Ref, Arg, WorkerPid}, false}) ->
    Result = get_result(Arg),
    gen_fsm:send_event(
        WorkerPid,
        {
            server_result,
            {
                Result,
                {infected, Ref}
            }
        }
    );

run({{Ref, Arg, WorkerPid}, true}) ->
    Result = get_result(Arg),
    gen_fsm:send_event(
        WorkerPid,
        {
            server_result,
            {
                Result,
                {infected, Ref}
            }
        }
    ),
    gen_server:cast({global, b_cache}, {cache_result, {infected, Arg, Result}}).

get_result(Arg) ->
    random:seed(Arg),
    Iter = random:uniform(12),
    Part = dump(Iter),
    io:format("~p going down!~n", [Part]),
    "<div id='infected-service-container'"++
    "style='width:80%;height:10%;border-style:solid;position:absolute;"++
    "background-color:#777777;right:10%;top:12%;text-align:center;'>"++
    "<div style='margin-top:1%;font-weight:bold;font-size:13pt;color:#ffffff;'>"++
    "Aktualne NAPADNUTY prvok architektury - "++Part++" </div>"++
    "<div style='font-size:11pt;color:#ffffff;'>(tato sluzba nahodne zhadzuje "++
        "prvky architektury, co dokazuje fault-tolerance - su obnovene a "++
        "nacitavanie stranky prebehne bez povsimnutia alebo len potrva dlhsie)</div>"++
    "</div>".

pick_service_server(Num, Service, Uppercase) ->
    Nodes = lists:filter(fun(X) -> case string:str(atom_to_list(X), "b_server") of 1 -> true; 0 -> false end end, nodes()),
    Servers = lists:foldl(fun(Node, Acc) ->
            case rpc:call(Node, b_server, check_registered, [Service]) of
                undefined -> Acc;
                Pid -> [{Pid, Node}|Acc]
            end
        end, [], Nodes),
    case Servers of
        [] ->
            dump(Num);
        _ ->
            Index = random:uniform(length(Servers)),
            {Pid, Node} = lists:nth(Index,Servers),
            gen_server:cast(Pid, stop),
            io_lib:format("~p SERVICE SERVER at NODE ~p", [Uppercase, Node])
    end.

dump(1) ->
    gen_server:cast({global, b_service_broker}, stop),
    "SERVICE BROKER";
dump(2) ->
    gen_server:cast({global, b_cache}, stop),
    "CACHE";
dump(3) ->
    case global:whereis_name(b_lb_caching) of
        undefined ->
            dump(4);
        Pid ->
            gen_server:cast(Pid, stop),
            "CACHING SERVICE LOAD BALANCER"
    end;
dump(4) ->
    case global:whereis_name(b_lb_content) of
        undefined ->
            dump(5);
        Pid ->
            gen_server:cast(Pid, stop),
            "CONTENT SERVICE LOAD BALANCER"
    end;
dump(5) ->
    case global:whereis_name(b_lb_header) of
        undefined ->
            dump(6);
        Pid ->
            gen_server:cast(Pid, stop),
            "HEADER SERVICE LOAD BALANCER"
    end;
dump(6) ->
    case global:whereis_name(b_lb_infected) of
        undefined ->
            dump(7);
        Pid ->
            gen_server:cast(Pid, stop),
            "INFECTED SERVICE LOAD BALANCER"
    end;
dump(7) ->
    case global:whereis_name(b_lb_timeouted) of
        undefined ->
            dump(8);
        Pid ->
            gen_server:cast(Pid, stop),
            "TIMEOUTED SERVICE LOAD BALANCER"
    end;
dump(8) ->
    pick_service_server(9, caching, "CACHING");
dump(9) ->
    pick_service_server(10, content, "CONTENT");
dump(10) ->
    pick_service_server(11, header, "HEADER");
dump(11) ->
    pick_service_server(12, infected, "INFECTED");
dump(12) ->
    pick_service_server(1, timeouted, "TIMEOUTED").