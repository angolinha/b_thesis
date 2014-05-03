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
    Iter = random:uniform(9),
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

turn_off(Part) ->
    gen_server:cast({global, Part}, stop).

switch_off(Part) ->
    supervisor:restart_child({global, b_thesis_sup}, Part).

dump(1) ->
    turn_off(b_adapter),
    "ADAPTER";
dump(2) ->
    turn_off(b_cache),
    "CACHE";
dump(3) ->
    turn_off(b_cache_cleaner),
    "CACHE CLEANER";
dump(4) ->
    switch_off(b_cache_sup),
    "CACHE SUPERVISOR";
dump(5) ->
    turn_off(b_event_mgr),
    "EVENT MANAGER";
dump(6) ->
    switch_off(b_usr_chain),
    "USER CHAIN SUPERVISOR";
dump(7) ->
    switch_off(b_lb_pool),
    "LOAD BALANCER POOL";
dump(8) ->
    turn_off(b_reg_heir),
    "LOAD BALANCER REGISTER HEIR";
dump(9) ->
    turn_off(b_lb_register),
    "LOAD BALANCER REGISTER".