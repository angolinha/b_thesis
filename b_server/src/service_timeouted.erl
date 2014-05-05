%% @author Martin Šustek <xsustek.martin@gmail.com>
%% @doc Header service
%%
%% The module contains an implementation of the Erlang framework Header service
%% which renders page header with logo and random advertising
%% @copyright 2014 Martin Šustek

-module(service_timeouted).
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
                {timeouted, Ref}
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
                {timeouted, Ref}
            }
        }
    ),
    gen_server:cast({global, b_cache}, {cache_result, {timeouted, Arg, Result}}).

get_result(Arg) ->
    count_pi(1, (1/math:sqrt(2)), (1/4), 1, 0, 3000),
    "<div id='timeouted-service-container'"++
    "style='width:20%;height:70%;border-style:solid;position:absolute;"++
    "background-color:#abc841;right:10%;top:23%;text-align:center;'>"++
    "<div style='margin:10px;font-size:12pt;'>"++
    "<p>ZLOZITY VYPOCET TRVAJUCI DLHSIE AKO POVOLUJE TIMEOUT</p>"++
    "<p>SLUZBA SPOTREBUVA PROCESOROVY CAS, NO VZDY ZLYHA A STRANKA</p>"++
    "<p>BUDE VYKRESLENA BEZ NEJ</p></div>"++
    "</div>".

count_pi(A, B, T, P, Count, Stop) ->
    case (Count+1 > Stop) of
        true ->
            nil;
        false ->
            An = round((A+B)/2),
            Bn = round(math:sqrt(A*B)),
            Tn = round( T - (P* ( An*An-Bn*Bn )) ),
            Pn = round(2*P),
            count_pi(An, Bn, Tn, Pn, (Count+1), Stop)
    end.