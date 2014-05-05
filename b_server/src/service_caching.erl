%% @author Martin Šustek <xsustek.martin@gmail.com>
%% @doc Header service
%%
%% The module contains an implementation of the Erlang framework Header service
%% which renders page header with logo and random advertising
%% @copyright 2014 Martin Šustek

-module(service_caching).
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
                {caching, Ref}
            }
        }
    );

run({{Ref, Arg, WorkerPid}, true}) ->
    % io:format("This result should be inserted to cache!~n"),
    Result = get_result(Arg),
    gen_fsm:send_event(
        WorkerPid,
        {
            server_result,
            {
                Result,
                {caching, Ref}
            }
        }
    ),
    gen_server:cast({global, b_cache}, {cache_result, {caching, Arg, Result}}).

get_result(Arg) ->
    count_pi(1, (1/math:sqrt(2)), (1/4), 1, 0, 1000),
    {{Year,Month,Day},{Hour,Min,Sec}} = erlang:localtime(),
    "<div id='caching-service-container'"++
    "style='width:20%;height:70%;border-style:solid;position:absolute;"++
    "background-color:#abc841;right:70%;top:23%;text-align:center;'>"++
    "<div style='margin:10px;font-size:12pt;'>"++
    "<p>ODPORUCAME VAM TIEZ:</p><p>Podobne produkty...</p>"++
    "<p>Dnes je: "++io_lib:format("~p.~p.~p",[Day, Month, Year])++"<br/>"++io_lib:format("~p:~p:~p",[Hour, Min, Sec])++"</p>"++
    "<p>AK SA TATO HODNOTA NEMENI, PREBEHLO KESOVANIE.</p></div>"++
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