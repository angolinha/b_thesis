%% @author Martin Šustek <xsustek.martin@gmail.com>
%% @doc Header service
%%
%% The module contains an implementation of the Erlang framework Header service
%% which renders page header with logo and random advertising
%% @copyright 2014 Martin Šustek

-module(service_content).
-author("Martin Sustek").
-date({2013,03,25}).
-version("1.0").

-export([run/1]).

%%% Gen server functions

run({{Ref, Arg, WorkerPid}, false}) ->
    Result = get_result(Arg, WorkerPid),
    gen_fsm:send_event(
        WorkerPid,
        {
            server_result,
            {
                Result,
                {content, Ref}
            }
        }
    );

run({{Ref, Arg, WorkerPid}, true}) ->
    Result = get_result(Arg, WorkerPid),
    gen_fsm:send_event(
        WorkerPid,
        {
            server_result,
            {
                Result,
                {content, Ref}
            }
        }
    ),
    gen_server:cast({global, b_cache}, {cache_result, {content, Arg, Result}}).

get_result(Arg, WorkerPid) ->
    "<div id='content-service-container'"++
    "style='width:40%;height:70%;border-style:solid;position:absolute;"++
    "background-color:#ffffff;right:30%;top:23%;text-align:left;'>"++
    "<div style='margin:10px;font-size:12pt;'>"++
    "<p>NAZOV</p><p>HODNOTENIE ZISKANE Z UZLA: "++io_lib:format("~p", [node()])++
    "<br/>VYTVORENE SERVEROM: "++io_lib:format("~p", [self()])++
    "</p><p>VYSLEDOK ZAPRACOVAL WORKER: "++io_lib:format("~p", [WorkerPid])++"</p>"++
    "<p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer pretium, libero vitae pharetra luctus, quam sem "++
    "dictum tortor, vel elementum tortor neque quis elit. Nunc at posuere eros, sit amet fringilla lectus. Curabitur "++
    "posuere aliquet lorem, in lobortis erat venenatis sit amet. Etiam iaculis massa non luctus suscipit. Nullam vel "++
    "magna sit amet sem adipiscing accumsan sit amet eget arcu. Vestibulum at lacus nec eros ultrices semper sit amet "++
    "non magna. Vivamus libero leo, auctor eget ornare nec, placerat suscipit augue. Sed pulvinar dui in ante pellentesque "++
    "commodo. Pellentesque id quam luctus, tincidunt mi ac, fringilla enim. Cum sociis natoque penatibus et magnis dis "++
    "parturient montes, nascetur ridiculus mus.</p>"++
    "<p>Etiam tempor nulla massa, id facilisis sem lobortis aliquet. Cras vel sapien a diam gravida mollis sit amet non "++
    "metus. Phasellus in auctor massa. Maecenas vehicula bibendum ante id luctus. Maecenas interdum urna sit amet adipiscing "++
    "vulputate. Praesent sagittis, libero vel ullamcorper tempus, turpis neque cursus justo, nec malesuada mauris lacus a felis.</p>"++
    "</div></div>".