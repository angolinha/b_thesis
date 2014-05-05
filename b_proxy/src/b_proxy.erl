%%% Proxy module for my bachelor thesis application working simultaneously as adapter.
%%% It provides abstraction from used webserver and makes routing to different
%%% nodes where application is running possible.
-module(b_proxy).
-behaviour(application).
-export([stop/1, start/2, out/1, try_receive/3]).
-include_lib("yaws_api.hrl").

start(normal, _Args) ->
    {ok, self()};
start(takeover, _OtherNode) ->
    {ok, self()}.

stop(_State) ->
    ok.

out(A) ->
    erlang:set_cookie(xsustekm_b_thesis, node()),
    Self = self(),
    spawn(fun() ->
        Ref = make_ref(),
        % io:format("Sending request with arg: --~p-- to ServiceBroker in [Proxy]~n", [list_to_atom(A#arg.appmoddata)]),
        {ok, Pid} = gen_server:call({global, b_service_broker}, {list_to_atom(A#arg.appmoddata), Ref, self()}),
        b_proxy:try_receive(Self, Pid, Ref)
    end),
    {streamcontent, "text/html", ""}.

try_receive(Pid, Worker, Ref) ->
    receive
        {Ref, Data} ->
            yaws_final(Pid, Data++"</body></html>")
    after
        application:get_env(b_proxy, request_timeout, nil) ->
            gen_fsm:send_all_state_event(Worker, stop),
            yaws_final(Pid, "<html><head></head><body>Request timed out!</body></html>")
    end.

yaws_final(Pid, Data) ->
    yaws_api:stream_chunk_deliver(Pid, Data),
    yaws_api:stream_chunk_end(Pid),
    exit(normal).