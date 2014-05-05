%% @author Martin Šustek <xsustek.martin@gmail.com>
%% @doc Service broker
%%
%% The module contains an implementation of the Erlang framework Service broker
%% which extends OTP gen_server class and handles requests. This consists of breaking
%% requests into separate services and spawning worker that will collect their results
%% @copyright 2014 Martin Šustek

-module(b_service_broker).
-behaviour(gen_server).
-author("Martin Sustek").
-date({2013,03,25}).
-version("1.0").

-export([init/1, handle_call/3, handle_cast/2, code_change/3,
        handle_info/2, start_link/0, terminate/2]).

%%% Gen server functions

start_link() ->
    % io:format("Starting [ServiceBroker]~n"),
    gen_server:start_link({global, b_service_broker}, ?MODULE, [], []).

init([]) ->
    {ok, []}.

handle_call({index, Ref, Pid}, {Pid, _Ref}, State) ->
    % io:format("Handling --index-- event in [ServiceBroker]~n"),
    WorkerPid = b_worker:start_link(
        {
            Pid,
            Ref,
            [
                {header, erlang:now(), make_ref()},
                % {infected, erlang:now(), make_ref()},
                {caching, [], make_ref()},
                {content, [], make_ref()},
                {timeouted, [], make_ref()}
            ]
        }
    ),
    {reply, WorkerPid, State};

handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.