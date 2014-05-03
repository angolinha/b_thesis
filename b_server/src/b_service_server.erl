%% @author Martin Šustek <xsustek.martin@gmail.com>
%% @doc Service server
%%
%% The module contains an implementation of the Erlang framework Service server
%% which provides interface for execution of certain service that is computationally intensive
%% @copyright 2014 Martin Šustek

-module(b_service_server).
-behaviour(gen_server).
-author("Martin Sustek").
-date({2013,03,25}).
-version("1.0").

-export([init/1, handle_call/3, handle_cast/2, code_change/3, handle_info/2, start_link/1, terminate/2]).

-record(state, {lbName,
                service,
                active}).

%%% Gen server functions

start_link({Service, LbName, ServerName}) ->
    io:format("Starting [ServiceServer].~n"),
    gen_server:start_link({local, ServerName}, ?MODULE, {Service, LbName}, []).

init({Service, LbName}) ->
    gen_server:cast({global, LbName}, {server_pid, node(), self(), 0}),
    {ok, #state{service=Service, lbName=LbName, active=0}}.

handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast({serve, Arg, InsToCache}, S=#state{service=Service, active=Active}) ->
    io:format("Handling request in [ServiceServer] and spawning specific service instance.~n"),
    Module = list_to_atom(atom_to_list(service_) ++ atom_to_list(Service)),
    spawn_link(Module, run, [{Arg, InsToCache}]),
    {noreply, S#state{active=(Active+1)}}.

handle_info({'EXIT', _, _}, S=#state{lbName=LbName, active=Active}) ->
    io:format("One of [ServiceServer]'s workers finished task.~n"),
    gen_server:cast({global, LbName}, {service_instance_finished, self()}),
    {noreply, S#state{active=(Active-1)}};

handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    io:format("[ServiceServer] instance killed in process of downgrade sleeping, so it's workers can finish.~n"),
    timer:sleep(1000),
    ok.