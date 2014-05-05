%% @author Martin Šustek <xsustek.martin@gmail.com>
%% @doc Cache
%%
%% The module contains an implementation of the Erlang framework Cache
%% that carries most requested results and returns them on demand
%% @copyright 2014 Martin Šustek

-module(b_cache).
-behaviour(gen_server).
-author("Martin Sustek").
-date({2013,03,25}).
-version("1.0").

-export([init/1, handle_call/3, handle_cast/2, code_change/3, handle_info/2, start_link/0, terminate/2]).
-include_lib("stdlib/include/ms_transform.hrl").

%% Record representing cache item

-record(item, {key,
                value,
                occur=0,
                transit}).

%%% Gen server functions

start_link() ->
    % io:format("Starting [Cache].~n"),
    gen_server:start_link({global, b_cache}, ?MODULE, [], []).

init([]) ->
    ets:new(cache, [ordered_set, public, named_table, {write_concurrency, true}, {keypos, #item.key}]),
    ets:new(pending, [ordered_set, public, named_table, {write_concurrency, true}, {keypos, #item.key}]),
    {ok, 0}.

handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.

handle_cast({peek, {{Service, Arg, Ref}, Pid}}, State) ->
    % io:format("Handling Peek request in [Cache].~n"),
    Key = erlang:phash2({Service, Arg}),
    case ets:lookup(cache, Key) of
        [#item{key=Key, value=Value, occur=Occur}] ->
            ets:update_element(cache, Key, {#item.occur, (Occur+1)}),
            gen_fsm:send_event(Pid, { cache_result, { Value, {Service, Ref} } });
        [] ->
            % io:format("Result NOT in cache~n"),
            check_pending({{Service, Ref}, Key, Pid})
    end,
    {noreply, State};

handle_cast({cache_result, {Service, Arg, Result}}, CacheSize) ->
    case (CacheSize == application:get_env(b_master, basic_server_capacity, nil)) of
        true ->
            Key = erlang:phash2({Service, Arg}),
            [I=#item{key=Key}] = ets:lookup(pending, Key),
            {[#item{key=OldKey}], _} = ets:select(cache, ets:fun2ms(fun(N=#item{transit=T}) when T == true -> N end), 1),
            ets:delete(cache, OldKey),
            ets:insert(cache, I#item{value=Result, transit=false}),
            ets:delete(pending, Key),
            {noreply, CacheSize};
        false ->
            Key = erlang:phash2({Service, Arg}),
            [I=#item{key=Key}] = ets:lookup(pending, Key),
            ets:insert(cache, I#item{value=Result, transit=false}),
            ets:delete(pending, Key),
            {noreply, (CacheSize+1)}
    end;

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

check_pending({{Service, Ref}, Key, Pid}) ->
    case ets:lookup(pending, Key) of
        [#item{key=Key, occur=Occur, transit=true}] ->
            % io:format("Item in pending CACHING~n"),
            ets:update_element(pending, Key, {#item.occur,(Occur+1)}),
            gen_fsm:send_event(Pid, { cache_result, { false, {Service, Ref} }, true });
        [#item{key=Key, occur=Occur, transit=false}] ->
            % io:format("Item in pending WITHOUT TRANSIT flag~n"),
            ets:update_element(pending, Key, {#item.occur,(Occur+1)}),
            gen_fsm:send_event(Pid, { cache_result, { false, {Service, Ref} }, false });
        [] ->
            % io:format("Item NOT IN pending. Adding~n"),
            ets:insert(pending, #item{key=Key, value=nil, occur=1, transit=false}),
            gen_fsm:send_event(Pid, { cache_result, { false, {Service, Ref} }, false })
    end.