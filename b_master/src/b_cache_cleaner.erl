%% @author Martin Šustek <xsustek.martin@gmail.com>
%% @doc Cache cleaner
%%
%% The module contains an implementation of the Erlang framework Cache cleaner
%% which cleans cache from unused or longterm items every period
%% @copyright 2014 Martin Šustek

-module(b_cache_cleaner).
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
    % io:format("Starting [CacheCleaner].~n"),
    gen_server:start_link({local, b_cache_cleaner}, ?MODULE, [], []).

init([]) ->
    self() ! enter_main_loop,
    {ok, {cache, pending}}.

handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(enter_main_loop, State) ->
    {ok, CacheCleanTimeout} = application:get_env(b_master, cache_clean_timeout),
    timer:sleep(CacheCleanTimeout),
    perform_cleanup(State, 0),
    {noreply, State};

handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

perform_cleanup({Cache, Pending}, Looped) ->
    {ok, CacheSize} = application:get_env(b_master, cache_size),
    case (Looped < CacheSize) of
        true ->
            MinCache = ets:foldl(
                fun(I=#item{transit=Transit, occur=Occur}, Acc=#item{occur=AccOccur}) ->
                    case Transit of
                        true -> Acc;
                        false ->
                            case AccOccur of
                                nil -> I;
                                _ ->
                                    case (AccOccur>Occur) of
                                        true -> I;
                                        false -> Acc
                                    end
                            end
                    end
                end,
                #item{occur=nil},
                Cache
            ),
            MaxPending = ets:foldl(
                fun(Itm=#item{transit=Transit, occur=Occur}, Acc=#item{occur=AccOccur}) ->
                    case Transit of
                        true -> Acc;
                        false ->
                            case AccOccur of
                                nil -> Itm;
                                _ ->
                                    case (AccOccur<Occur) of
                                        true -> Itm;
                                        false -> Acc
                                    end
                            end
                    end
                end,
                #item{occur=nil},
                Pending
            ),
            case MaxPending of
                #item{occur=nil} ->
                    clear_flags({Cache, Pending});
                _ ->
                    perform_checkup({Cache, Pending}, MinCache, MaxPending, Looped)
            end;
        false ->
            clear_flags({Cache, Pending})
    end.

perform_checkup({Cache, Pending}, #item{key=MinKey, occur=MinOccur}, #item{key=MaxKey, occur=MaxOccur}, Looped) ->
    case MinOccur of
        nil ->
            ets:update_element(Pending, MaxKey, {#item.transit, true}),
            perform_cleanup({Cache, Pending}, (Looped+1));
        _ ->
            case (MinOccur < MaxOccur) of
                true ->
                    ets:update_element(Cache, MinKey, {#item.transit, true}),
                    ets:update_element(Pending, MaxKey, {#item.transit, true}),
                    perform_cleanup({Cache, Pending}, (Looped+1));
                false ->
                    clear_flags({Cache, Pending})
            end
    end.

clear_flags({Cache, Pending}) ->
    ets:foldl(
        fun(#item{key=Key, occur=Occur}, _Acc) ->
            case (Occur /= 0) of
                true ->
                    ets:update_element(Pending, Key, {#item.occur, 0});
                false ->
                    #item{}
            end
        end,
        #item{},
        Pending
    ),
    ets:foldl(
        fun(#item{key=Key, occur=Occur}, _Acc) ->
            case (Occur /= 0) of
                true ->
                    ets:update_element(Cache, Key, {#item.occur, 0});
                false ->
                    #item{}
            end
        end,
        #item{},
        Cache
    ),
    self() ! enter_main_loop.