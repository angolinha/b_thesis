%%% API module for the bachelor thesis application's master part
-module(b_master).
-behaviour(application).
-export([stop/1, start/2]).

start(normal, _Args) ->
    io:format("Starting [MasterPart]~n"),
    erlang:set_cookie(xsustekm_b_thesis, node()),
    b_master_sup:start_link();

start({takeover, _OtherNode}, []) ->
    io:format("Taking over [MasterPart] after node restart.~n"),
    erlang:set_cookie(xsustekm_b_thesis, node()),
    b_master_sup:start_link();

start({failover, _OtherNode}, []e) ->
    io:format("Starting [MasterPart] after primary node failure.~n"),
    erlang:set_cookie(xsustekm_b_thesis, node()),
    b_master_sup:start_link().

stop(_State) ->
    ok.