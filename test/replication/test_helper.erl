%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%

-module(test_helper).

-export([start_master/0, stop_master/0, start_slave/2, stop_slave/1]).
-export([ensure_loaded/1]).

%%% CT Macros
-include_lib("test/replication/include/ct.hrl").

start_master() -> start_master(?NODE, ?COOKIE).

start_master(LongName, Cookie) ->
    %% Starting a master node with Distributed Erlang
    {ok, i_am_master} = replication:init(LongName, Cookie, master).

stop_master() ->
    %% turn local noerlang:get_cookie().de from distributed mode to non-distribted
    {ok, disconnected_from_cluster} = replication:stop(normal),
    ok = net_kernel:stop().

start_slave(Name, Node) ->
    %% Starting a slave node with Distributed Erlang
    Cookie = erlang:get_cookie(),
    ok = ct:pal("Slave:start slave_ip=\"~p\" node_name=\"~p\" cookie=\"~p\"",[?SLAVE_IP, Name, Cookie]),
    {ok, State} = replication:init(Name, Cookie),
    ok = ct:pal("Slave \"~p\" \"~p\" \"~p\"",[Name, State, Node]),
    %ok = ct_rpc:call(Node, code, add_pathsz, [code:get_path()], 5000, Cookie),
    %% Start the application remotely
    %{module, test_helper} = ct_rpc:call(Node, code, ensure_loaded, [test_helper], 5000, Cookie),
    ok.

stop_slave(Name) ->
    ok = slave:stop(Name).

ensure_loaded(App) ->
  case application:load(App) of
      ok ->
          ok;
      {error,{already_loaded,App}} ->
          ok;
      Error ->
          throw({"failed to load", App, Error})
  end.
