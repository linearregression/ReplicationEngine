%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%

-module(replication_SUITE).

-include_lib("test/replication/include/ct.hrl").

%%% Common Test callbacks
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

-export([cluster_of_one/1, 
         master_of_zero/1,
         master_of_one/1,
         slave_of_one/1,
         master_join_cluster/1,
         master_leave_cluster/1,
         slave_join_cluster/1,
         slave_leave_cluster/1,
         observer_join_cluster/1,
         observer_leave_cluster/1,
         promote_slave_to_master/1,
         promote_observer_to_master/1,
         demote_master_to_slave/1,
         demote_master_to_observer/1,
         split_brain/1 
        ]).

all() ->
    {exports, Functions} = lists:keyfind(exports, 1, ?MODULE:module_info()),
    [FName || {FName, _} <- lists:filter(
                               fun ({module_info,_}) -> false;
                                   ({all,_}) -> false;
                                   ({init_per_suite,1}) -> false;
                                   ({end_per_suite,1}) -> false;
                                   ({_,1}) -> true;
                                   ({_,_}) -> false
                               end, Functions)].

init_per_suite(Config) ->
    test_helper:start_master(),
    ok = ct:pal("Master started"),
    Config.

end_per_suite(_Config) ->
    %test_helper:stop_master(),
    %ok = ct:pal("Master \"~p\" stopped",[?NODE]),
    ok.

init_per_testcase(_, Config) ->
    ok = ct:pal("Init per testcase"),
    true = erlang:set_cookie(node(), ?COOKIE),
    test_helper:start_slave(?SLAVE1, ?SLAVE1_NAME),
%    ok = ct:pal(" Slave1 \"~p\" started", [?SLAVE1]),
    test_helper:start_slave(?SLAVE2, ?SLAVE2_NAME),
%    ok = ct:pal(" Slave2 \"~p\" started", [?SLAVE2]),
    test_helper:start_slave(?OBSERVER, ?OBSERVER_NAME),
    ok = ct:pal(" Slaves/Observers \"~p\" started", [?SLAVE1]),
    Config.

end_per_testcase(_, _Config) ->
    ok = ct:pal("End per testcase"),
    test_helper:stop_slave(?SLAVE1),
%    ok = ct:pal(" Slave \"~p\" stopped", [?SLAVE1]),
    test_helper:stop_slave(?SLAVE2),
%    ok = ct:pal(" Slave \"~p\" stopped", [?SLAVE2]),
    test_helper:stop_slave(?OBSERVER),
    ok = ct:pal("Slaves/Observers \"~p\" stopped",[?OBSERVER]),
    ok.

%% Test main functions
cluster_of_one(_Config) ->
    ok = ct:pal("Testing [cluster_of_one]"),
 %   Ret = replication:refresh(),
 %   ok = ct:pal("Cluster members: \"~p\" ", [Ret]),
 %   yes = replication:am_I_Master(?NODE),
 %   no = replication:am_I_Slave(?NODE),
 %   {ready, [?NODE,[],[]]} = replication:refresh(),
 %   ok = replication:replicate(),
 %   {error, {not_permitted, {}}} = replication:stop(normal),
    ok = ct:pal("Each cluster must have one master."),
    ok.

master_of_zero(_Config) ->
    ok = ct:pal("Testing [master_of_zero]"),
 %   Ret = replication:refresh(),
 %   ok = ct:pal("Cluster members: \"~p\" ", [Ret]),
 %   yes = replication:am_I_Master(?NODE),
 %   {error, {already_master, {}}}  = replication:join_cluster(?NODE),
 %   no = replication:am_I_Slave(?NODE),
 %   {ready, [?NODE,[],[]]} = replication:refresh(),
 %   {ok, disconnected_from_cluster} =replication:leave_cluster(),
 %   {ready, [[],[],[]]} = replication:refresh(),
 %   {error, {not_permitted, {}}} = replication:stop(normal),
    ok = ct:pal("Each cluster must have one master."),
    ok.

master_of_one(_Config) ->
    ok = ct:pal("Testing [master_of_one]"),
 %   Ret = replication:refresh(),
 %   ok = ct:pal("Cluster members: \"~p\" ", [Ret]),
 %   yes = replication:am_I_Master(?NODE),
 %   {error, {already_master, {}}}  = replication:join_cluster(?SLAVE1),
 %   no = replication:am_I_Slave(?SLAVE1),
 %   {ready, [?NODE,[?SLAVE1],[?SLAVE1]]} = replication:refresh(),
 %   {ok, disconnected_from_cluster} =replication:leave_cluster(),
 %   {ready, [[],[],[]]} = replication:refresh(),
 %   {error, {not_permitted, {}}} = replication:stop(normal),
    ok = ct:pal("Each cluster must have one master."),
    ok.

slave_of_one(_Config) ->
    ok = ct:pal("Testing [slave_of_one]").

slave_join_cluster(_Config) ->
    ok = ct:pal("Testing [slave_join_cluster]").

slave_leave_cluster(_Config) ->
    ok = ct:pal("Testing [slave_leave_cluster").

master_join_cluster(_Config) ->
    ok = ct:pal("Testing [master_join_cluster]").

master_leave_cluster(_Config) ->
    ok = ct:pal("Testing [master_leave_cluster").

observer_join_cluster(_Config) ->
    ok = ct:pal("Testing [observer_join_cluster]").

observer_leave_cluster(_Config) ->
    ok = ct:pal("Testing [promote_slave_to_master]").

promote_slave_to_master(_Config) ->
    ok = ct:pal("Testing [promote_slave_to_master]").

promote_observer_to_master(_Config) ->
    ok = ct:pal("Testing [promote_observer_to_master]").

demote_master_to_slave(_Config) ->
    ok = ct:pal("Testing [demote_master_to_slave]").

demote_master_to_observer(_Config) ->
    ok = ct:pal("Testing [demote_master_to_observer]").

split_brain(_Config) ->
    ok = ct:pal("Testing [split_brain]").


