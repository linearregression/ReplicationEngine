%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%

-module(replication).
-author("Jon Vlachoyiannis").

-behaviour(application).

-export([start/0,
         start/2,
         stop/1]).

-export([init/2,
         init/3,
         refresh/0,
         join_cluster/1,
         join_cluster/2,
         who_is_master/0,
         replicate/0,
         make_Master/1,
         make_Observer/1,
         am_I_Master/1,
         am_I_Slave/1,
         test_slave/1]).

-include("node_records.hrl").
-include("replication.hrl").

start() ->
    ok.

start(_Type, _Args) ->
    ok.

% you need to start epmd first
% try running an erl -name smth
init(LongName, Cookie, master) when is_atom(LongName), is_atom(Cookie)->
    ok = replication_helper:start_node(LongName, Cookie),
    % starting mnesia
    ok = mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    case mnesia:create_table(ldb_nodes, [{type, set},
                                         {ram_copies,[node()]},
                                         {local_content, false},
                                         {attributes, record_info(fields, ldb_nodes)}])  of
         {atomic, ok} ->
            {atomic, ok} = mnesia:create_table(node_info, [{type, set},
                                            {ram_copies,[node()]},
                                            {local_content, false},
                                            {attributes, record_info(fields, node_info)}]),
            make_Master(node());
         Error ->
            {error, {failed_master, Error}}
    end.
  

init(LongName, Cookie) ->
    ok = replication_helper:start_node(LongName, Cookie),
    % starting mnesia for replication slave
    case mnesia:start() of
        ok ->
            {ok, ready_to_join};
        Error ->
            {error, {failed_to_start, Error}}
    end.


stop(_Reason) ->
    Ret = leave_cluster(),
    ok = net_kernel:stop(),
    stopped = mnesia:stop(),
    Ret.

% Returns a new Master based
% on who was the last the got replicated data
elections(LiveNodes) ->
    NodesInfo = lists:flatten(lists:map(fun(X) -> mnesia:dirty_read(node_info, X) end, 
                                        LiveNodes)),
    
    ?CONSOLE("XA ~p ~p ~n", [NodesInfo, LiveNodes]),
                          
    SortedNodes = lists:reverse(lists:map(fun(Y) -> element(2, Y) end,
                                         lists:keysort(3, NodesInfo))),
                                
    ?CONSOLE("Sorted nodes ~p ~n", [SortedNodes]),
    SortedNodes.
    
%
% Returns the nodes that are active,
% cleaning the table of dead nodes
% and elects a new Master if there is no Master
%
refresh() ->
    case mnesia:dirty_read(ldb_nodes, "ldbNodes") of
        [Data] -> 
            {ldb_nodes, _, MasterNode, SlaveNodes, ObserverNodes} = Data,

            % remove slave nodes that are down
            SlaveNodesUp = lists:filter(fun(X) -> check_if_alive(X) end, 
                                        SlaveNodes),

            % remove observer nodes that are down
            ObserverNodesUp = lists:filter(fun(X) -> check_if_alive(X) end,
                                           ObserverNodes),
            
            ok = mnesia:dirty_write(#ldb_nodes{clusterID="ldbNodes", master_node=MasterNode, 
                                          slave_nodes=sets:to_list(sets:from_list(SlaveNodesUp)),
                                          observer_nodes=sets:to_list(sets:from_list(ObserverNodesUp))}),

            % if Master node is down, re-elect a new node
            MasterStatus = check_if_alive(MasterNode),
            if MasterStatus =:= false ->
                    ?CONSOLE("---> No master node, going to elections ~n", []),
                    [NewMaster|NewSlaves] = elections(SlaveNodesUp),
                    ok = mnesia:dirty_write(#ldb_nodes{clusterID="ldbNodes", master_node=NewMaster, slave_nodes=NewSlaves, observer_nodes=ObserverNodesUp}),
                    {ready, [NewMaster, NewSlaves, ObserverNodesUp]};
               true -> 
                    {ready, [MasterNode, SlaveNodesUp, ObserverNodesUp]}
            end;
        Error ->
            {error, {not_connected, Error}}
    end.


check_if_alive(NodeName) ->
    NodeResp = net_adm:ping(NodeName),
    if NodeResp =:= pong -> true;
       true -> false
    end.


%
% Join cluster as slave
%
join_cluster(ClusterName) ->
    IsMaster = am_I_Master(node()),
    join_cluster(ClusterName, IsMaster).

join_cluster(ClusterName, no) ->
    case mnesia:change_config(extra_db_nodes, [ClusterName]) of
        {ok, [_]} -> % connected to cluster, lets read info
            
            % make a copy of the Master MnesiaTable
            {atomic, ok} = mnesia:add_table_copy(ldb_nodes, node(), ram_copies),
            {atomic, ok} = mnesia:add_table_copy(node_info, node(), ram_copies),

            % it is a network, wait for 10 secs
            ok = mnesia:wait_for_tables([ldb_nodes, node_info], 10000),

            case mnesia:dirty_read(ldb_nodes, "ldbNodes") of
                [{ldb_nodes, _, MasterNode, SlaveNodes, ObserverNodes}] ->
                    ok = mnesia:dirty_write(#ldb_nodes{clusterID="ldbNodes", master_node=MasterNode, slave_nodes=sets:to_list(sets:from_list(SlaveNodes ++ [node()])), observer_nodes=ObserverNodes}),

                    {connected, SlaveNodes};
                Error ->
                    {error, {mnesia_error, Error}}
            end;
        {ok, []} ->
            {error, {not_connected, {}}};
        Error ->
            Error
    end;

%
% Join cluster as observer
%
join_cluster(ClusterName, observer) ->
    join_cluster(ClusterName),
    make_Observer(node());
join_cluster(_ClusterName, yes) -> {error, {already_master, {}}}.

leave_cluster() ->
    IsMaster = am_I_Master(node()),
    case mnesia:dirty_read(ldb_nodes, "ldbNodes") of
        _ when IsMaster =:= yes ->
            {error, {not_permitted, {}}};

        [Data] -> 
            {ldb_nodes, _, MasterNode, Slaves, Observers} = Data,
            ok = mnesia:dirty_write(#ldb_nodes{clusterID="ldbNodes", master_node=MasterNode, slave_nodes=Slaves -- [node()], observer_nodes=Observers -- [node()]}),

            {ok, disconnected_from_cluster};
        Error ->
            {error, {not_connected, Error}}
    end.

who_is_master() ->
    case mnesia:dirty_read(ldb_nodes, "ldbNodes") of
        [{ldb_nodes, "ldbNodes", MasterNode, _, _}] -> 
            {ok, MasterNode};
        Error ->
            {error, {not_connected, Error}}
    end.

am_I_Master(NodeName) ->
    case who_is_master() of
        {ok, NodeName} -> yes;
        {ok, _} -> no;
        Error ->
            {error, {not_connected, Error}}
    end.
    
% Only slaves can become Masters, not observers
make_Master(NodeName) ->
    IsMaster = am_I_Master(NodeName),
    case mnesia:dirty_read(ldb_nodes, "ldbNodes") of
        _ when IsMaster =:= yes ->
            {error, {already_master, {}}};

        %% node is the first master
        [] ->
            ok = mnesia:dirty_write(#ldb_nodes{clusterID="ldbNodes", master_node=NodeName, slave_nodes=[], observer_nodes=[]}),
            {ok, i_am_master};

        %% if node is a slave
        %% remove him from there!
        [{ldb_nodes, _, MasterNode, SlaveNodes, ObserverNodes}] ->
            NodeIsSlave = am_I_Slave(NodeName),
            if NodeIsSlave =:= yes  ->
                    ok = mnesia:dirty_write(#ldb_nodes{clusterID="ldbNodes", master_node=NodeName, 
                                                  slave_nodes= (SlaveNodes -- [NodeName]) ++ [MasterNode],
                                                   observer_nodes=ObserverNodes}),
                    {ok, i_am_master};
               true ->
                    {error, {not_slave, {}}}
            end;
        Error ->
            {error, {not_connected, Error}}
    end.


am_I_Slave(NodeName) ->
    case mnesia:dirty_read(ldb_nodes, "ldbNodes") of
        %% node hasn't joined a cluster 
        [] ->
            {error, {not_connected, {}}};

        [{ldb_nodes, _, _, SlaveNodes, _}] ->
            NodeIsSlave = (SlaveNodes =/= ([NodeName] -- SlaveNodes)),
            if NodeIsSlave =:= true  -> yes;
               true -> no
            end;
        Error ->
            {error, {not_connected, Error}}
    end.

make_Observer(NodeName) ->
    IsMaster = am_I_Master(NodeName),
    case mnesia:dirty_read(ldb_nodes, "ldbNodes") of
        _ when IsMaster =:= yes ->
            {error, {is_master, {}}};

        %% node hasn't joined a cluster 
        [] -> 
            {error, {not_connected, {}}};

        %% if node is a slave
        %% remove him from there
        %% and make him an observer
        [{ldb_nodes, _, MasterNode, SlaveNodes, ObserverNodes}] ->
            NodeIsSlave = am_I_Slave(NodeName),
            if NodeIsSlave =:= yes  ->
                    ok = mnesia:dirty_write(#ldb_nodes{clusterID="ldbNodes", master_node=MasterNode,
                                                  slave_nodes=(SlaveNodes -- [NodeName]) ,
                                                  observer_nodes=ObserverNodes ++ [NodeName]}),
                                                  {ok, i_am_observer};
               true ->
                    ok = mnesia:dirty_write(#ldb_nodes{clusterID="ldbNodes", master_node=MasterNode,
                                                  slave_nodes=(SlaveNodes) ,
                                                  observer_nodes=sets:to_list(sets:from_list(ObserverNodes ++ [NodeName]))}),
                    {ok, i_am_observer}
            end;
        Error ->
            {error, {not_connected, Error}}
    end.

replicate() ->
    ok = ?CONSOLE("--> [Replication Started] ~n", []),

    % replication
    {ok, Master} = who_is_master(),

    NodeStatus = check_if_alive(Master),
    case NodeStatus of 
        true ->
            % replicate
            ok;
        false -> % elect a new Master
            refresh(),
            replicate()
    end,

    ok = ?CONSOLE("--> [Replication Finished] ~n", []),
    % Updating replication stamp
    ok = mnesia:dirty_write(#node_info{node_name=node(), replication_time=element(2, ?now())}).

% Test if everything is working
% start a node alpha@localhost.localdomain
% then > replication:test_slave(alpha@localhost.localdomain).
% make sure to update your /etc/hosts
% 127.0.0.1    localhost.localdomain localhost

test_slave(MasterNodeName) ->
    [] = os:cmd("epmd -daemon"),
    % start one node as slave
    {ok, ready_to_join} = replication:init('beta@localhost.localdomain', 'hello'),

    % join cluster
    {connected,[]} = replication:join_cluster(MasterNodeName),

    % check nodes
    {ready,[Master2,[Beta2],[]]} = replication:refresh(),
    Master2 = MasterNodeName,
    Beta2 = node(),
    
    yes = replication:am_I_Slave(node()),

    no = replication:am_I_Master(node()),

    {ok, i_am_master} = replication:make_Master(node()),

    {ready,[Master3,[Beta3],[]]} = replication:refresh(),
    Master3 = node(),
    Beta3 = MasterNodeName,

    {error, {is_master, {}}} = replication:make_Observer(node()),

    % make someone else Master
    {ok, i_am_master} = replication:make_Master(MasterNodeName),
    {ready,[Master2,[Beta2],[]]} = replication:refresh(),

    {ok, i_am_observer} = replication:make_Observer(node()),
    {ready,[Master2,[],[Beta2]]} = replication:refresh(),
    
    {ok, disconnected_from_cluster} = replication:stop(normal),
    
    ?CONSOLE("TEST SUCCESSFUL ~n", []).

    
