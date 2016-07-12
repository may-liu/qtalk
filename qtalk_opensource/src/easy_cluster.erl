-module(easy_cluster).

-export([test_node/1,join/1,join_as_master/1,sync_node/1]).

test_node(NodeName) ->
        case net_adm:ping(NodeName) of 'pong' ->
                io:format("server is reachable.~n");
        _ ->
                io:format("server could NOT be reached.~n")
        end.

join(NodeName) ->
        mnesia:stop(),
        mnesia:delete_schema([node()]),
        mnesia:start(),
        mnesia:change_config(extra_db_nodes, [NodeName]),
        mnesia:change_table_copy_type(schema, node(), disc_copies),
        application:stop(ejabberd),
        application:start(ejabberd).

join_as_master(NodeName) ->
        application:stop(ejabberd),
        mnesia:stop(),
        mnesia:delete_schema([node()]),
        mnesia:start(),
        mnesia:change_config(extra_db_nodes, [NodeName]),
        mnesia:change_table_copy_type(schema, node(), disc_copies),
        easy_cluster:sync_node(NodeName),
        application:start(ejabberd).

sync_node(NodeName) ->
        [{Tb, mnesia:add_table_copy(Tb, node(), Type)} 
        || {Tb, [{NodeName, Type}]} <- [{T, mnesia:table_info(T, where_to_commit)}
        || T <- mnesia:system_info(tables)]].
