-module(sync_ets_cache).

-export([send_sync_node_notcie/5,send_sync_nodes_notcie/4,send_async_node_notcie/5]).

-include("logger.hrl").


send_sync_nodes_notcie(Server,Module,Function,Args) ->
	Nodes = [node()] ++ nodes() ,
	lists:map(fun(Node) ->
		send_sync_node_notcie(Server,Node,Module,Function,Args) 
		end,Nodes).

send_sync_node_notcie(Server,Node,Module,Function,Args) ->
	case catch rpc:call(Node,Module,Function,Args) of 
	{badrpc, Reason} ->
			{Node,{error,Reason}};
		Res ->
		{Node,{sucess,Res}}
	end.



send_async_node_notcie(Server,Node,Module,Function,Args) ->
	case catch rpc:async_call(Node,Module,Function,Args) of 
	{badrpc, Reason} ->
			{Node,{error,Reason}};
		Res ->
		{Node,{sucess,Res}}
	end.
