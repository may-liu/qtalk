-module(transfer_node_rpc).

-include("logger.hrl").

-export([call_ejabberd_node/4]). 

call_ejabberd_node(Server,Module,Function,Args) ->
	Hidden_nodes = nodes([hidden]),
	Ejabberd_nodes = 
		lists:filter(fun(Node) -> 
			str:str(list_to_binary(atom_to_list(Node)),<<"ejabberd">>) =/= 0 end,Hidden_nodes),
		?DEBUG("Ejabberd_nodes ~p ~n",[Ejabberd_nodes]),
	case Ejabberd_nodes of
	[] ->
		{false,<<"no found ejabberd node">>};
	_ ->
		Node = lists:nth(1,Ejabberd_nodes),
		case catch rpc:call(Node,'sync_ets_cache','send_sync_nodes_notcie',[Server,Module,Function,Args]) of
		{badrpc, Reason} ->
			?DEBUG("Reasion ~p ~n",[Reason]),
			{false,Reason};
		Res ->
			?DEBUG("Reasion ~p ~n",[Res]),
			{true,Res}
		end
	end.

	
