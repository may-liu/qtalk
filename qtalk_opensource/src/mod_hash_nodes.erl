-module(mod_hash_nodes).

-behaviour(gen_server).
-behaviour(gen_mod).

-export([start/2,stop/1]).
-export([start_link/2]).
-export([code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).
-export([register_hash_nodes/3,unregister_hash_nodes/2,send_mnesia_update/2,update_ets_user_hash/1]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").

-define(HASH_SIZE,33).
-define(PROCNAME,mod_hash_nodes).

-record(hash_nodes, {node, pid, num_range}).
-record(state, {server}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------

start(Host, Opts)->
    Proc = get_proc_name(Host),
    ChildSpec =  {Proc,{?MODULE, start_link,[Opts]}, permanent, infinity,supervisor,[Proc]},
    {ok, _Pid} = supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    Proc = get_proc_name(Host),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc),
    ok.

start_link(Server,_Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Server], []).

register_hash_nodes(Server,Node,Flag) ->
	Nodes = mnesia:dirty_all_keys(hash_nodes) ++ [Node],
	Length = length(Nodes),
	Hash_N	= lists:map(fun(N) ->
			{erlang:hash(N,?HASH_SIZE),N} end,Nodes),
	Sort_Hash = lists:keysort(1,Hash_N),
	New_Nodes = 
		lists:map(fun(I) ->
			if I =/= Length ->
				{H1,N1} = lists:nth(I,Sort_Hash),
				{H2,_ } = lists:nth(I+1,Sort_Hash),
				{N1,{H1,H2}};
			true ->
				{H0,_} = lists:nth(1,Sort_Hash),
				{H1,N1} = lists:nth(I,Sort_Hash),
				{N1,{H1,H0}}
			end end,lists:seq(1,Length)),
	F = fun () ->
		lists:foreach(fun({HN,Num_Range}) ->
			case mnesia:wread({hash_nodes, HN}) of
			[] ->
				mnesia:write(#hash_nodes{node = HN,pid = whereis(mod_hash_nodes),num_range = Num_Range});
			[HN1] ->
				mnesia:write(HN1#hash_nodes{num_range = Num_Range})
			end end,New_Nodes) end,
	mnesia:sync_transaction(F),
	case Flag of true ->
		catch send_mnesia_update(Server,0);
	_ ->
		ok
	end.

unregister_hash_nodes(Server,Dpid) ->
	[Node] = get_node_by_pid(Dpid),
	[Del_info] = mnesia:dirty_read({hash_nodes,Node}),
	mnesia:dirty_delete_object(Del_info),
	Nodes = mnesia:dirty_all_keys(hash_nodes) -- [Node],
	Length = length(Nodes),
	Hash_N	= lists:map(fun(N) ->
			{erlang:hash(N,?HASH_SIZE),N} end,Nodes),
	Sort_Hash = lists:keysort(1,Hash_N),
	New_Nodes = 
		lists:map(fun(I) ->
			if I =/= Length ->
				{H1,N1} = lists:nth(I,Sort_Hash),
				{H2,_ } = lists:nth(I+1,Sort_Hash),
				{N1,{H1,H2}};
			true ->
				{H0,_} = lists:nth(1,Sort_Hash),
				{H1,N1} = lists:nth(I,Sort_Hash),
				{N1,{H1,H0}}
			end end,lists:seq(1,Length)),
	
	%%timer:sleep(1*1000),
	lists:foreach(fun({HN,Num_Range}) ->
		case mnesia:dirty_read({hash_nodes, HN}) of
		[] ->
		%%	mnesia:write(#hash_nodes{node = HN,pid = whereis(mod_hash_nodes),num_range = Num_Range});
			ok;
		[HN1] ->
			mnesia:dirty_write(HN1#hash_nodes{num_range = Num_Range})
		end end,New_Nodes),
	  catch spawn(?MODULE, send_mnesia_update, [Server,10]).

get_node_by_pid(Pid ) ->
	 mnesia:dirty_select(hash_nodes,[{{hash_nodes, '$1', Pid, '_'}, [], ['$1']}]).
			
dirty_get_all_domains() ->
    lists:usort(mnesia:dirty_all_keys(hash_nodes)).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Server]) ->
    update_tables(),
    mnesia:create_table(hash_nodes,
			[{ram_copies, [node()]}, {type, set},
			 {attributes, record_info(fields, hash_nodes)}]),
    mnesia:add_table_copy(hash_nodes, node(), ram_copies),
    mnesia:subscribe({table, hash_nodes, simple}),
	register_hash_nodes(Server,node(),false),
	L =	mnesia:dirty_select(hash_nodes,
				      [{{hash_nodes, '_', '$1', '_'}, [], ['$1']}]),
    lists:foreach(fun (Pid) -> 
			erlang:monitor(process, Pid),
			case Pid == self() of
			true ->
				ok;
			_ ->
				catch gen_server:cast(Pid,update_process)
			end	end,L),
	catch spawn(?MODULE, send_mnesia_update, [Server,10]),
    {ok, #state{server = Server}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(get_user_hash_pid , _From, State) ->
    {reply,whereis(mod_hash_user),State};
handle_call(_Request, _From, State) ->
    Reply = ok, {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
handle_cast(update_hash_info, #state{server = Server} = State) ->
	mod_hash_user:update_handle_info(Server),
	{noreply, State};
handle_cast(update_process, #state{server = Server} = State) ->
	L =	mnesia:dirty_select(hash_nodes,
				      [{{hash_nodes, '_', '$1', '_'}, [], ['$1']}]),
  	lists:foreach(fun (Pid) ->
              erlang:monitor(process, Pid) end,L),
	{noreply, State};
handle_cast(_Msg, State) -> {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({'DOWN', _Ref, _Type, Pid, _Info}, #state{server = Server} = State) ->
	unregister_hash_nodes(Server,Pid),	
    {noreply, State};
handle_info(unregister_hash_nodes, #state{server = Server} = State) ->
	unregister_hash_nodes(Server,node()),	
    {noreply, State};
%handle_info({mnesia_table_event,{delete,{hash_nodes,Node},_ }}, #state{server = Server} = State) ->
%	unregister_hash_nodes(Server,Node),	
 %   {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

update_tables() ->
    case catch mnesia:table_info(hash_nodes, attributes) of
      [node, pid] -> mnesia:delete_table(route);
      [pid,num_range] -> mnesia:delete_table(route);
      [node, pid, num_range] -> ok;
      [node, num_range] -> mnesia:delete_table(route);
      {'EXIT', _} -> ok
    end.

get_proc_name(Host) ->
    gen_mod:get_module_proc(Host, ?PROCNAME).

send_mnesia_update(Server,Time) ->
	timer:sleep(Time*1000),
	L = mnesia:dirty_select(hash_nodes, [{{hash_nodes, '_', '$1', '$2'}, [], [['$1','$2']]}]),
	lists:foreach(fun([Hash_Pid,_]) ->
			case catch gen_server:call(Hash_Pid,get_user_hash_pid) of
			Pid when is_pid(Pid) ->
				catch gen_server:cast(Pid,update_hash_info);
			 Error ->
			 	?INFO_MSG("update_ets_user_hash reason ~p ~n",[Error])
			end	 end,L).

update_ets_user_hash(Server) ->
	catch ets:delete_all_objects(hash_user_pid),
	L = mnesia:dirty_select(hash_nodes, [{{hash_nodes, '_', '$1', '$2'}, [], [['$1','$2']]}]),
    lists:foreach(fun ([Hash_Pid,Num_Range]) ->
		case catch gen_server:call(Hash_Pid,get_user_hash_pid,10000) of
		Pid when is_pid(Pid) ->
			ets:insert(hash_user_pid,{Pid,Num_Range});
		{'EXIT',{{nodedown,_},_}} ->
			unregister_hash_nodes(Server,Hash_Pid);		
		Error ->
			?INFO_MSG("update_ets_user_hash reason ~p ~n",[Error])
		end end,L).

