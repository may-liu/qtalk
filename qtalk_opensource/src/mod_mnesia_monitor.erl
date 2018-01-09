-module(mod_mnesia_monitor).

-behaviour(gen_server).
-behaviour(gen_mod).

-export([start/2,stop/1,start_link/2,init/1]).

-export([handle_call/3, handle_cast/2,
 	    handle_info/2, terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("logger.hrl").

-define(SERVER, ?MODULE).

-record(state,{}).

start(Host,Opts) ->
	Proc = get_proc_name(Host),
	ChildSpec = {Proc,{?MODULE, start_link, [Host,Opts]}, temporary,1000,worker,[?MODULE]},
	{ok,_Pid} = supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
	Proc = get_proc_name(Host),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).

start_link(Server,Opts) ->
	gen_server:start_link({local, get_proc_name(Server)}, ?MODULE, [Server,Opts], []).

init([_Server,_Opts]) ->
	{ok, _} = mnesia:subscribe(system),
	{ok, #state{}}.


handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({mnesia_system_event,
		{inconsistent_database, running_partitioned_network, Node}},State) ->
	catch mnesia:change_config(extra_db_nodes,['ejabberd@l-ejab1.vc.cn5']),
	{noreply, State};
handle_info({mnesia_system_event,
		{inconsistent_database, starting_partitioned_network, Node}},State) ->
	catch mnesia:change_config(extra_db_nodes,['ejabberd@l-ejab1.vc.cn5']),
	{noreply, State};
handle_info({mnesia_system_event,
		Info},State) ->
	?ERROR_MSG("catch mnesia_system_event ~p ~n",[Info]),
	{noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_Reason, _State) ->
    ok.

get_proc_name(Host) ->
    gen_mod:get_module_proc(Host, ?MODULE).
	
