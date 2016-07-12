
%%==================================================
%%群订阅push消息处理
%%==================================================

-module(mod_muc_subscribe).

-behaviour(gen_mod).

-define(PROCNAME, mod_muc_subscribe).
-define(SERVER, ?MODULE).

-include("ejabberd.hrl").
-include("logger.hrl").
-export([start/2,stop/1]).
-export([start_link/1,init/1]).

start(Host, Opts)->
    Proc = get_proc_name(Host),
    ChildSpec =  {Proc,{?MODULE, start_link,[Opts]}, permanent, infinity,supervisor,[Proc]},
    {ok, _Pid} = supervisor:start_child(ejabberd_sup, ChildSpec).

start_link(Opts) ->
    case supervisor:start_link({local, ?SERVER}, ?MODULE, [Opts]) of
	{ok, Pid} ->
	        {ok, Pid};	
   	{error, Reason} ->
	        ?DEBUG(" supervisor ~p  start error ~p ",[?MODULE,Reason])
     end.

init([Opts]) ->
	Subscribe_msg_app  = {subscribe_msg_sup, {subscribe_msg_sup,start_link, [Opts]}, permanent, brutal_kill, supervisor, [subscribe_msg_sup]},
    {ok, {{one_for_one, 10, 1},[Subscribe_msg_app]}}.

stop(Host) ->
    Proc = get_proc_name(Host),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc),
    ok.

get_proc_name(Host) ->
    gen_mod:get_module_proc(Host, ?PROCNAME).

