
%%===================================================
%%			http请求模块
%%	使用cowboy 提供http服务
%%===================================================

-module(mod_http).

-behaviour(gen_mod).

-define(PROCNAME, mod_http).

-include("ejabberd.hrl").
-include("logger.hrl").

-export([start/2,stop/1]).
-export([start_link/2,init/1]).

start(Host, Opts)->
    Proc = get_proc_name(Host),
    ChildSpec =  {Proc,{?MODULE, start_link,[Host,Opts]}, permanent, infinity,supervisor,[Proc]},
    {ok, _Pid} = supervisor:start_child(ejabberd_sup, ChildSpec).

start_link(_Host,Opts) ->
    case supervisor:start_link({local, ?MODULE}, ?MODULE, [Opts]) of
	{ok, Pid} ->
	        {ok, Pid};	
   	{error, Reason} ->
	        ?DEBUG("mod_http start error ~p ",[Reason])
     end.

init([Opts]) ->
	Cowboy  = {http_cowboy, {http_cowboy,start_link, [Opts]}, permanent, brutal_kill, worker, [http_cowboy]},
    {ok, {{one_for_one, 10, 1},[Cowboy]}}.

stop(Host) ->
    Proc = get_proc_name(Host),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc),
    ok.

get_proc_name(Host) ->
    gen_mod:get_module_proc(Host, ?PROCNAME).

