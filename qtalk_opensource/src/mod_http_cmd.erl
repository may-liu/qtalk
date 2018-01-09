-module(mod_http_cmd).

-behaviour(gen_mod).

-define(PROCNAME, mod_http_cmd).
-define(SERVER, ?MODULE).

-include("ejabberd.hrl").
-include("logger.hrl").
-export([start/2,stop/1]).
-export([start_link/1,init/1]).

start(Host, Opts)->
%% rpc 调用频繁失败时调用，不建议打开改模块,该接口不宜暴露
    Proc = get_proc_name(Host),
    ChildSpec =  {Proc,{?MODULE, start_link,[Opts]}, permanent, infinity,supervisor,[Proc]},
    {ok, _Pid} = supervisor:start_child(ejabberd_sup, ChildSpec).

start_link(Opts) ->
    case supervisor:start_link({local, ?SERVER}, ?MODULE, [Opts]) of
	{ok, Pid} ->
	        {ok, Pid};	
   	{error, Reason} ->
	        ?DEBUG(" supervisor start error ~p ",[Reason])
     end.

init([Opts]) ->
	Http_get   = {mod_http_get, {mod_http_get,start_link, [Opts]}, permanent, brutal_kill, worker, [mod_http_get]},
    {ok, {{one_for_one, 10, 1},[Http_get]}}.

stop(Host) ->
    Proc = get_proc_name(Host),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc),
    ok.

get_proc_name(Host) ->
    gen_mod:get_module_proc(Host, ?PROCNAME).

