
%%==========================================================
%%		异步处理sql模块
%%用于处理部分易于阻塞的sql
%%==========================================================


-module(mod_async_sql).

-behaviour(gen_mod).

-define(PROCNAME, mod_async_sql).
-define(SERVER, ?MODULE).

-define(DEFAULT_POOL_SIZE, 35).

-include("ejabberd.hrl").
-include("logger.hrl").
-export([start/2,stop/1]).
-export([start_link/2,init/1]).
-export([add_pid/2, remove_pid/2, get_pids/1, get_random_pid/1]).

start(Host, Opts)->
    Proc = get_proc_name(Host),
	catch ets:new(async_sql_pid, [named_table, bag, public]),
    ChildSpec =  {Proc,{?MODULE, start_link,[Host,Opts]}, permanent, infinity,supervisor,[Proc]},
    {ok, _Pid} = supervisor:start_child(ejabberd_sup, ChildSpec).

start_link(Host,Opts) ->
    case supervisor:start_link({local, ?SERVER}, ?MODULE, [Host,Opts]) of
	{ok, Pid} ->
	        {ok, Pid};	
   	{error, Reason} ->
	        ?INFO_MSG("Supervisor ~p start error ,reason ~p. ~n",[?MODULE,Reason])
     end.

init([Host,Opts]) ->
	PoolSize = gen_mod:get_opt(pool_size, Opts, fun(A) -> A end, ?DEFAULT_POOL_SIZE),
    {ok,
     {{one_for_one, ?DEFAULT_POOL_SIZE * 10, 1},
		lists:map(fun (I) ->
		           {I,
		     {async_sql, start_link,
		           [Host, 20 * 1000]},
		           transient, 2000, worker, [?MODULE]}
					          end,
		    lists:seq(1, PoolSize))}}.

get_pids(Host) ->
    case ets:lookup(async_sql_pid,Host) of
    [] ->
		[];
    Rs when is_list(Rs) ->
		 lists:flatmap(fun(B) -> 
				 [element(2,B)]
		  end,Rs);
    _ ->
		[]
    end.

get_random_pid(Host) ->
    case get_pids(Host) of
      [] -> none;
      Pids -> lists:nth(erlang:phash(os:timestamp(), length(Pids)), Pids)
    end.

add_pid(Host,Pid) ->
      ets:insert(async_sql_pid,{Host,Pid}).

remove_pid(Host,Pid) ->
      ets:delete_object(async_sql_pid,{Host,Pid}).


stop(Host) ->
    Proc = get_proc_name(Host),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc),
    ok.

get_proc_name(Host) ->
    gen_mod:get_module_proc(Host, ?PROCNAME).
