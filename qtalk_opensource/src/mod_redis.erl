
%========================================
%		Redis处理模块
%========================================
-module(mod_redis).

-behaviour(gen_mod).

-define(PROCNAME, mod_redis).
-define(SERVER, ?MODULE).

-include("ejabberd.hrl").
-include("logger.hrl").
-export([start/2,stop/1]).
-export([start_link/2,init/1]).
-export([add_pid/3, remove_pid/3, get_pids/2, get_random_pid/2]).

-define(DEFAULT_POOL_SIZE, 20).

-define(CONNECT_TIMEOUT, 500).

start(Host, Opts)->
    Proc = get_proc_name(Host),
	catch ets:new(redis_pid, [named_table, bag, public]),
	catch ets:new(redis_start_flag, [named_table, set, public]),
	start_redis_link(Host,Proc,Opts,mod_time:get_timestamp()).

start_redis_link(Host,Proc,Opts,Time) ->
    ChildSpec =  {Proc,{?MODULE, start_link,[Host,Opts]}, permanent, infinity,supervisor,[Proc]},
	catch ets:insert(redis_start_flag,{redis_start_flag,Time}),
	supervisor:start_child(ejabberd_sup, ChildSpec).


start_link(Host,Opts) ->
    case supervisor:start_link({local, ?SERVER}, ?MODULE, [Host,Opts]) of
	{ok, Pid} ->
	        {ok, Pid};	
   	{error, Reason} ->
	        ?DEBUG(" supervisor start error ~p ",[Reason])
     end.

init([Host,Opts]) ->
	StartMode = gen_mod:get_opt(redis_start_mode, Opts, fun(A) -> A end, 1),
	PoolSize = gen_mod:get_opt(pool_size, Opts, fun(A) -> A end, ?DEFAULT_POOL_SIZE),
	Redis_Tabs = string:tokens(binary_to_list(gen_mod:get_opt(redis_tab, Opts, fun(A) -> A end, <<"0,1,2,3">>)),","),
	Tabs = lists:flatmap(fun(T) ->
				case catch list_to_integer(T) of
				I when is_integer(I) ->
					[I];
				_ ->
					[]
				end end,Redis_Tabs),
    {ok,
     {{one_for_one, ?DEFAULT_POOL_SIZE * 10, 1},
		lists:flatmap(fun (I) -> lists:map(fun(Tab) ->
			{I*100+Tab,
			 {redis_link, start_link,
			  [Host, Tab,Opts]},
			 transient, 2000, worker, [?MODULE]}
		end,Tabs) end,
		lists:seq(1, PoolSize))}}.

get_pids(Host,Tab) ->
    case ets:lookup(redis_pid,Host) of
    [] ->
		case whereis(?MODULE) of
		undefined ->
			restart_redis(Host),
			[];
		_ ->
			[]
		end;
    Rs when is_list(Rs) ->
		 lists:flatmap(fun(B) -> 
				 case element(2,B) of 
				 Tab -> [element(3,B)];
				 _ -> [] 
				 end 
		  end,Rs);
    _ ->
		[]
    end.

get_random_pid(Host,Tab) ->
    case get_pids(Host,Tab) of
      [] -> none;
      Pids -> lists:nth(erlang:phash(os:timestamp(), length(Pids)), Pids)
    end.

add_pid(Host,Tab, Pid) ->
      ets:insert(redis_pid,{Host,Tab, Pid}).

remove_pid(Host,Tab, Pid) ->
      ets:delete_object(redis_pid,{Host,Tab,Pid}).

stop(Host) ->
	catch ets:delete(redis_pid),
    Proc = get_proc_name(Host),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc),
    ok.

get_proc_name(Host) ->
    gen_mod:get_module_proc(Host, ?PROCNAME).

parse_sentinel_host(Configure) ->
	Sentinel_hosts = str:tokens(Configure,<<",">>),	
	lists:map(fun(Sentinel) ->
		[Host,Port] = str:tokens(Sentinel,<<":">>),
		{binary_to_list(Host),binary_to_integer(Port)} end,Sentinel_hosts).

restart_redis(Host) ->
	Now = mod_time:get_timestamp(),
	case ets:lookup(redis_start_flag,redis_start_flag) of
	[{_,Time}] ->
		if Now - Time > 10 ->
			gen_mod:start_module(Host, mod_redis);
		true ->
			ok
		end;
	_ ->
		gen_mod:start_module(Host, mod_redis)
	end.

