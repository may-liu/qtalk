-module(mod_http_client).

-behaviour(gen_mod).

-define(PROCNAME, mod_http_client).
-define(SERVER, ?MODULE).

-include("ejabberd.hrl").
-include("logger.hrl").
-export([start/2,stop/1]).
-export([start_link/2,init/1]).
-export([add_pid/2, remove_pid/2,get_pids/1, get_random_pid/1]).

-define(DEFAULT_POOL_SIZE, 65).

start(Host, Opts)->
	ets:new(http_client_pid, [named_table, bag, public]),
    ChildSpec =  {?MODULE,{?MODULE, start_link,[Host,Opts]}, permanent, infinity,supervisor,[?MODULE]},
    {ok, _Pid} = supervisor:start_child(ejabberd_sup, ChildSpec).

start_link(Host,Opts) ->
    case supervisor:start_link({local, ?SERVER}, ?MODULE, [Host,Opts]) of
	{ok, Pid} ->
	        {ok, Pid};	
   	{error, Reason} ->
	        ?INFO_MSG("Supervisor ~p start error,reason: ~p . ~n",[?MODULE,Reason])
     end.

init([Host,Opts]) ->
	PoolSize = gen_mod:get_opt(pool_size, Opts, fun(A) -> A end, ?DEFAULT_POOL_SIZE),
    {ok,
     {{one_for_one, ?DEFAULT_POOL_SIZE * 10, 1},
		lists:map(fun (I) ->
		           {I,
		     {http_client, start_link,
		           [Host, Opts ++ [{<<"seq">>,I}]]},
		           transient, 2000, worker, [?MODULE]}
					          end,
		    lists:seq(1, PoolSize))}}.

get_pids(Host) ->
    case ets:lookup(http_client_pid,Host) of
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
    [] -> 
		none;
    Pids -> 
		lists:nth(erlang:phash(os:timestamp(), length(Pids)), Pids)
    end.

add_pid(Host,Pid) ->
    ets:insert(http_client_pid,{Host,Pid}).

remove_pid(Host,Pid) ->
    ets:delete_object(http_client_pid,{Host,Pid}).

stop_child() ->
    lists:foreach(fun({_,Pid}) ->
        gen_server:cast(Pid, stop) end,ets:tab2list(http_client_pid)).

stop(Host) ->
    supervisor:terminate_child(ejabberd_sup, ?MODULE),
    supervisor:delete_child(ejabberd_sup, ?MODULE),
    stop_child(),
	catch ets:delete(http_client_pid).


