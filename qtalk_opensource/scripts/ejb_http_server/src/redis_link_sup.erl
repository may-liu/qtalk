-module(redis_link_sup).

-include("logger.hrl").

-author('liufannana@sina.com').

%% Callback Functions
-export([
         start_link/1,
         init/1
        ]).

%% APIs
-export([
         add_pid/2,
         remove_pid/2,
         get_pids/1,
         get_random_pid/1
        ]).

-define(SERVER, ?MODULE).
-define(DEFAULT_POOL_SIZE, 40).
-define(DEFAULT_TABLES, []).

start_link(Opts) ->
    ets:new(redis_pid, [named_table, bag, public]),
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Opts]).

init([Opts]) ->
    StartMode = proplists:get_value(start_mode, Opts, ?DEFAULT_TABLES),
    PoolSize = proplists:get_value(pool_size, Opts, ?DEFAULT_POOL_SIZE),
    RedisTabs = proplists:get_value(tables, Opts, ?DEFAULT_TABLES),
	case StartMode of
	1 ->
	    Redis_sentinel_hosts = parse_sentinel_host(proplists:get_value(sentinel_hosts, Opts, "")),
		eredis_sentinel:start_link(Redis_sentinel_hosts);
	_ ->
		ok
	end,

	ChildSpec =	lists:flatmap(fun (I) ->
                    lists:map(fun(Tab) ->
                                {I*100+Tab,
                                    {redis_link, start_link,
                                        [list_to_atom(lists:flatten(io_lib:format("~p:~p", [I, Tab]))),Tab,Opts]},
                                    transient, 2000, worker, [redis_link]}
                        end,RedisTabs)
            end,
            lists:seq(1, PoolSize)),
    {ok,
        {{one_for_one, PoolSize * 10, 1}, ChildSpec}}.

get_pids(Table) ->
    case ets:tab2list(redis_pid) of
    [] ->
		[];
    Rs when is_list(Rs) ->
		 lists:foldl(fun({Tab, Pid}, Acc) when Tab =:= Table->
                        [Pid|Acc];
                     (_, Acc) ->
                        Acc 
                     end, [], Rs);
    _ ->
		[]
    end.

get_random_pid(Table) ->
    case get_pids(Table) of
      [] -> undefined;
      Pids -> lists:nth(erlang:phash(os:timestamp(), length(Pids)), Pids)
    end.

add_pid(Table, Pid) ->
      ets:insert(redis_pid,{Table, Pid}).

remove_pid(Table, Pid) ->
      ets:delete_object(redis_pid,{Table, Pid}).

parse_sentinel_host(Configure) ->
	  lists:map(fun(Sentinel) ->
		  [Host,Port] = string:tokens(Sentinel,":"),
		  {Host,list_to_integer(Port)} end,Configure).
