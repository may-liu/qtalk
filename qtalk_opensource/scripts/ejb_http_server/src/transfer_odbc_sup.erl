%%%----------------------------------------------------------------------
%%% File    : transfer_odbc_sup.erl
%%%----------------------------------------------------------------------

-module(transfer_odbc_sup).

%% API
-export([start_link/1, init/1]).

-export([add_pid/2, remove_pid/2,get_pids/1, get_random_pid/1]).

-define(DEFAULT_POOL_SIZE, 10).

start_link(Options) ->
    ets:new(transfer_odbc_pid, [named_table, bag, public]),
    supervisor:start_link({local,?MODULE}, ?MODULE, [Options]).

init([Options]) ->
    PoolSize = proplists:get_value(pool_size, Options, ?DEFAULT_POOL_SIZE),
    {ok,
     {{one_for_one, PoolSize * 10, 1},
      lists:map(fun (I) ->
			{I,
			 {transfer_odbc, start_link,
			  [Options]},
			 transient, 2000, worker, [?MODULE]}
		end,
		lists:seq(1, PoolSize))}}.

get_pids(Host) ->
    case ets:tab2list(transfer_odbc_pid) of
    [] ->
		[];
    Rs when is_list(Rs) ->
		 lists:foldl(fun({H, Pid}, Acc) when H =:= Host ->
                        [Pid|Acc];
                     (_, Acc) ->
                        Acc 
                     end, [], Rs);
    _ ->
		[]
    end.

get_random_pid(Host) ->
    case get_pids(Host) of
      [] -> undefined;
      Pids -> lists:nth(erlang:phash(os:timestamp(), length(Pids)), Pids)
    end.

add_pid(Host, Pid) ->
      ets:insert(transfer_odbc_pid,{Host, Pid}).

remove_pid(Host, Pid) ->
      ets:delete_object(transfer_odbc_pid,{Host, Pid}).

