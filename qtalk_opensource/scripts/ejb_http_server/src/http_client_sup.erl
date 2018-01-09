-module(http_client_sup).

%% Callback Functions
-export([init/1]).

%% API
-export([start_link/1,
        add_pid/1,
        remove_pid/1,
        get_pids/0,
        get_random_pid/0]).

-define(SERVER, ?MODULE).
-define(DEFAULT_POOL_SIZE, 10).

start_link(Opts) ->
    ets:new(http_client_pid, [named_table, bag, public]),
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Opts]).

init([Opts]) ->
    PoolSize = proplists:get_value(poolsize, Opts, ?DEFAULT_POOL_SIZE),
    {ok,
     {{one_for_one, PoolSize * 10, 1},
		lists:map(fun (I) ->
		           {I,
		     {http_client, start_link,
		           [[{<<"seq">>,I}|Opts]]},
		           transient, 2000, worker, [http_client]}
					          end,
		    lists:seq(1, PoolSize))}}.

get_pids() ->
    case ets:tab2list(http_client_pid) of
    [] ->
		[];
    Rs when is_list(Rs) ->
		 lists:map(fun({Pid}) -> 
				 Pid
		  end,Rs);
    _ ->
		[]
    end.

get_random_pid() ->
    case get_pids() of
      [] -> undefined;
      Pids -> lists:nth(erlang:phash(os:timestamp(), length(Pids)), Pids)
    end.

add_pid(Pid) ->
      ets:insert(http_client_pid, {Pid}).

remove_pid(Pid) ->
      ets:delete_object(http_client_pid, {Pid}).
