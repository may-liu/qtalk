-module(ejabberd_ldap_sup).

%% API
-export([start_link/1, init/1]).

-export([add_pid/1, remove_pid/1,
     get_pids/0, get_random_pid/0]).

-export([
		 login/2,
		 get_dep/0]).

-define(POOLSIZE, 5).

start_link(Option) ->
    ets:new(ldap_server_pid, [named_table, bag, public]),
    supervisor:start_link({local,?MODULE}, ?MODULE, [Option]).


init([Option]) ->
	PoolSize = proplists:get_value("poolsize", Option, ?POOLSIZE),
    {ok,
     {{one_for_one, 1000, 1}, lists:map(fun(I) ->
        {I, 
            {ejabberd_ldap_server, start_link, [Option]},
            transient,
            2000,
            worker,
            [?MODULE]} end, lists:seq(1, PoolSize))
	}}.

get_pids() ->
    case ets:tab2list(ldap_server_pid) of
    [] ->
        []; 
    Pids when is_list(Pids) ->
		Pids;
    _ ->
        []
    end.

get_random_pid() ->
    case get_pids() of
      [] -> undefined;
      Pids -> {Pid} = lists:nth(erlang:phash(os:timestamp(), length(Pids)), Pids), Pid
    end.

add_pid(Pid) ->
      ets:insert(ldap_server_pid,{Pid}).

remove_pid(Pid) ->
      ets:delete_object(ldap_server_pid,{Pid}).

login(User, Passwd) ->
	case get_random_pid() of
		undefined -> error;
		Pid ->
			ejabberd_ldap_server:login(Pid, User, Passwd)
	end.

%% ejabberd_ldap_sup:get_dep().
get_dep() ->
	case get_random_pid() of
		undefined -> [];
		Pid ->
			ejabberd_ldap_server:get_dep(Pid)
	end.
