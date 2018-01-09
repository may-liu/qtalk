-module(mod_hash).

-behaviour(gen_mod).
-behaviour(supervisor).

-export([start/2,stop/1]).
-export([start_link/2,init/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

start(Host, Opts)->
    Proc = get_proc_name(Host),
	catch ets:new(hash_user_pid,[set, named_table, public, {keypos, 1},{write_concurrency, true}, {read_concurrency, true}]),
    catch ets:new(friend_opts,  [set, named_table, public, {keypos, 2},{write_concurrency, true}, {read_concurrency, true}]),
    catch ets:new(user_friends, [set, named_table, public, {keypos, 2},{write_concurrency, true}, {read_concurrency, true}]),
    catch ets:new(recv_msg_limit,  [set, named_table, public, {keypos, 2},{write_concurrency, true}, {read_concurrency, true}]),
    ChildSpec =  {Proc,{?MODULE, start_link,[Host,Opts]}, permanent, infinity,supervisor,[Proc]},
    {ok, _Pid} = supervisor:start_child(ejabberd_sup, ChildSpec).
%%	catch mod_hash_nodes:send_mnesia_update(Host,10)

stop(Host) ->
    Proc = get_proc_name(Host),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc),
    ok.

start_link(Host,Opts) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Host,Opts]).

init([Host,Opts]) ->
	Hash_nodes = {mod_hash_nodes,{mod_hash_nodes,start_link,[Host,Opts]},permanent, infinity,worker,[mod_hash_nodes]},
	Hash_users = {mod_hash_user, {mod_hash_user,start_link,[Host,Opts]},permanent, infinity,worker,[mod_hash_user]},

	{ok, {{rest_for_one, 5, 10}, [Hash_nodes,Hash_users]}}.

get_proc_name(Host) ->
    gen_mod:get_module_proc(Host, ?MODULE).
