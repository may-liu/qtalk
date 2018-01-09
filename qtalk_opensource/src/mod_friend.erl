-module(mod_friend).

-behaviour(gen_mod).
-behaviour(supervisor).

-export([start/2,stop/1]).
-export([start_link/2,init/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

-record(state,{server}).
-record(user_friends,{user,friends,unfriends}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------

start(Host, Opts)->
    Proc = get_proc_name(Host),
    ChildSpec =  {Proc,{?MODULE, start_link,[Host,Opts]}, permanent, infinity,supervisor,[Proc]},
    {ok, _Pid} = supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    Proc = get_proc_name(Host),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc),
    ok.

start_link(Host,Opts) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Host,Opts]).
%%    supervisor:start_link({local, ?MODULE}, ?MODULE, [Host,Opts], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------

init([Host,Opts]) ->
	Recv_msg_limit = {recv_msg_limit,{mod_recv_msg_limit,start_link,[Host,Opts]},permanent, infinity,supervisor,[recv_msg_limit]},
	User_Relationship = {mod_user_relation,{mod_user_relation,start_link,[Host,Opts]},permanent, infinity,supervisor,[mod_user_relation]},

	{ok, {{rest_for_one, 5, 10}, [Recv_msg_limit,User_Relationship]}}.

get_proc_name(Host) ->
    gen_mod:get_module_proc(Host, ?MODULE).
