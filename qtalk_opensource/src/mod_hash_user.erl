-module(mod_hash_user).

-behaviour(gen_server).
-behaviour(gen_mod).

-export([start/2,stop/1]).
-export([start_link/2]).
-export([code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).
-export([get_user_hash_pid/1,get_user_relation_info/3,check_user_friend_relationship/4]).
-export([add_user_friend/4,add_user_friend_opts/6,handle_del_friend/5,get_user_friends/3,get_ets_user_friends/3]).
-export([update_handle_info/1]).


-define(HASH_SIZE,33).
-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").

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
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Host,Opts], []).

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
    {ok, #state{server = Host}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(get_random_user_hash_pid, _From, #state{server = Server} = State) ->
    {reply,mod_user_relation:get_random_pid(Server),State};
handle_call(_Request, _From, State) ->
    Reply = ok, {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(update_hash_info, #state{server = Server} = State) ->
	?DEBUG("Update hash ~p ~n",[self()]),
	update_handle_info(Server),
	{noreply, State};
handle_cast(_Msg, State) -> {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------

handle_info(timeout,#state{server = Server} = State) ->
	{noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.
%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

get_proc_name(Host) ->
    gen_mod:get_module_proc(Host, ?MODULE).

get_user_relation_info(Server,User,Key) ->
	case catch get_user_hash_pid(User) of
	[{Pid,_ }] ->
		case gen_server:call(Pid,get_random_user_hash_pid)  of
		Rpid when is_pid(Rpid) ->
			gen_server:call(Rpid,{Key,User});
		_ ->
			user_relation:get_user_friend_opts(Server,User)
		end;
	_ ->
		user_relation:get_user_friend_opts(Server,User)
	end.

get_user_friends(Server,User,Key) ->
	case catch get_user_hash_pid(User) of
	[{Pid,_ }] ->
		case gen_server:call(Pid,get_random_user_hash_pid)  of
		Rpid when is_pid(Rpid) ->
			gen_server:call(Rpid,{get_user_friends,Key,User});
		_ ->
			get_ets_user_friends(Server,User,Key)
		end;
	_ ->
		get_ets_user_friends(Server,User,Key)
	end.

check_user_friend_relationship(Server,From,To,Domain) ->
	case catch get_user_hash_pid(To) of
	[{Pid,_ }] ->
		case gen_server:call(Pid,get_random_user_hash_pid)  of
		Rpid when is_pid(Rpid) ->
			gen_server:call(Rpid,{check_friend,From,To,Domain});
		_ ->
			check_user_friend_relation(Server,From,To,Domain)
		end;
	_ ->
		false
	end.

add_user_friend(Server,From,To,Version) ->
	case catch ejabberd_odbc:sql_query(Server,
		[<<"insert into user_friends(username,friend,host,relationship,version) values ('">>,From#jid.luser,<<"','">>,
			To#jid.luser,<<"','">>,To#jid.lserver,<<"',1,">>,Version,<<");">>]) of
	{updated,1} ->
		hash_add_user(From#jid.luser,To#jid.luser,To#jid.lserver,Version);
	_ ->
		case catch ejabberd_odbc:sql_query(Server,
		[<<"update user_friends set relationship = 1,version = version +1  where username = '">>,From#jid.luser,
			<<"' and friend = '">>,To#jid.luser,<<"';">>]) of
		 {updated,1} ->
		 	hash_add_user(From#jid.luser,To#jid.luser,To#jid.lserver,Version);
		_ ->	
			false
		end
	end.

hash_add_user(From,To,Host,Version) ->
	case catch get_user_hash_pid(From) of
	[{Pid,_ }] ->
		case gen_server:call(Pid,get_random_user_hash_pid)  of
		Rpid when is_pid(Rpid) ->
			gen_server:call(Rpid,{add_friend,From,To,Host,Version});		
		_ ->
			user_relation:add_friend(From,To,Host,Version)
		end;
	_ ->
		user_relation:add_friend(From,To,Host,Version)
	end.

add_user_friend_opts(Server,User,Mode,Question,Answer,Version) ->
	case catch ejabberd_odbc:sql_query(Server,
		[<<"insert into user_relation_opts(username,vld_friend_opt,validate_quetion,validate_answer,vesion) values ('">>,User,<<"',">>,
		Mode,<<",'">>,Question,<<"','">>,Answer,<<"',">>,Version,<<");">>]) of
	{updated,1} -> 
		add_ets_user_friend_opts(User,Mode,Question,Answer,Version);
	_ ->
		case Mode of 
		<<"2">> ->
			case catch ejabberd_odbc:sql_query(Server,
				[<<"update user_relation_opts set vld_friend_opt = 2, validate_quetion = '">>,Question,
				   <<"', validate_answer = '">>,Answer,<<"' where username = '">>,User,<<"';">>]) of
			{updated,1} -> 
				add_ets_user_friend_opts(User,Mode,Question,Answer,Version);
			_ ->
				false
			end;
		_ ->
			case catch ejabberd_odbc:sql_query(Server,
				[<<"update user_relation_opts set vld_friend_opt = ">>,Mode,<<", validate_quetion = '">>,Question,
				   <<"', validate_answer = '">>,Answer,<<"' where username = '">>,User,<<"';">>]) of
			{updated,1} -> 
				add_ets_user_friend_opts(User,Mode,Question,Answer,Version);
			_ ->
				false
			end
		end
	end.
			
add_ets_user_friend_opts(User,Mode,Question,Answer,Version) ->
	case catch get_user_hash_pid(User) of
	[{Pid,_ }] ->
		case catch gen_server:call(Pid,get_random_user_hash_pid)  of
		Rpid when is_pid(Rpid) ->
			catch gen_server:call(Rpid,{set_friend_opts,User,Mode,Question,Answer,Version});     
		_ ->
			user_relation:insert_ets_friend_opts(User,Mode,Question,Answer,Version)
		end;
	_ ->
		user_relation:insert_ets_friend_opts(User,Mode,Question,Answer,Version)
	end.
			
get_user_hash_pid(User) ->
	lists:filter(fun({_Pid,Range_Num}) ->
		is_suitable_hash(User,Range_Num) end,ets:tab2list(hash_user_pid)).

is_suitable_hash(User,{R1,R2}) ->
	Hash_Num = erlang:hash(User,?HASH_SIZE),
	if (R1 < R2 andalso R1 < Hash_Num andalso  Hash_Num < R2)
		orelse (R1 >= R2 andalso (( Hash_Num >= 1  andalso Hash_Num =< R2 ) orelse  (Hash_Num >= R1 andalso Hash_Num =< ?HASH_SIZE))) ->
			true;
	true ->
			false
	end;
is_suitable_hash(_,_) ->
    false.

check_user_friend_relation(Server,From,To,Domain) ->
	case catch ejabberd_odbc:sql_query(Server,
		[<<"select relationship from user_friends where username = '">>,From,<<"' and friend = '">>,To,<<"' and host = '">>,Domain,<<"';">>]) of
	{selected, _ , [[<<"1">>]]}  ->
		true;
	_ ->
		false
	end.

handle_del_friend(Host,From,To,Domain,Mode) ->
	case catch get_user_hash_pid(From) of
	[{Pid,_ }] ->
		case gen_server:call(Pid,get_random_user_hash_pid)  of
		Rpid when is_pid(Rpid) ->
			catch gen_server:call(Rpid,{del_friend,From,To,Domain,Mode});
		_ ->
				user_relation:del_friend(Host,From,To,Domain,Mode)
		end;
	_ ->
		user_relation:del_friend(Host,From,To,Domain,Mode)
	end.	

get_ets_user_friends(Host,User,Key) ->
	Rslt = 
		case catch  ets:lookup(user_friends,User) of
		[Friends] when is_record(Friends,user_friends) ->
			Friends#user_friends.friends;
		_ ->
			user_relation:get_user_friends(Host,User)
		end,
	case Key of
	size ->
		length(Rslt);
	_ ->
		Rslt
	end.

update_handle_info(Server) ->
    catch mod_hash_nodes:update_ets_user_hash(Server),
    catch user_relation:get_all_user_friend_opts(Server),
    catch user_relation:get_all_user_friends(Server),
    catch mod_recv_msg_limit:update_all_recv_msg_limit(Server).
