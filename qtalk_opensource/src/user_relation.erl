-module(user_relation).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		                terminate/2,code_change/3]).
-export([get_all_user_friend_opts/1,get_all_user_friends/1,get_user_friend_opts/2,get_user_friends/2,insert_ets_friend_opts/5]).
-export([add_friend/4,del_friend/5]).
-behaviour(gen_server).

-define(SERVER, ?MODULE).
-include("ejabberd.hrl").
-include("logger.hrl").

-record(state, {host,pid}).
-record(friend_opts,{user,rec_msg_flag,vld_friend_flag,validate_quetion,validate_answer,version}).
-record(user_friends,{user,friends,unfriends}).
-record(recv_msg_limit,{user,mode,version}).


start_link(Host,Opts) ->
    gen_server:start_link(?MODULE, [Host,Opts], []).

init([Host,_Opts]) ->
	mod_user_relation:add_pid(Host,self()),
    {ok, #state{host = Host,pid = self()}}.

handle_call({get_user_relation_opts,User}, _From, #state{host = Host} = State) ->
	Reply = 
		case catch  ets:lookup(friend_opts,User) of
		[Friend_opts] when is_record(Friend_opts,friend_opts) ->
			Friend_opts;
		_ ->
			get_user_friend_opts(Host,User)
		end,
    {reply, Reply, State};
handle_call({get_user_friends,Key,User}, _From,#state{host = Host} = State) ->
	Reply =  mod_hash_user:get_ets_user_friends(Host,User,Key),
    {reply, Reply, State};
handle_call({check_friend,From,To,Domain}, _From,#state{host = Host} = State) ->
	Reply = 
		case catch ets:lookup(user_friends,From) of
		[] ->
			ets:insert(user_friends,#user_friends{user = From,friends = [],unfriends = []}),
			Rslt = get_user_friends(Host,From),
		   	proplists:get_value({To,Domain},Rslt) =/= undefined;
		[UF] when is_record(UF,user_friends) ->
			proplists:get_value({To,Domain},UF#user_friends.friends) =/= undefined;
		_ ->
			false
		end,
    {reply, Reply, State};
handle_call({add_friend,From,To,Domain,Version}, _From,#state{host = Host} = State) ->
	Reply = 
		add_friend(From,To,Domain,Version),
    {reply, Reply, State};
handle_call({set_friend_opts,User,Mode,Question,Answer,Version}, _From,#state{host = Host} = State) ->
	Reply = insert_ets_friend_opts(User,Mode,Question,Answer,Version), 
    {reply, Reply, State};
handle_call({get_user_recv_limit,User}, _From, #state{host = Host} = State) ->
	Reply = 
		case catch  ets:lookup(recv_msg_limit,User) of
		[Recv_msg_limit] when is_record(Recv_msg_limit,recv_msg_limit) ->
			Recv_msg_limit;
		_ ->
			mod_recv_msg_limit:get_user_recv_limit(Host,User)
		end,
    {reply, Reply, State};
handle_call({del_friend,From,To,Domain,Mode}, _From,#state{host = Host} = State) ->
	Reply =	del_friend(Host,From,To,Domain,Mode),
    {reply, Reply, State};
handle_call({set_user_recv_limit,User,Recv_limit}, _From, #state{host = Host} = State) ->
	Reply =	mod_recv_msg_limit:set_user_recv_limit(Host,User,Recv_limit),
    {reply, Reply, State};
handle_call(Msg, _From, State) ->
    {reply,  Msg, State}.

handle_cast({sql,Host,Sql},State) ->
	{noreply,State};
handle_cast(stop, State) ->
	mod_user_relation:remove_pid(State#state.host,self()),
    {stop, normal, State}.

handle_info(_From,State) ->
    {noreply,State}.

terminate(_Reason, State) ->
	mod_user_relation:remove_pid(State#state.host,self()),
	{ok,State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

get_all_user_friend_opts(Server) ->
	Pid = whereis(mod_hash_user),
	catch ets:delete_all_objects(friend_opts),
	case catch ejabberd_odbc:sql_query(Server,
		[<<"select username,rec_msg_opt,vld_friend_opt,validate_quetion,validate_answer,vesion from user_relation_opts">>]) of
	{selected, _ , SRes} when is_list(SRes) ->
		lists:foreach(fun([U,R,V,Q,A,Ver]) ->	
			  case mod_hash_user:get_user_hash_pid(U) of
			[{Pid,_ }] ->
				ets:insert(friend_opts,#friend_opts{user = U ,rec_msg_flag = R,vld_friend_flag = V ,
						validate_quetion = Q,validate_answer = A,version = Ver});
			_ ->
				[]
			end end,SRes);
	_ ->
		ok
	end.
			
get_all_user_friends(Server) ->
	Pid = whereis(mod_hash_user),
	catch ets:delete_all_objects(user_friends),
	case catch ejabberd_odbc:sql_query(Server,
		[<<"select username,friend,host,relationship,version  from user_friends">>]) of
	{selected, _ , SRes} when is_list(SRes) ->
		lists:foreach(fun([U,F,H,R,V]) ->	
		  case catch mod_hash_user:get_user_hash_pid(U) of
			[{Pid,_ }] ->
				insert_user_friend(U,{F,H},R,V);
			_ ->
				[]
			end end,SRes);
	_ ->
		ok
	end.

insert_user_friend(User,Friend,Relation,Version) ->
	case ets:lookup(user_friends,User) of
	[] ->
		case Relation of
		<<"1">> ->
			ets:insert(user_friends,#user_friends{user = User,friends = [{Friend,Version}],unfriends = []});
		_ ->
			ets:insert(user_friends,#user_friends{user = User,unfriends = [{Friend,Version}],friends = []})
		end;
	[OF] when is_record(OF,user_friends) ->
		case Relation of
		<<"1">> ->
			Old_freind_list = OF#user_friends.friends,
			ets:insert(user_friends,OF#user_friends{friends = lists:append([{Friend,Version}],Old_freind_list)});
		_ ->
			Old_unfreind_list = OF#user_friends.unfriends,
			ets:insert(user_friends,OF#user_friends{unfriends = lists:append([{Friend,Version}],Old_unfreind_list)})
		end;
	Error ->
		?INFO_MSG("insert_user_friend ets error ~p ~p,Reason ~p  ~n",[User,Friend,Error])
	end.

get_user_friend_opts(Server,Username) ->
	case catch ejabberd_odbc:sql_query(Server,
[<<"select rec_msg_opt,vld_friend_opt,validate_quetion,validate_answer,vesion from user_relation_opts where username = '">>,Username,<<"';">>]) of
	{selected, _ , [[R,V,Q,A,Ver]]} ->
		Friend_opts = #friend_opts{user = Username ,rec_msg_flag = R,vld_friend_flag = V,validate_quetion = Q,validate_answer = A,version = Ver},
		ets:insert(friend_opts,Friend_opts),
		Friend_opts;
	_ ->
		[]
	end.

get_user_friends(Server,User) ->
	case catch ejabberd_odbc:sql_query(Server,
		[<<"select friend,host,relationship,version from user_friends where username = '">>,User,<<"';">>]) of
	{selected,[<<"friend">>,<<"host">>,<<"relationship">>,<<"version">>],[]} ->
		ets:insert(user_friends,#user_friends{user = User,friends = [],unfriends = []}),
		[];
	{selected, _ , SRes} when is_list(SRes) ->
		lists:flatmap(fun([F,H,R,V]) ->	
				insert_user_friend(User,{F,H},R,V),
				case R of
				<<"1">> ->
					[{{F,H},V}];
				_ ->
					[] 
				end end,SRes);
	_ ->
		[]
	end.

add_friend(From,To,Domain,Version) ->
	case catch ets:lookup(user_friends,From) of
	[] ->
		ets:insert(user_friends,#user_friends{user = From,friends = [{{To,Domain},Version}],unfriends = []}),
		true;
	[UF] when is_record(UF,user_friends) ->
		New_User_friends = do_handle_friends(add,UF,To,Domain,Version),
		ets:insert(user_friends,New_User_friends),
		true;
	_ ->
		false
	end.

del_friend(Server,From,To,Domain,Version) ->
	case catch ets:lookup(user_friends,From) of
	[] ->
		?DEBUG("New User Friendsnulll  ~n",[]),
		true;
	[UF] when is_record(UF,user_friends) ->
		New_User_friends = do_handle_friends(del,UF,To,Domain,Version),
		catch ets:insert(user_friends,New_User_friends),
		case catch ejabberd_odbc:sql_query(Server,
			[<<"update user_friends set relationship = 0 where username = '">>,From,<<"' and friend = '">>,To,<<"';">>]) of
		{updated,1} ->
			true;
		_ ->
			true
		end;
	_ ->
		false
	end.

do_handle_friends(add,User_friends,User,Domain,Version) ->
	Friends = 
		case proplists:get_value({User,Domain},User_friends#user_friends.friends) of
		undefined ->
			User_friends#user_friends.friends ++ [{{User,Domain},Version}];
		V ->
			User_friends#user_friends.friends
		end,
	UnFriends = proplists:delete({User,Domain},User_friends#user_friends.unfriends),
	User_friends#user_friends{friends = Friends,unfriends = UnFriends};
do_handle_friends(del,User_friends,User,Domain,Version) ->
	Friends =  proplists:delete({User,Domain},User_friends#user_friends.friends),
	UnFriends = 
		case proplists:get_value({User,Domain},User_friends#user_friends.unfriends) of
		undefined ->
			User_friends#user_friends.unfriends ++ [{{User,Domain},Version}];
		V ->
			User_friends#user_friends.unfriends
		end,
	User_friends#user_friends{friends = Friends,unfriends = UnFriends};
do_handle_friends(_,User_friends,_User,_Domain,Version) ->
	User_friends.
		

insert_ets_friend_opts(User,Mode,Question,Answer,Version) ->
	catch ets:insert(friend_opts,#friend_opts{user = User ,vld_friend_flag = Mode ,validate_quetion = Question,
				validate_answer = Answer,version = Version}),
	true.
