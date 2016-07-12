-module(mod_update).

-behaviour(gen_server).
-behaviour(gen_mod).

-export([start/2,stop/1,start_link/1,init/1]).

-export([handle_call/3, handle_cast/2,
 	    handle_info/2, terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("ejabberd_extend.hrl").

-define(SERVER, ?MODULE).
-define(PROCNAME, ?SERVER).

-record(state, {pg_timer,ets_timer,list_user_timer,iplimit_timer, server}).

start(Host,Opts) ->
         Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
         ChildSpec = {Proc,{?MODULE, start_link, [{Host,Opts}]}, permanent, infinity,worker,[Proc]},
         {ok,_Pid} = supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    	Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    	supervisor:terminate_child(ejabberd_sup, Proc),
    	supervisor:delete_child(ejabberd_sup, Proc).

start_link({Server,Opts}) ->
	 gen_server:start_link({local, ?SERVER}, ?MODULE, [{Server,Opts}], []).

init([{Server,_Opts}]) ->
	create_ets_table(),
	{ok, Pg_timer} = timer:send_interval(7200*1000, update_pgsql),
	{ok, Ets_timer} = timer:send_interval(120*1000, update_ets),
    {ok, List_user_timer} = timer:send_interval(3600*1000, update_list_user),
    {ok, Iplimit_timer} = timer:send_interval(600 * 1000, update_iplimit),
	{ok, #state{pg_timer = Pg_timer,ets_timer = Ets_timer,
				   server = Server,list_user_timer = List_user_timer, iplimit_timer = Iplimit_timer},0}.

handle_call(stop, _From, State=#state{pg_timer = Pg_timer,ets_timer = Ets_timer,
	   	list_user_timer = List_user_timer, iplimit_timer = Iplimit_timer}) ->
	{ok, cancel} = timer:cancel(Pg_timer),
	{ok, cancel} = timer:cancel(Ets_timer),
	{ok, cancel} = timer:cancel(List_user_timer),
	{ok, cancel} = timer:cancel(Iplimit_timer),
	{stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->    
    {noreply, State}.

handle_info(timeout, State=#state{server = LServer}) ->
	update_online_users(LServer),
	update_away_users(LServer),
	update_department_info(LServer),
    update_vcard_version(LServer),
    update_blacklist(LServer),
	update_user_list(LServer),
	update_whitelist(LServer),
	update_user_sn(LServer),
	update_muc_vcard_version(LServer),
	subscription:update_subscription_info(LServer),
	subscription:update_user_robots(LServer),
	iplimit_util:update_iplimit(LServer),
	{noreply, State};
handle_info(update_pgsql, State=#state{server = LServer}) ->
	update_user_list(LServer),
	update_user_sn(LServer),
	{noreply, State};
handle_info(update_ets, State=#state{server = LServer}) ->
	update_online_users(LServer),
	update_away_users(LServer),
	update_user_status_list(LServer),
	{noreply, State};
handle_info(update_list_user, State=#state{server = LServer}) ->
	update_whitelist(LServer),
    update_blacklist(LServer),
	subscription:update_subscription_info(LServer),
	subscription:update_user_robots(LServer),
    {noreply, State};
handle_info(update_iplimit, State=#state{server = LServer}) ->
    iplimit_util:update_iplimit(LServer),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

update_department_info(LServer) ->
	ets:delete_all_objects(name_nick),
	ets:delete_all_objects(user_department),
	case catch odbc_queries:get_department_info(jlib:nameprep(LServer)) of
    {selected,_,Res} when is_list(Res) ->
      Info =
           lists:map(fun([D1,D2,D3,D4,D5,J,N,D,_Fp,_Sp]) ->
	 			catch ets:insert(department_users,#department_users{dep1 = D1,dep2 = D2,dep3 = D3,dep4 = D4,dep5 = D5,user = J}),
	 			catch ets:insert(nick_name,{N,J}),
	 			catch ets:insert(name_nick,{J,N,D}),
				{obj, [{"U", J}, {"D", D},{"N",N},{"S",0}]}
             end,Res),
      handle_abbreviate(Res),
      handle_department:make_tree_dept(Res,true),
      handle_department:get_dept_json(true),
      Num = length(Res),
	  ets:insert(cache_info,#cache_info{name = <<"user_num">>,cache = Num}),
	  ets:insert(cache_info,#cache_info{name = <<"list_depts">>,cache = Info});
    _ ->
        ok
    end.

handle_abbreviate(Res) ->
	Abbreviates =
		lists:map(fun([_D1,_D2,_D3,_D4,_D5,J,_N,_D,Fp,Sp]) ->
			{SF1,SF2} = spilt_abbreviate(binary_to_list(Fp)),
			{SS1,SS2} = spilt_abbreviate(binary_to_list(Sp)),
			{obj, [{"U", J}, {"FPY", list_to_binary(SF1)},{"FSX",list_to_binary(SF2)},
				{"SPY",list_to_binary(SS1)},{"SSX",list_to_binary(SS2)}]} end,Res),
	ets:delete(cache_info,<<"abbreviates">>),
	catch ets:insert(cache_info,#cache_info{name = <<"abbreviates">>,cache = rfc4627:encode(Abbreviates)}).
												
spilt_abbreviate(Pinyin) ->
	Abbreviates = string:tokens(Pinyin,"|"),
	case length(Abbreviates) of
	0 ->
		{[],[]};
	1 ->
		[P] = Abbreviates,
		{P,[]};
	N when N rem 2 == 0 ->
		Abbreviate1 = 
			lists:foldl(fun(Num,Acc) -> 
				case Num rem 2 of 
				1 ->
					case Acc of
					[] ->
						[lists:nth(Num,Abbreviates)];
					_ ->
						[lists:nth(Num,Abbreviates)] ++ "|" ++ Acc 
					end;
				_ ->
					Acc
				end end,[],lists:seq(1,N)),
		Abbreviate2 = 
			lists:foldl(fun(Num,Acc) -> 
				case Num rem 2 of 
				0 ->
					case Acc of
					[] ->
						[lists:nth(Num,Abbreviates)];
					_ ->
						[lists:nth(Num,Abbreviates)] ++ "|" ++ Acc 
					end;
				_ ->
					Acc
				end end,[],lists:seq(1,N)),
		{Abbreviate1,Abbreviate2};
	_ ->
		{[],[]}
	end.

get_update_time(Opts) ->
    gen_mod:get_opt(update_time_interval, Opts,
	    fun(A) -> A end,
            500000).

create_ets_table() ->
	table_handle:create_mnesia_table(),
	table_handle:create_ets_table(),
    iplimit_util:create_ets().

update_vcard_version(Server) ->
    case catch odbc_queries:get_vcard_version(Server) of
    {selected,_,Res} when is_list(Res) ->
		ets:delete_all_objects(vcard_version),
		lists:foreach(fun([User,Ver,Url]) ->
				case ets:lookup(name_nick,User) of
				[{_,N,_}] ->
					catch ets:insert(vcard_version,#vcard_version{user = User,version = Ver, name = N,url = Url});
				_ ->
					catch ets:insert(vcard_version,#vcard_version{user = User,version = Ver, name = <<"">>,url = Url})
				end  end,Res);
	_ ->
	          ok
    end.

update_muc_vcard_version(Server) ->
   case catch odbc_queries:get_muc_vcard_version(Server) of
   {selected,_,Res} when is_list(Res) ->
		ets:delete_all_objects(muc_vcard),
		lists:foreach(fun([M,N,D,T,P,V]) ->
			ets:insert(muc_vcard,#muc_vcard{muc_name = M,show_name = N,muc_desc = D,muc_title = T,muc_pic = P,version = V}) end,Res);
	_ ->
		ok
	end.

update_blacklist(LServer) ->
    ets:delete_all_objects(blacklist),
	case catch odbc_queries:get_blacklist(jlib:nameprep(LServer)) of
	{selected,[<<"username">>],SRes}
	when is_list(SRes) ->
		lists:foreach(fun([Username]) ->
			ets:insert(blacklist,{Username}) end,SRes);
	Error ->
		?DEBUG("Get blacklist error for ~p ~n",[Error])
	end.
    
update_user_list(LServer) ->
	case catch odbc_queries:list_users(jlib:nameprep(LServer)) of
	{selected,[<<"username">>],SRes}
	when is_list(SRes) ->
    	ets:delete_all_objects(user_list),
		lists:foreach(fun([Username]) ->
			ets:insert(user_list,{Username}) end,SRes);
	Error ->
		?DEBUG("Get user_list error for ~p ~n",[Error])
	end.

update_whitelist(LServer) ->
	case catch odbc_queries:get_white_list_users(jlib:nameprep(LServer)) of
	{selected,[<<"username">>, <<"single_flag">>],SRes}
	when is_list(SRes) ->
    	ets:delete_all_objects(whitelist),
		lists:foreach(fun([Username, SingleFlag]) ->
			ets:insert(whitelist,{Username, SingleFlag}) end,SRes);
	Error ->
		?DEBUG("Get whitelist error for ~p ~n",[Error])
	end.

update_online_users(Server) ->
	case  ejabberd_sm:get_vh_session_list(Server) of
	Online_users when is_list(Online_users) ->
		OUsers = 
			lists:map(fun({User,_,_}) ->
				ets:insert(online_users,{User}),User end,Online_users),
        ets:insert(cache_info,#cache_info{name = <<"online1">>,cache = {obj, [{<<"Online_Users">>,OUsers}]}});
	_ ->
		ok
	end.
		
update_away_users(LServer) ->
    ets:delete_all_objects(away_users),
	Server = jlib:nameprep(LServer),
	case catch mnesia:dirty_select(session,[{#session{usr = {'$1','$2','_' },show = <<"away">>, _ = '_'},[{'==', '$2', Server}], ['$1']}]) of 
	U when is_list(U) ->
		lists:foreach(fun(User) ->
			ets:insert(away_users,{User}) end,U),
		Away_Users = [{obj, [{<<"Away_Users">>,U}]}],
		ets:insert(cache_info,#cache_info{name = <<"away_users">>,cache = Away_Users });
	_ ->
		ok
	end.

update_user_sn(LServer) ->
       ets:delete_all_objects(sn_user),
       case catch ejabberd_odbc:sql_query(LServer,[<<"select sn,hire_type from users;">>]) of
       {selected, [<<"sn">>,<<"hire_type">>], SRes} when is_list(SRes) ->
             lists:foreach(fun([Sn,Hire_type]) ->
					case Hire_type =/= <<"实习生（地推）"/utf8>> andalso Hire_type =/= <<"实习（HC）"/utf8>> 
						andalso Hire_type =/= <<"外包"/utf8>> of 
					true ->
						ets:insert(sn_user,{Sn,Hire_type});
					_ ->
						ok
					end end,SRes);
	    _ ->
	               ok
       end.

update_user_status_list(_Server) ->
	catch ets:delete_all_objects(online_status),
	lists:foreach(fun({U}) ->
			case ets:lookup(away_users,U) of
			[] ->
				ets:insert(online_status,#online_status{user = U,status = 6});
			_ ->
				ets:insert(online_status,#online_status{user = U,status = 1})
			end end, ets:tab2list(online_users)),

    Res =
        lists:map(fun(U) ->
	       	{obj, [{"U", U#online_status.user},{"S",U#online_status.status}]} end,ets:tab2list(online_status)),
	catch ets:delete(cache_info,<<"online2">>),
    ets:insert(cache_info,#cache_info{name = <<"online2">>,cache = {obj,[{"Online_User_Status",Res}]}}).

