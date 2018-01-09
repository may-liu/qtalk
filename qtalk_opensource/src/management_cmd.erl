-module(management_cmd).
-export([get_user_num/2,get_muc_opts/2,update_muc_opts/2,stop_muc/2,start_muc/2,remove_muc_user/2]).
-export([judge_muc_online/2,destroy_muc/2,get_online_status/2,kick_user/2,get_user_mac_key/2]).
-export([update_user_info/2,delete_user/2,restart_pgsql_odbc/2,get_user_registed_muc_num/2]).
-export([insert_iplimit/2,delete_iplimit/2,get_user_rescource/2,get_user_rescource_list/2]).
-export([migrate_one_muc_by_name/2,migrate_num_mucs/2,close_invaild_session/0]).
-export([clear_s2s_session/0,check_invaild_session/1,do_kick_user/2,send_check_msg/0]).
-export([add_muc_users/2,do_stop_muc/2]).

-include("ejabberd.hrl").
-include("logger.hrl").

-record(muc_room, {name_host = {<<"">>, <<"">>} :: {binary(), binary()} | {'_', binary()},
	   				opts = [] :: list() | '_'}).

-record(muc_online_room, {name_host = {<<"">>, <<"">>} :: {binary(), binary()} | '$1' | {'_', binary()} | '_',
							pid = self() :: pid() | '$2' | '_' | '$1'}).

-record(session, {sid, usr, us, priority, info,show}).

%%--------------------------------------------------------------------
%% @date 2015-07-01
%% 获取当前用户数
%% qunar.com
%%--------------------------------------------------------------------
get_user_num(Server,Args) ->
	User_num = ejabberd_sm:get_vh_session_number(Server),
	http_utils:gen_result(true, <<"0">>, User_num).

%%--------------------------------------------------------------------
%% @date 2015-07-01
%% 获取聊天室配置项
%% qunar.com
%%--------------------------------------------------------------------
get_muc_opts(Server,Args) ->
	Conference_server = str:concat(<<"conference.">>,Server),
	case proplists:get_value("muc",Args) of
	undefined ->
		http_utils:gen_result(false, <<"-1">>, <<"Not find Muc name">>);
	Muc_name ->
	    SName = ejabberd_odbc:escape(Muc_name),
	    SHost = ejabberd_odbc:escape(Conference_server),
		case catch ejabberd_odbc:sql_query(Server, [<<"select opts from muc_room where name='">>,SName, <<"' and host='">>, SHost,<<"';">>]) of
		{selected, [<<"opts">>], [[Opts]]} ->
			BOpts = mod_muc:opts_to_binary(ejabberd_odbc:decode_term(Opts)),
			New_opts = rebuilt_opts(BOpts),
			http_utils:gen_result(true, <<"0">>, [{obj,New_opts}])
		end
	end.

%%--------------------------------------------------------------------
%% @date 2015-07-01
%% 重设聊天室配置项
%% qunar.com
%%--------------------------------------------------------------------
rebuilt_opts(Opts) ->
	Affiliations = proplists:get_value(affiliations,Opts),
	List_opts = lists:flatmap(fun({{U,S,R},{Aff,J}}) -> 
				tuple_to_list({U,<<"@">>,S,R}) ++ tuple_to_list({<<"/">>,atom_to_list(Aff),J,<<";">>})
				end,Affiliations),
	New_opts = list_to_binary(List_opts),
	lists:append(proplists:delete(affiliations,Opts),[{affiliations,New_opts}]).

%%--------------------------------------------------------------------
%% @date 2015-07-01
%% 更新聊天室配置项
%% qunar.com
%%--------------------------------------------------------------------
update_muc_opts(Server,Args) ->
	Conference_server = str:concat(<<"conference.">>,Server),
	case proplists:get_value("muc",Args) of
	undefined ->
		http_utils:gen_result(false, <<"-1">>, <<"Not find Muc name">>);
	Muc_name ->
		SName = ejabberd_odbc:escape(Muc_name),
		SHost = ejabberd_odbc:escape(Conference_server),
		Muc_opts = 
			case catch ejabberd_odbc:sql_query(Server,[<<"select opts from muc_room where name='">>,SName, <<"' and host='">>, SHost,<<"';">>]) of
			{selected, [<<"opts">>], [[Opts]]} ->
				mod_muc:opts_to_binary(ejabberd_odbc:decode_term(Opts));
			_ ->
				error
			end,
		
		case Muc_opts  of
		error ->
			http_utils:gen_result(false, <<"-1">>, <<"Muc not exit or Muc is temp Muc">>);
		_ ->
			New_opts = do_update_muc_opts(Server,Muc_name,Muc_opts,Args),
			New_muc = #muc_room{name_host= {Muc_name,Conference_server},opts = New_opts},
			mod_muc:store_room(Server,Conference_server,Muc_name,New_opts,odbc),
			case mnesia:dirty_read(muc_online_room, {Muc_name,Conference_server}) of
			[] -> 
				http_utils:gen_result(true, <<"0">>, <<"Muc set opt ok,muc not start">>);
			[M] ->
				Pid = M#muc_online_room.pid,
				gen_fsm:send_all_state_event(Pid,update_state),
				http_utils:gen_result(true, <<"0">>, <<"Muc set opt ok">>)
			end
		end
	end.

do_update_muc_opts(Server,Muc_name,Opts,Args) ->
	KeyList = ["public","persistent","max_users"],
	New_opts = lists:foldl(fun(Opt,Acc) ->
			   		case proplists:get_value(Opt,Args) of
					undefined ->
						Acc;
					<<>> ->
						Acc;
					V ->
						New_acc = proplists:delete(list_to_atom(Opt),Acc),
						case Opt of
						"max_users" ->
							lists:append(New_acc,[{list_to_atom(Opt),binary_to_integer(V)}]);
						_ ->
							lists:append(New_acc,[{list_to_atom(Opt),list_to_atom(binary_to_list(V))}])
						end
					end end,Opts,KeyList),
	New_opts1 = 
		case proplists:get_value( "affiliations",Args) of
		undefined ->
			New_opts;
		<<>> ->
			New_opts;
		V ->
			Aff_opts = 
				case proplists:get_value(affiliations,New_opts) of
				undefined ->
					[{{<<"none">>,Server,<<>>},{owner,<<>>}}];
				Vo ->
					Vo
				end,
			Opts_mv_aff = proplists:delete(affiliations,New_opts),
			New_aff_opts = 
					lists:flatmap(fun({{U,S,R},{Aff,J}}) -> 
						case Aff of
						owner ->
							[{{V,S,R},{Aff,J}}];
						_ ->
							[{{U,S,R},{Aff,J}}]
						end end,Aff_opts),
			lists:append(Opts_mv_aff,[{affiliations,New_aff_opts}])
		end,
	New_opts1.

%%--------------------------------------------------------------------
%% @date 2015-07-01
%% 停止聊天室
%% qunar.com
%%--------------------------------------------------------------------
stop_muc(Server,Args) ->
	Conference_server = str:concat(<<"conference.">>,Server),
	case proplists:get_value("muc",Args) of
	undefined ->
		http_utils:gen_result(false, <<"-1">>, <<"Not find Muc name">>);
	Muc_name ->
		case do_stop_muc(Muc_name,Conference_server) of
		ok ->
		 	http_utils:gen_result(true, <<"0">>, <<"Stop Muc Sucess">>);
		_ ->
		 	http_utils:gen_result(false, <<"-1">>, <<"Muc not exit">>)
		end
	end.

do_stop_muc(Muc_name,Conference_server) ->
	case mnesia:dirty_read(muc_online_room, {Muc_name, Conference_server}) of 
	[] ->
		 error;
	[R] ->
		 Pid = R#muc_online_room.pid,
		 gen_fsm:send_all_state_event(Pid, {stop, <<"management close">>}),
		 ok
	end.




%%--------------------------------------------------------------------
%% @date 2015-07-01
%% 启动聊天室配置项
%% qunar.com
%%--------------------------------------------------------------------
start_muc(Server,Args) ->
	Conference_server = str:concat(<<"conference.">>,Server),
	case proplists:get_value("muc",Args) of
	undefined ->
		http_utils:gen_result(false, <<"-1">>, <<"Not find Muc name">>);
	Muc_name ->
		case mnesia:dirty_read(muc_online_room, {Muc_name, Conference_server}) of 
		[] ->
			 case do_start_muc(Server,Conference_server,Muc_name) of
			 ok ->
			 	http_utils:gen_result(true, <<"0">>, <<"Start Muc Sucess">>);
			 _ ->
				http_utils:gen_result(false, <<"-1">>, <<"Muc start failed">>)
			 end;	
		[R] ->
			 http_utils:gen_result(false, <<"-1">>, <<"Muc has already start">>)
		end
	end.

do_start_muc(Server,Conference_server,Muc_name) ->
	?DEBUG("Muc_name ~p ~n",[Muc_name]),
    SName = ejabberd_odbc:escape(Muc_name),
    SHost = ejabberd_odbc:escape(Conference_server),
	case catch ejabberd_odbc:sql_query(Server,[<<"select opts from muc_room where name='">>,SName, <<"' and host='">>, SHost,<<"';">>]) of
	{selected, [<<"opts">>], [[Opts]]} ->
		Bopts = mod_muc:opts_to_binary(ejabberd_odbc:decode_term(Opts)),
	 	{ok, Pid} = mod_muc_room:start(Conference_server,Server,{all,all,none,all},Muc_name,20,none,Bopts),
    	 F = fun() ->
	    	     mnesia:write(#muc_online_room{name_host = {Muc_name,Conference_server}, pid = Pid})  end,
     	mnesia:transaction(F),
		ok;
	  _ ->
	  	error
	end.

%%--------------------------------------------------------------------
%% @date 2015-07-01
%% 删除聊天室用户
%% qunar.com
%%--------------------------------------------------------------------
remove_muc_user(Server,Args) ->
	Conference_server = str:concat(<<"conference.">>,Server),
	case proplists:get_value("muc",Args) of
	undefined ->
		http_utils:gen_result(false, <<"-1">>, <<"Not find Muc name">>);
	Muc_name ->
		case mnesia:dirty_read(muc_online_room, {Muc_name, Conference_server}) of 
		[] ->
			 http_utils:gen_result(false, <<"-1">>, <<"Muc has stop or Muc not exist">>);
		[R] ->
			case proplists:get_value("user",Args) of
			undefined ->
			 	http_utils:gen_result(false, <<"-1">>, <<"User is null">>);
			User ->
				Pid = R#muc_online_room.pid,
				do_remove_muc_user(Server,User,Pid)
			end
		end
	end.

do_remove_muc_user(Server,User,Pid) ->
	gen_fsm:send_all_state_event(Pid, {remove_users,User,Server, <<"management remove user">>}),
	http_utils:gen_result(true, <<"0">>, <<"Remove user ok">>).

%%--------------------------------------------------------------------
%% @date 2015-07-01
%% 判断聊天室是否存活
%% qunar.com
%%--------------------------------------------------------------------
judge_muc_online(Server,Args) ->
	Conference_server = str:concat(<<"conference.">>,Server),
	case proplists:get_value("muc",Args) of
	undefined ->
		http_utils:gen_result(false, <<"-1">>, <<"Not find Muc name">>);
	Muc_name ->
		case mnesia:dirty_read(muc_online_room, {Muc_name, Conference_server}) of
		[] ->
			http_utils:gen_result(false, <<"-1">>, <<"Muc has stop or Muc not exist">>);
		[R] ->
			http_utils:gen_result(true, <<"0">>, <<"Muc is alive">>)
		end
	end.

%%--------------------------------------------------------------------
%% @date 2015-07-01
%% 销毁聊天室
%% qunar.com
%%--------------------------------------------------------------------
destroy_muc(Server,Args) ->
	Conference_server = str:concat(<<"conference.">>,Server),
	case proplists:get_value("muc",Args) of
	undefined ->
		http_utils:gen_result(false, <<"-1">>, <<"Not find Muc name">>);
	Muc_name ->
		case do_destroy_muc(Muc_name,Server,Conference_server) of
		ok ->
			http_utils:gen_result(true, <<"0">>, <<"Muc destroy ok">>);
		_ ->
			http_utils:gen_result(false, <<"-1">>, <<"Muc not alive">>)
		end
	end.

do_destroy_muc(Room,LServer,Server_Host) ->
	case mnesia:dirty_read(muc_online_room, {Room,Server_Host}) of	
	[] ->
		error;
	[M] ->
		?INFO_MSG("Destory Room ~s  by management cmd ~n",[Room]),
		Pid = M#muc_online_room.pid,
		gen_fsm:send_all_state_event(Pid, {destroy, <<"management close">>}),
		mod_muc:room_destroyed(Server_Host, Room,Pid, LServer),
		mod_muc:forget_room(LServer,Server_Host ,Room),
		catch odbc_queries:restore_muc_user_mark(LServer,Room),
    	catch odbc_queries:del_muc_users(LServer,<<"muc_room_users">>,Room),
    	catch odbc_queries:delete_muc_last(LServer,Room),
    	catch odbc_queries:del_muc_vcard_info(LServer,Room,<<"Admin Destroy">>),
		ok
	end.
			
%%--------------------------------------------------------------------
%% @date 2015-07-01
%% 获取用户状态
%%--------------------------------------------------------------------
get_online_status(Server,Args) ->
	case proplists:get_value("user",Args) of
	undefined ->
		http_utils:gen_result(false, <<"-1">>, <<"Not find User name">>);
	User ->
		case ejabberd_sm:get_user_resources(User,Server) of
		[] ->
			http_utils:gen_result(true, <<"0">>, <<"Offline">>);
		_ ->
			http_utils:gen_result(true, <<"0">>, <<"Online">>)
		end
	end.

%%--------------------------------------------------------------------
%% @date 2015-07-01
%% 踢人下线
%%--------------------------------------------------------------------
kick_user(Server,Args) ->
	case proplists:get_value("user",Args) of
	undefined ->
		http_utils:gen_result(false, <<"-1">>, <<"Not find User name">>);
	User ->
		case ejabberd_sm:get_user_resources(User,Server) of
		[] ->
			http_utils:gen_result(false, <<"-1">>, <<"User offline">>);
		_ ->
			case do_kick_user(User,Server) of
			ok ->
				http_utils:gen_result(true, <<"0">>, <<"Kick User sucess">>);
			_ ->
				http_utils:gen_result(false, <<"-1">>, <<"Kick User failed">>)
			end
		end
	end.

do_kick_user(User,Server) ->
	case mnesia:dirty_select(session,[{#session{sid = '$1',usr = {User,Server, '_'},_ = '_'}, [], ['$1']}]) of
	Users when is_list(Users) ->
		[Pid ! {kick, kicked_by_admin, []} || {_, Pid} <- Users],
		ok;
	_ ->
		error
	end.

%%--------------------------------------------------------------------
%% @date 2015-07-01
%% 获取用户key
%%--------------------------------------------------------------------
get_user_mac_key(Server,Args) ->
	case proplists:get_value("user",Args) of
	undefined ->
		http_utils:gen_result(false, <<"-1">>, <<"Not find User name">>);
	User ->
		case do_get_user_mac_key(User,Server) of
		error ->
			http_utils:gen_result(false, <<"-1">>, <<"Get user tkey error">>);
		LTk when is_list(LTk) ->
			Res = 
				lists:flatmap(fun({K,MK}) ->
						[{obj,[{"Tkey",K},{"MKey",MK}]}] end,LTk),
			JSon_Res = rfc4627:encode({obj,[{"data",Res}]}),
			http_utils:gen_result(true, <<"0">>, list_to_binary(JSon_Res));
		_ ->
			http_utils:gen_result(false, <<"-1">>, <<"Get user tkey error">>)
		end
	end.

do_get_user_mac_key(User,Server) ->
	case ejabberd_sm:get_user_resources(User,Server) of
	[] ->
		error;
	Rs when is_list(Rs) ->
		lists:flatmap(fun(R) ->
				case catch redis_link:hash_get(Server,1,binary_to_list(User),binary_to_list(R)) of
				{ok,undefined} ->
					[];
				{ok,V } ->
					case catch redis_link:hash_get(Server,2,binary_to_list(User),binary_to_list(V)) of
					{ok,undefined} ->
						[];
					{ok,Mac_Key} ->
						[{V,Mac_Key}];
					_ ->
						[{V,<<"undefined">>}]
					end;
				_ ->
					[]
				end end, Rs);
	_ ->
		error
	end.
				
%%--------------------------------------------------------------------
%% @date 2015-07-01
%% 更新用户信息
%%--------------------------------------------------------------------
update_user_info(Server,Args) ->
	case proplists:get_value("user",Args) of
	undefined ->
		http_utils:gen_result(false, <<"-1">>, <<"Not find User name">>);
	User ->
		case do_update_user_info(User,Server,Args) of 
		error ->
			http_utils:gen_result(false, <<"-1">>, <<"update user info failed">>);
		_ ->
			http_utils:gen_result(true, <<"0">>, <<"update user info sucess">>)
		end
	end.	
			
do_update_user_info(User,Server,Args) ->
	Key = [<<"password">>,<<"name">>,<<"dep1">>,<<"dep2">>,<<"dep3">>,<<"dep4">>,<<"dep5">>],
	KVL = 
		lists:foldl(fun(K,Acc) ->
			case  proplists:get_value(binary_to_list(K),Args) of
			undefined ->
				Acc;
			<<"">> ->
				Acc;
			V ->
				?DEBUG("V ~p ~n",[V]),
				case Acc of
				[] ->
					[{[K],[V]}];
				[{KL,VL}] ->
					New_KL = lists:append(KL,[K]),
					New_VL = lists:append(VL,[V]),
					[{New_KL,New_VL}];
				_ ->
					Acc
				end
			end end,[],Key),
	case KVL of
	[] ->
		ok;
	[{Fields,Vals}] ->
        Version = 
            case catch ejabberd_odbc:sql_query(Server,   [<<"select max(version) from users">>]) of
            {selected,[<<"max">>],[[V]]} when is_binary(V) ->
                integer_to_binary((binary_to_integer(V) + 1));
             _ ->
                <<"1">>
            end,

		Where =  [<<"username='">>,User,<<"'">>],
		case catch ejabberd_odbc:sql_transaction(Server,fun() ->
			 odbc_queries:update_no_insert(<<"users">>,Fields ++ [<<"hire_flag">>,<<"version">>],Vals ++ [<<"1">>,Version],Where) end) of	
		{atomic,ok} ->
			ok;
		Reason ->
			?DEBUG("Update error,Reason is ~p ~n",[Reason]),
			error
		end;
	_ ->
		error
	end.

%%--------------------------------------------------------------------
%% @date 2015-07-01
%% 删除用户信息
%%--------------------------------------------------------------------
delete_user(Server,Args) ->
    case proplists:get_value("user",Args) of
    undefined ->
        http_utils:gen_result(false, <<"-1">>, <<"Not find User name">>);
    User ->	
		case catch odbc_queries:del_user(Server,User) of
		{updated,1} ->
			http_utils:gen_result(true, <<"0">>, <<"delete user sucess">>);
		Reason  ->
			?DEBUG("remvoe error,Reason ~p ~n",[Reason]),
			http_utils:gen_result(false, <<"-1">>, <<"delete user failed">>)
		end
	end.

%%--------------------------------------------------------------------
%% @date 2015-07-01
%% 重启pgsql连接数据库
%%--------------------------------------------------------------------
restart_pgsql_odbc(Server,Args) ->
	 Registed_name = registered(),	
	 Mod_R_name1 = str:concat(<<"'ejabberd_odbc_sup_">>,Server),
	 Mod_R_name2 = str:concat(Mod_R_name1,<<"'">>),
	 Mod_name = list_to_atom(binary_to_list(Mod_R_name2)),
	 case lists:member(Mod_name,Registed_name) of
	 true ->
	 	case whereis(Mod_name) of
		undefined ->
        	http_utils:gen_result(false, <<"-1">>, <<"Not find pgsql odbc sup">>);
		Pid when is_pid(Pid)->
			exit(Pid, kill),
			http_utils:gen_result(true, <<"0">>, <<"restart pgsql odbc user sucess">>);
		_ ->
        	http_utils:gen_result(false, <<"-1">>, <<"Not find pgsql odbc sup">>)
		end;
	 _ ->
        http_utils:gen_result(false, <<"-1">>, <<"Not find pgsql odbc sup">>)
	end.
			
%%--------------------------------------------------------------------
%% @date 2015-07-01
%% 更新ets状态
%%--------------------------------------------------------------------
update_ets_info(Server,Args) ->
	 http_utils:gen_result(true, <<"0">>, <<"update ets sucess">>).	

%%--------------------------------------------------------------------
%% @date 2015-07-01
%% 获取用户注册聊天室数量
%%--------------------------------------------------------------------
get_user_registed_muc_num(Server,Args) ->
	case proplists:get_value("user",Args) of
	undefined ->
		http_utils:gen_result(false, <<"-1">>, <<"Not find User name">>);
	User -> 
		case do_get_user_registed_muc_num(Server,User) of
		error ->
        	http_utils:gen_result(false, <<"-1">>, <<"get_user_registed_muc_num error">>);
		Res when is_list(Res)->
			JSon_Res = rfc4627:encode({obj,[{"data",Res}]}),
			http_utils:gen_result(true, <<"0">>, list_to_binary(JSon_Res));
		_ ->
        	http_utils:gen_result(false, <<"-1">>, <<"get_user_registed_muc_num error">>)
		end
	end.

do_get_user_registed_muc_num(Server,User) ->
	case catch odbc_queries:get_use_registed_muc_num(Server,User) of
	{selected,[<<"count">>,<<"muc_name">>],SRes} 
	when is_list(SRes) ->
		 lists:map(fun([Count,Muc_name]) ->
				  {obj,[{"Muc_name",Muc_name},{"Num",Count}]}
				    end ,SRes);
	Reason ->
		?DEBUG("Reason is ~p ~n",[Reason]),
		error
	end.

%%--------------------------------------------------------------------
%% @date 2015-07-01
%% 添加ip白名单
%%--------------------------------------------------------------------
insert_iplimit(Server,Args) ->
	case proplists:get_value("ip",Args) of
	undefined ->
		http_utils:gen_result(false, <<"-1">>, <<"Not find Ip">>);
	Ip ->
		case do_insert_iplimit(Server,Ip,Args) of
		error ->
			http_utils:gen_result(false, <<"-1">>, <<"insert iplimit error">>);
		ok ->
			http_utils:gen_result(true, <<"0">>, <<"insert iplimit ok">>)
		end
	end.

do_insert_iplimit(Server,Ip,Args) ->
	Des = proplists:get_value("desc",Args,<<"undefined">>),
	Name = proplists:get_value("name",Args,<<"none">>),
	Priority = proplists:get_value("priority",Args,<<"1">>),
	case catch ejabberd_odbc:sql_query(Server,
			[<<"insert into iplimit(ip,descriptions,name,priority) values ('">>,Ip,<<"','">>,Des,<<"','">>,Name,<<"','">>,Priority,<<"');">>]) of

	{updated,1} ->
		ok;
	Reason ->
		?DEBUG("insert_iplimit error Reson ~p ~n",[Reason]),
		error
	end.

%%--------------------------------------------------------------------
%% @date 2015-07-01
%% 删除ip白名单
%%--------------------------------------------------------------------
delete_iplimit(Server,Args) ->
	case proplists:get_value("ip",Args) of
	undefined ->
		http_utils:gen_result(false, <<"-1">>, <<"Not find Ip">>);
	Ip ->
		case catch odbc_queries:delete_iplimit(Server,Ip) of
		{updated,1} ->
			http_utils:gen_result(true, <<"0">>, <<"delete iplimit ok">>);
		Reason ->
			?DEBUG("delete iplimit error Reson ~p ~n",[Reason]),
			http_utils:gen_result(false, <<"-1">>, <<"delete iplimit error">>)
		end
	end.

%%--------------------------------------------------------------------
%% @date 2015-07-01
%% 获取用户资源号
%%--------------------------------------------------------------------
get_user_rescource(Server,Args) ->
	case proplists:get_value("user",Args) of
	undefined ->
		http_utils:gen_result(false, <<"-1">>, <<"Not find user">>);
	User ->
		case ejabberd_sm:get_user_resources(User,Server) of
		[] ->
			http_utils:gen_result(false, <<"-1">>, <<"User offline">>);
		Rescources when is_list(Rescources) ->
			Res = lists:foldl(fun(R,Acc) ->
					lists:concat([Acc,[R,<<";">>]])
					end,[],Rescources),
            http_utils:gen_result(true, <<"0">>, list_to_binary(Res));
		_ ->	
			http_utils:gen_result(false, <<"-1">>, <<"User offline">>)
		end
	end.	
			
get_user_rescource_list(Server,Args) ->
	case proplists:get_value("key",Args) of
	undefined ->
		 http_utils:gen_result(false, <<"-1">>, <<"Not find key word">>);
	<<"All">> ->
		Ret = lists:flatmap(fun({U,S,R}) -> 
				lists:concat([" ",binary_to_list(U),"@",binary_to_list(S),"/",binary_to_list(R),",\n<br>"]) end,
					ejabberd_sm:get_vh_session_list(Server)),
		N = string:words(Ret,$,),
		Res = {obj,[{"num",N},{"list",list_to_binary(Ret)}]},
		http_utils:gen_result(true, <<"0">>, Res);
	Key ->
		Ret = lists:flatmap(fun({U,S,R}) -> 
				case str:str(R,Key) =/= 0 of
				true ->
					lists:concat([" ",binary_to_list(U),"@",binary_to_list(S),"/",binary_to_list(R),","]);
				_ ->
					[]
				end end,ejabberd_sm:get_vh_session_list(Server)),
		N = string:words(Ret,$,),
		Res = {obj,[{"num",N},{"list",list_to_binary(Ret)}]},
		http_utils:gen_result(true, <<"0">>, Res)
	end.

migrate_num_mucs(Server,Args) ->
	case proplists:get_value("num",Args) of
	Num ->
		migrate_mucs(Server,binary_to_integer(Num));
	_ ->
		ok
	end.

migrate_mucs(Server,Num) ->
	Mucs = ets:tab2list(muc_online_room),
	do_migrate_mucs(Num,Mucs,Server).

do_migrate_mucs(0,_,Server) ->
	http_utils:gen_result(true, <<"0">>,<<"migrate muc ok.">>);
do_migrate_mucs(_,[],Server) ->
	http_utils:gen_result(true, <<"0">>,<<"no muc to migrate.">>);
do_migrate_mucs(Num,Mucs,Server) ->
	Muc = lists:nth(1,Mucs),
	migrate_muc_by_name(Server,Num,Muc,Mucs).

migrate_muc_by_name(Server,Num,Muc,Mucs) ->
	Flag = 	do_migrate_muc_by_name(Server,Muc),
	case Flag of
	true ->
		do_migrate_mucs(Num-1,Mucs -- [Muc],Server);
	_ ->
		do_migrate_mucs(Num,Mucs -- [Muc],Server)
	end.

migrate_one_muc_by_name(Server,Args) ->
	Muc_name = proplists:get_value("muc",Args),
	Conference_server = proplists:get_value("domain",Args),
	case mnesia:dirty_read(muc_online_room, {Muc_name,Conference_server}) of
	[] ->
		http_utils:gen_result(true, <<"0">>,<<"no muc to migrate.">>);	
	[Muc] ->
		case force_migrate_muc_by_name(Server,Muc) of
		true ->
			http_utils:gen_result(true, <<"0">>,<<"migrate muc sucess.">>);
		_ ->
			http_utils:gen_result(true, <<"0">>,<<"migrate muc error.">>)
		end
	end.


do_migrate_muc_by_name(Server,Muc) ->
	Pid = Muc#muc_online_room.pid,
	{ok,Num } = gen_fsm:sync_send_all_state_event(Pid,get_user_num),
	case Num of 
	0 ->
		case node(Pid) =/= node() of
		true ->
			gen_fsm:send_all_state_event(Pid, {stop, <<"management close">>}),
			{Muc_name,ServerHost} = Muc#muc_online_room.name_host,
			do_start_muc(Server,ServerHost,Muc_name),
			true;
		_ ->
			false
		end;
	_ ->
		false
	end.

force_migrate_muc_by_name(Server,Muc) ->
	Pid = Muc#muc_online_room.pid,
	case node(Pid) =/= node() of
	true ->
		gen_fsm:send_all_state_event(Pid, {stop, <<"management close">>}),
		{Muc_name,ServerHost} = Muc#muc_online_room.name_host,
		do_start_muc(Server,ServerHost,Muc_name),
		true;
	_ ->
		false
	end.

close_invaild_session() ->
        lists:foreach(fun(S) ->
                Sid = S#session.sid,{U,S1,R} = S#session.usr,
                {_,Pid} = Sid ,
                case check_pid_alive(Pid) of
				true ->
                	ok;
	            _ ->
	                   ejabberd_sm:close_session(Sid,U,S1,R)
	            end  end,ets:tab2list(session)).

clear_s2s_session() ->
	 L = ets:tab2list(s2s),
	 lists:foreach(fun(S) -> F = fun() -> mnesia:delete_object(S) end,mnesia:transaction(F) end,L).

check_pid_alive(Pid) ->
	if node() =:= node(Pid) ->
		is_process_alive(Pid);
	true ->
		true
	end.

check_invaild_session(Server) ->
	L = mnesia:dirty_select(session, [{#session{usr = '$1', _ = '_'},[{'==', {element, 2, '$1'}, Server}], ['$1']}]),
	lists:foreach(fun({U,S,R}) ->
				case ejabberd_sm:get_session_pid(U,S,R) of
				none ->
					?INFO_MSG("get invaild session U ~p ,S ~p ,R ~p ~n",[U,S,R]);
				_ ->
					ok
				end end,L).

send_check_msg() ->
        lists:foreach(fun(S) ->
                Sid = S#session.sid,{U,S1,R} = S#session.usr,
                {_,Pid} = Sid ,
                Pid ! test end,ets:tab2list(session)).

%%--------------------------------------------------------------------
%% @date 2017-02=06
%% 添加群成员
%% qunar.com
%%--------------------------------------------------------------------

add_muc_users(Server,Args) ->
	case proplists:get_value("muc",Args) of
    <<"">> ->
       http_utils:gen_result(false, <<"1">>,<<"add_muc_user failed,no found muc">>); 
    Muc ->
        case proplists:get_value("users",Args,<<"">>) of
        <<"">> ->
            http_utils:gen_result(false, <<"1">>,<<"add_muc_user failed,no found user">>);
        Users ->
            lists:foreach(fun({obj,[{"user",User}]}) ->
                case catch ejabberd_odbc:sql_query(Server,
                    [<<"insert into user_register_mucs (username,muc_name,domain,registed_flag) values ('">>,User,<<"','">>,
                         Muc,<<"','conference.ejabhost1','1');">>]) of
                {updated,1} ->
                        ok;
                Res ->
                    ?INFO_MSG("insert user_register_mucs res ~p ~n",[Res])
                end,
                case catch ejabberd_odbc:sql_query(Server,
                    [<<"insert into muc_room_users (username,muc_name,host,login_date) values ('">>,User,<<"','">>,
                            Muc,<<"','ejabhost1','1459820477');">>]) of
                {updated,1} ->
                        ok;
                Res1 ->
                    ?INFO_MSG("insert muc_room_users res ~p ~n",[Res1])
                end,
                do_stop_muc(Muc,<<"conference.ejabhost1">>) end, Users),
            http_utils:gen_result(true, <<"0">>,<<"add user sucess">>)
        end
    end.

            
