-module(clear_expire_muc).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").

-export([clear_expired_muc/2,get_res/2]).
-export([get_permanent_muc_name_and_time/1]).

-record(muc_online_room,
          {name_host = {<<"">>, <<"">>} :: {binary(), binary()} | '$1' |{'_', binary()} | '_', pid = self() :: pid() | '$2' | '_' | '$1'}).


get_muc_expire_date(Server,Stand) ->
	Dict = 
		lists:foldl(fun({Min_mucs,Max_mucs,Days},Acc) ->
			case catch ejabberd_odbc:sql_query(Server,
				[<<"select distinct(muc_name) from user_register_mucs where username in (
						select username from user_register_mucs group by username 
						having count(muc_name) > ">>,http_utils:to_binary(Min_mucs,<<"200">>),
						<<" and count(muc_name) < ">>,http_utils:to_binary(Max_mucs,<<"500">>),<<") ;">>]) of
			{selected, _ , SRes} when is_list(SRes) ->
					lists:foldl(fun([M],Acc1) ->
						  dict:store(M,Days,Acc1) end,Acc,SRes);
			_ ->
				Acc
			end end,dict:new(),Stand),
	White_list_mucs = 
		case catch ejabberd_odbc:sql_query(Server,
			[<<"select muc_name from muc_white_list;">>] ) of
		 {selected, _ , SRes} when is_list(SRes) ->
				SRes;
		_ ->
			[]
		end,
	lists:foldl(fun([Muc],Acc) ->
				dict:erase(Muc,Acc) end,Dict,White_list_mucs).

get_res(Server,Stand) ->
	Dict = get_muc_expire_date(Server,Stand) ,
	?DEBUG("Res ~p ~n",[dict:to_list(Dict)]).
			
get_permanent_muc_name_and_time(Server) ->
	LServer = jlib:nameprep(Server),
	case catch ejabberd_odbc:sql_query(LServer,
			[<<"select r.name, extract(epoch from date_trunc('second', r.created_at)) from muc_room r join muc_room a on r.name = a.name
			 and r.name not in (select muc_name from muc_white_list)">>]) of
	{selected,_,SRes} when is_list(SRes)	->
		lists:foldl(fun([M,T],Acc) ->
			dict:store(M,T,Acc) end,dict:new(),SRes);
	_ ->
		dict:new()
	end.


get_muc_create_time(Server,Room,Dict,Now) ->
	Create_time = 
		case catch dict:find(Room,Dict) of
		{ok,T} when is_binary(T) ->
			binary_to_integer(T);
		_ ->
			Now
		end,
	Last_time = 
		case catch odbc_queries:get_muc_msg_last_timestamp(Server,Room) of 
		{selected,_,[[Timestamp]]} ->
			Msg_time = binary_to_integer(Timestamp),
			case Create_time > Msg_time  of
			true ->
				Create_time;
			false ->
				Msg_time
			end;
		{selected,_,[]} ->
				Create_time;
		Reason ->
			?ERROR_MSG("Get Muc last timestamp error,Reason is ~p ~n", [Reason]),
			Create_time
		end,
	Now - Last_time.
			
%%--------------------------------------------------------------------
%% @date 2015-07-02
%% 聊天室警告删除信息
%%--------------------------------------------------------------------
clear_expired_muc(Server,Stand) ->
	LServer = jlib:nameprep(Server),
	ServerHost =  str:concat(<<"conference.">>,Server),
	Now = mod_time:get_timestamp(),
	Dict  = get_muc_expire_date(Server,Stand),	
	Dict1 = get_permanent_muc_name_and_time(Server),
	lists:foreach(fun({Room,Day}) ->
			Time_diff = get_muc_create_time(Server,Room,Dict1,Now),
			timer:sleep(100),
			send_muc_notice(LServer,Time_diff,Day,Room,ServerHost) 
			end,dict:to_list(Dict)).

send_muc_notice(LServer,Time_diff,Day,Room,ServerHost) ->
	if Time_diff  > 86400*(Day-3) ->
		case mnesia:dirty_read(muc_online_room, {Room,ServerHost}) of
		[] ->
			if Time_diff > 86400*Day ->
				destroy_muc_room(<<"">>,LServer,Room,ServerHost);
			true ->
				ok
			end;
		[M] ->
			Pid = M#muc_online_room.pid,
			if Time_diff > 86400*Day ->
				destroy_muc_room(Pid,LServer,Room,ServerHost);
			true ->
				ok
			end,
			if Time_diff > 86400*(Day-1) andalso Time_diff < 86400*Day ->
				send_muc_service_msg(LServer,Room,Pid,<<"通知：群将于1天后销毁，如需保留，请进入该群，输入任意信息!回复通知账号无效！！"/utf8>>);
			true ->
					ok
			end,
			if Time_diff > 86400*(Day-2) andalso Time_diff < 86400*(Day-1) ->
				send_muc_service_msg(LServer,Room,Pid,<<"通知：群将于2天后销毁，如需保留，请进入该群,输入任意信息!回复通知账号无效！！"/utf8>>);
			true ->
				ok
			end,
			if Time_diff > 86400*(Day-3) andalso Time_diff < 86400*(Day-4) ->
				send_muc_service_msg(LServer,Room,Pid,<<"通知：群将于3天后销毁，如需保留，请进入该群,输入任意信息!回复通知账号无效！！"/utf8>>);
			true ->
				ok
			end
		end;
	true ->
		ok
	end.

send_muc_service_msg(Server,Muc,Pid,Msg) ->
	case catch  ejabberd_odbc:sql_query(Server,
		 [<<"select show_name from muc_vcard_info where muc_name = '">>,Muc,<<"@conference.ejabhost1';">>]) of
	{selected,[<<"show_name">>],[[Show]]} when is_binary(Show) ->
		Warn_Msg = list_to_binary(binary_to_list(Msg) ++ " ,Room name : " ++ binary_to_list(Show) ++ " , Room id :" ++ binary_to_list(Muc)),
		?INFO_MSG("Send notie ~p ~p ~n",[Pid,Warn_Msg]),
		send_muc_warn_msg(Server,Muc,Warn_Msg);
	%	gen_fsm:send_all_state_event(Pid, {service_message, Warn_Msg});
	INFO ->
%%		Warn_Msg = iolist_to_binary(binary_to_list(Msg) ++ " ,Room name : " ++ binary_to_list(Muc) ++ " , Room id :" ++ binary_to_list(Muc)),
%		?INFO_MSG("Send notie ~p ~p ~n",[Pid,Warn_Msg]),
		?INFO_MSG("Get Muc ~p name Error,Info ~p ~n",[Muc,INFO])
%		send_muc_warn_msg(Server,Muc,Warn_Msg)
	end.

destroy_muc_room(Pid,LServer,Room,ServerHost) ->
	catch log_destroy_room_info(LServer,Room,ServerHost),
	case Pid of 
	<<"">> ->
		mod_muc:forget_room(LServer,ServerHost, Room),
		ok;
	P when is_pid(P)  ->
		gen_fsm:send_all_state_event(Pid, {destroy, <<"management close">>}),
		mod_muc:room_destroyed(LServer, Room,Pid, ServerHost),
		mod_muc:forget_room(LServer, ServerHost,Room)
	end,
	?INFO_MSG("90 Days left,but no message ,Del Muc Room ~s ~n",[Room]),

	catch odbc_queries:restore_muc_user_mark(LServer,Room),
	catch odbc_queries:del_muc_users(LServer,<<"muc_room_users">>,Room),
	catch odbc_queries:delete_muc_last(LServer,Room),
	catch odbc_queries:del_user_register_mucs(LServer,Room),
	catch odbc_queries:del_muc_vcard_info(LServer,Room,<<"Day check Destroy">>).

send_muc_warn_msg(Server,Room,Msg) ->
    MessagePkt = ejabberd_public:make_message_packet(<<"chat">>,Msg,<<"">>,undefined),
%		#xmlel{name = <<"message">>,
%			attrs = [{<<"type">>, <<"chat">>}],
%			children =
%			    [#xmlel{name = <<"body">>, attrs = [],
%				    children = [{xmlcdata, Msg}]}]},
	
	Users = 
		case catch ejabberd_odbc:sql_query(Server,
			[<<"select username from muc_room_users where muc_name = '">>,Room,<<"';">>]) of
		{selected,[<<"username">>],Res} when is_list(Res) ->
			Res;
		_ ->
			[]
		end,
    lists:foreach(
		fun([U]) ->
	      ejabberd_router:route(
			jlib:make_jid(<<"Muc_recovery_warn">>,Server,<<"">>),
			jlib:make_jid(U,Server,<<"">>),
			MessagePkt)
	      end,
	  Users).

log_destroy_room_info(Server,Room,_ServerHost) ->
	case catch  ejabberd_odbc:sql_query(Server,
		 [<<"select show_name from muc_vcard_info where muc_name = '">>,Room,<<"@conference.ejabhost1';">>]) of
	{selected,[<<"show_name">>],[[Show]]} when is_binary(Show) ->
		?INFO_MSG("Log Destroy Muc name ~s ,Muc id ~p ~n",[Show,Room]);
	Info ->
		?INFO_MSG("Log Destroy Muc ~p error,SQL retrun error ~p ~n",[Room,Info]),
		?INFO_MSG("Log Destroy Muc name ~s ,Muc id ~p ~n",[Room,Room])
	end,
	case catch  ejabberd_odbc:sql_query(Server,
		 [<<"select username from muc_room_users where muc_name = '">>,Room,<<"';">>]) of
	{selected,_,Res} when is_list(Res) ->
		R = lists:concat(Res),
		Users = lists:zipwith(fun(L1,L2) -> [L1,L2]   end,R,lists:duplicate(length(R)-1,<<",">>)++ [<<"">>]),
		?INFO_MSG("Log Destroy Muc id ~p,Users ~p  ~n",[Room,list_to_binary(lists:concat(Users))]);
	Info1 ->
		?INFO_MSG("Log Destroy Muc ~p error,SQL retrun error ~p ~n",[Room,Info1])
	end.

