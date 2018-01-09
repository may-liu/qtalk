-module(mod_day_check).

-behaviour(gen_server).
-behaviour(gen_mod).

-export([start/2,stop/1,start_link/2,init/1]).

-export([handle_call/3, handle_cast/2,
 	    handle_info/2, terminate/2, code_change/3]).

-export([find_max_memory_process/0,do_upate_elastic_search_set/3,send_muc_service_msg/4,clear_muc_users_dimission/1]).
-export([destroy_muc_room/4,log_destroy_room_info/3]).
-export([kick_dimission_mucs/2,clear_user_invalid_redis_key/3,clear_invalid_redis_key/1,clear_live_not_active_muc/1]).


-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").

-define(SERVER, ?MODULE).

-record(state,{server,day_timer,three_day_timer}).

-record(muc_online_room,
		   {name_host = {<<"">>, <<"">>} :: {binary(), binary()} | '$1' |{'_', binary()} | '_', pid = self() :: pid() | '$2' | '_' | '$1'}).


start(Host,Opts) ->
	Proc = get_proc_name(Host),
	ChildSpec = {Proc,{?MODULE, start_link, [Host,Opts]}, temporary,1000,worker,[?MODULE]},
	{ok,_Pid} = supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
	Proc = get_proc_name(Host),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).

start_link(Server,Opts) ->
	gen_server:start_link({local, get_proc_name(Server)}, ?MODULE, [Server,Opts], []).

init([Server,_Opts]) ->
	Timestamp = mod_time:get_timestamp(),
	Tref_day = 
		case mod_time:get_timestamp_of_end_day(Timestamp) of
		0 ->
    	    erlang:start_timer(10000,self(),first_day_sum);
		End_timestamp ->
			Count_time = End_timestamp - Timestamp+43200,
    	    erlang:start_timer(Count_time*1000,self(),first_day_sum)
		end,
	Three_day_Tref = erlang:start_timer(86400*3*1000,self(),three_day_check),
	{ok, #state{server = Server,day_timer = Tref_day,three_day_timer = Three_day_Tref}}.


handle_call(stop, _From, State=#state{day_timer = Day_tref}) ->
    {ok, cancel} = timer:cancel(Day_tref),
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
	
handle_info({timeout, TimerRef, first_day_sum},State=#state{server = Server}) ->
	case erlang:read_timer(TimerRef) of
	false ->
		New_day_tref = erlang:start_timer((86400 + 3600*7)*1000,self(),day_sum),
	%	catch  clear_expire_muc:clear_expired_muc(Server,[{0,150,90},{150,250,75},{250,400,60},{400,1000,45}]),
		catch clear_muc_users_dimission(Server),
		lists:foreach(fun(Node) ->
				erlang:send({'mod_update_v2',Node},update_user_info) end,nodes()),
		clear_redis_keys(Server,0),
		record_redis_info(Server),
		clear_out_date_invite(Server),
		NewState = State#state{day_timer = New_day_tref},
		{noreply, NewState};
	_ ->
	%	catch clear_expire_muc:clear_expired_muc(Server,[{0,150,90},{150,250,75},{250,400,60},{400,1000,45}]),
		catch clear_muc_users_dimission(Server),
		clear_redis_keys(Server,0),
		record_redis_info(Server),
		clear_out_date_invite(Server),
		{noreply, State}
	end;
handle_info({timeout, TimerRef, day_sum},State=#state{server = Server}) ->
	case  erlang:read_timer(TimerRef) of
	false ->
		New_day_tref = erlang:start_timer(86400*1000,self(),day_sum),
		catch clear_expire_muc:clear_expired_muc(Server,[{0,150,90},{150,250,75},{250,400,60},{400,1000,45}]),
	%%	check_muc_last(Server),
		catch clear_muc_users_dimission(Server),
		clear_redis_keys(Server,0),
		record_redis_info(Server),
		clear_out_date_invite(Server),
        clear_live_not_active_muc(Server),
		NewState = State#state{day_timer = New_day_tref},
		{noreply,NewState};
	_ ->
	%%	check_muc_last(Server),
		New_day_tref = erlang:start_timer(86400*1000,self(),day_sum),
		catch clear_expire_muc:clear_expired_muc(Server,[{0,150,90},{150,250,75},{250,400,60},{400,1000,45}]),
		catch clear_muc_users_dimission(Server),
		clear_redis_keys(Server,0),
		record_redis_info(Server),
		clear_out_date_invite(Server),
        clear_live_not_active_muc(Server),
        NewState = State#state{day_timer = New_day_tref},
		{noreply,NewState}
	end;
handle_info({timeout, TimerRef, three_day_timer},State=#state{server = Server}) ->
	clear_invalid_redis_key(Server),
	case  erlang:read_timer(TimerRef) of
	false ->
		New_3day_tref = erlang:start_timer(86400*3*1000,self(),three_day_check),
		NewState = State#state{three_day_timer = New_3day_tref},
		{noreply,NewState};
	_ ->
		{noreply,State}
	end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @date 2015-07-02
%% 清理离职人员信息
%%--------------------------------------------------------------------
clear_muc_users_dimission(Server) ->
	LServer = jlib:nameprep(Server),
	kick_dimission_mucs(LServer,3), 
%	catch ejabberd_odbc:sql_query(LServer, 
%			[<<"delete from user_register_mucs a where a.username not in (select username from users where hire_flag = 1);">>]),
%	catch odbc_queries:clear_muc_users(LServer),
	catch odbc_queries:clear_spool(LServer),
	catch odbc_queries:clear_user_mac_key(LServer),
	catch odbc_queries:clear_muc_spool(LServer),

	catch clear_reids_info(Server).


%%--------------------------------------------------------------------
%% @date 2015-07-02
%% 获取聊天室创建时间
%%--------------------------------------------------------------------
get_muc_name_and_time(Server) ->
	LServer = jlib:nameprep(Server),
	case catch ejabberd_odbc:sql_query(LServer,
			[<<"select r.name, extract(epoch from date_trunc('second', r.created_at)) from muc_room r join muc_room a on r.name = a.name
			 and r.name not in (select muc_name from muc_white_list)">>]) of
	{selected,_,SRes} when is_list(SRes)	->
		SRes;
	_ ->
		[]	
	end.

get_proc_name(Host) ->
    gen_mod:get_module_proc(Host, ?MODULE).

%%--------------------------------------------------------------------
%% @date 2015-07-02
%% 聊天室警告删除信息
%%--------------------------------------------------------------------
check_muc_last(Server) ->
	LServer = jlib:nameprep(Server),
	Now = mod_time:get_timestamp(),
	lists:foreach(
			fun([Room,CreateTime]) ->
				ServerHost =  str:concat(<<"conference.">>,Server),
				IntegerCreTime = 
					case CreateTime of
					null ->
						Now;
					_ ->
						binary_to_integer(CreateTime)
					end,	
				LastTime = 
					case catch odbc_queries:get_muc_msg_last_timestamp(LServer,Room) of 
					{selected,_,[[Timestamp]]} ->
						IntegerMsgTime = binary_to_integer(Timestamp),
						case IntegerCreTime > IntegerMsgTime of
						true ->
							IntegerCreTime;
						false ->
							IntegerMsgTime
						end;
					{selected,_,[]} ->
						IntegerCreTime;
					Reason ->
						?ERROR_MSG("Get Muc last timestamp error,Reason is ~p ~n", [Reason]),
						IntegerCreTime
					end,
				Time_diff = Now - LastTime,
				if Time_diff  > 86400*87 ->
					case mnesia:dirty_read(muc_online_room, {Room,ServerHost}) of
					[] ->
						if Time_diff > 86400*90 ->
							destroy_muc_room(<<"">>,LServer,Room,ServerHost);
						true ->
							ok
						end;
					[M] ->
						Pid = M#muc_online_room.pid,
						if Time_diff > 86400*90 ->
							destroy_muc_room(Pid,LServer,Room,ServerHost);
						true ->
							ok
						end,
						if Time_diff > 86400*89 andalso Time_diff < 86400*90 ->
							send_muc_service_msg(LServer,Room,Pid,"通知：群将于1天后销毁，如需保留，请进入该群，输入任意信息!回复通知账号无效！！");
						true ->
							ok
						end,
						if Time_diff > 86400*88 andalso Time_diff < 86400*89 ->
							send_muc_service_msg(LServer,Room,Pid,"通知：群将于2天后销毁，如需保留，请进入该群,输入任意信息!回复通知账号无效！！");
						true ->
							ok
						end,
						if Time_diff > 86400*87 andalso Time_diff < 86400*88 ->
							send_muc_service_msg(LServer,Room,Pid,"通知：群将于3天后销毁，如需保留，请进入该群,输入任意信息!回复通知账号无效！！");
						true ->
							ok
						end
					end;
				true ->
					ok
				end
			end,get_muc_name_and_time(LServer)).

%%--------------------------------------------------------------------
%% @date 2015-07-02
%% 获取最大的内存队列
%%--------------------------------------------------------------------
find_max_memory_process() ->
    %%进程列表
    ProcessL = processes() -- [self()],
    %%获取进程信息，
    AttrName = memory,
    F = fun(Pid, L) ->  
    	case process_info(Pid, [AttrName, registered_name, current_function, initial_call]) of
        [Attr, Name, Init, Cur] ->
			Info = {Attr, [{pid, Pid}, Name, Init, Cur]},
	  		[Info | L]; 
        undefined ->
	         L   
	    end 
	 end,
	ProInfoL = lists:foldl(F, [], ProcessL),
	 %%排序
	CompF = fun({A, _},{B, _}) ->  A > B     end,
	ProInfoSortL = lists:usort(CompF, ProInfoL),
	%%取前10个
	Num = 10, 
	lists:sublist(ProInfoSortL, Num).

judge_month_info() ->
	{{Year,Month_after,_},{_,_,_}}  =  mod_time:timestamp_to_datetime(mod_time:get_timestamp() + 24*3600*1),
	{{_,Month,_},{_,_,_}} = mod_time:timestamp_to_datetime(mod_time:get_timestamp()),
	case Month_after =:= Month of
	true ->
		{true,Year,Month_after};
	false ->
		false
	end.

update_elastic_search_set(Server) ->
	case judge_month_info() of
	false ->
		ok;
	{true,Year,Month} ->
		do_upate_elastic_search_set(Server,Year,Month);
	_ ->
		ok
	end.

do_upate_elastic_search_set(Server,Year,Month) ->
	Body = rfc4627:encode(make_set_json()),
	Es_Url = "http://xxxxxxxxxx.com:9200/",
	Url = Es_Url ++ "message_" ++ integer_to_list(Year) ++ "_" ++ integer_to_list(Month),
	Header = [],
	Type = "application/json",
	HTTPOptions = [],
	Options = [],
	case http_client:http_post(Server,Url,Header,Type,Body,HTTPOptions,Options) of
	{ok, {_Status,_Headers, Res}} ->
		case rfc4627:decode(Res) of
		{ok,{obj,[{"acknowledged",true}]},_} ->
			?INFO_MSG("Update elastic_search set index message_~p_~p sucess",[Year,Month]);
		Reason ->
			?INFO_MSG("Update elastic_search set index message_~p_~p failed,Reason ~p",[Year,Month,Reason])
		end;
	_ ->
		ok
	end.

make_set_json() ->
	Raw = {obj,[{"type",<<"string">>},{"index",<<"not_analyzed">>}]},
	Fields1 = {obj,[{"raw",Raw}]},
	Propertie1 = {obj,[{"type",<<"string">>},{"analyzer",<<"standard">>},{"fields",Fields1}]},
	ID = {obj,[{"type",<<"long">>},{"index",<<"not_analyzed">>}]},
	Propertie_msg = {obj,[{"from",Propertie1},{"to",Propertie1},{"id",ID},
			{"body",{obj,[{"type",<<"string">>},{"index_analyzer",<<"index_ngram">>},{"search_analyzer",<<"search_ngram">>}]}}]},
	Msg = {obj,[{"properties",Propertie_msg}]},
	
	
	Propertie_muc_msg = {obj,[{"muc",Propertie1},{"nick",Propertie1},{"id",ID},
			{"body",{obj,[{"type",<<"string">>},{"index_analyzer",<<"index_ngram">>},{"search_analyzer",<<"search_ngram">>}]}}]},
	Muc_msg = {obj,[{"properties",Propertie_muc_msg}]},

	Mappings = {obj,[{"message",Msg},{"muc_msg",Muc_msg}]},

	Search_ngram = {obj,[{"type",<<"custom">>},{"tokenizer",<<"standard">>},{"filter",<<"lowercase">>}]},
	Index_ngram  = {obj,[{"type",<<"custom">>},{"tokenizer",<<"standard">>},{"filter",[<<"msg_ngram">>,<<"lowercase">>]}]},
	Analyzer = {obj,[{"index_ngram",Index_ngram},{"search_ngram",Search_ngram}]},

	Msg_ngram = {obj,[{"type",<<"ngram">>},{"min_gram",1},{"max_gram",20}]},
	Filter = {obj,[{"msg_ngram",Msg_ngram}]},

	Analysis = {obj,[{"filter",Filter},{"analyzer",Analyzer}]},

	Settings = {obj,[{"analysis",Analysis}]},
	
	{obj,[{"mappings",Mappings},{"settings",Settings}]}.

record_redis_info(LServer) ->
	case catch 	ejabberd_odbc:sql_query(LServer,
			[<<"select username,name from users">>]) of
	{selected, _ , SRes} when is_list(SRes) ->
		lists:foreach(fun([Key,Val]) ->
			case redis_link:str_set(LServer,3,Key,Val) of
			{ok,<<"OK">>} ->
				catch redis_link:expire_time(LServer,3,Key,86400*3);
			_ ->
				ok
			end end,SRes);
	_ ->
		ok
	end,
	case catch 	ejabberd_odbc:sql_query(LServer,
			[<<"select muc_name,show_name from muc_vcard_info">>]) of
	{selected, _ , SRes1} when is_list(SRes1) ->
		lists:foreach(fun([Key,Val]) ->
			case redis_link:str_set(LServer,3,Key,Val) of
			{ok,<<"OK">>} ->
				catch redis_link:expire_time(LServer,3,Key,86400*3);
			_ ->
				ok
			end end,SRes1);
	_ ->
		ok
	end.

send_muc_service_msg(Server,Muc,Pid,Msg) ->
	case catch  ejabberd_odbc:sql_query(Server,
		 [<<"select show_name from muc_vcard_info where muc_name = '">>,Muc,<<"@conference.ejabhost1';">>]) of
	{selected,[<<"show_name">>],[[Show]]} when is_binary(Show) ->
		Warn_Msg = list_to_binary(Msg ++ " ,Room name : " ++ binary_to_list(Show) ++ " , Room id :" ++ binary_to_list(Muc)),
		?INFO_MSG("Send notie ~p ~p ~n",[Pid,Warn_Msg]),
		send_muc_warn_msg(Server,Muc,Warn_Msg);
	%	gen_fsm:send_all_state_event(Pid, {service_message, Warn_Msg});
	INFO ->
		Warn_Msg = list_to_binary( Msg ++ " ,Room name : " ++ binary_to_list(Muc) ++ " , Room id :" ++ binary_to_list(Muc)),
		?INFO_MSG("Send notie ~p ~p ~n",[Pid,Warn_Msg]),
		?INFO_MSG("Get Muc ~p name Error,Info ~p ~n",[Muc,INFO]),
		send_muc_warn_msg(Server,Muc,Warn_Msg)
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

clear_redis_keys(Server,Table) ->
	case catch odbc_queries:list_users(Server) of
	{selected, _, Users} when is_list(Users) ->
		case redis_link:get_all_keys(Server,Table) of
		{ok,Keys} ->
			lists:foreach(fun(Key) ->
				case str:str(Key,<<".">>) =/= 0 andalso str:str(Key,<<"_">>) =/= 0 of 
				true -> 
					User = lists:nth(1, str:tokens(Key,<<"_">>)),
					case lists:member([User],Users) of
					true ->
						check_expire_time(Server,Table,Key);
					_ ->
						catch redis_link:str_del(Server,Table,Key)
					end;
				false ->
					ok
				end end,Keys);
		_ ->
			ok
		end;
	_ ->
		?INFO_MSG("Now ~p get all keys Error",[mod_time:get_timestamp()])
	end.

check_expire_time(Server,Table,Key) ->
	case catch redis_link:ttl_key(Server,Table,Key) of
	{ok,<<"-1">>} ->
		catch redis_link:expire_time(Server,Table,Key,86400*6);
	_ ->
		ok
	end.

clear_out_date_invite(Server) ->
	Time = mod_time:get_timestamp() - 86400*7,
	catch ejabberd_odbc:sql_query(Server,
		[<<"delete from invite_spool where timestamp < ">>,integer_to_binary(Time)]).

send_muc_warn_msg(Server,Room,Msg) ->
    MessagePkt =
		#xmlel{name = <<"message">>,
			attrs = [{<<"type">>, <<"chat">>}],
			children =
			    [#xmlel{name = <<"body">>, attrs = [],
				    children = [{xmlcdata, Msg}]}]},
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

kick_dimission_mucs(Server,Min_Version) ->
	case catch ejabberd_odbc:sql_query(Server,
		[<<"select username,muc_name from muc_room_users where username in 
				(select username from users where version > (select max(version) - ">>,
			integer_to_binary(Min_Version),<<" from users ) and hire_flag = 0); ">>]) of
	{selected,[<<"username">>,<<"muc_name">>],Res} when is_list(Res) ->
		lists:foreach(fun([U,M]) ->
			case jlib:make_jid(U,Server,<<"">>) of		 
			error ->
				?INFO_MSG("Make User Jid Error ~p ~n",[U]);
			JID ->
				ServerHost =  str:concat(<<"conference.">>,Server),
				case mnesia:dirty_read(muc_online_room, {M,ServerHost}) of
				[] ->
					catch ejabberd_public:clear_ets_muc_room_users(M,U,Server),	
				    catch odbc_queries:del_muc_user(Server,<<"muc_room_users">>,M,U),
		            catch ejabberd_odbc:sql_query(Server,
					   	[<<"delete from user_register_mucs where username = '">>,U,<<"' and muc_name = '">>,M,<<"';">>]);
				[Muc] ->
					?INFO_MSG("Remove dimission User ~p ,Muc ~p ~n",[U,M]),
					Muc#muc_online_room.pid ! {http_del_user,JID}
				end
			end end,Res);
	_ ->
		ok
	end.

clear_reids_info(Server) ->
	Flag = random:uniform(2) =:= 2,
	case catch  ejabberd_odbc:sql_query(Server,[<<"select hire_flag,username from users ;">>]) of
	{selected,[<<"hire_flag">>,<<"username">>],Res} when is_list(Res) ->
			lists:foreach(fun([Hf,U]) ->
                case catch ejabberd_public:to_integer(Hf) of
				HF when HF > 0 -> 
			        case ejabberd_sm:get_user_resources(U,Server)  of
					[] ->
						clear_user_all_redis_key(false,Server,U);
					L when is_list(L) ->
						clear_user_invalid_redis_key(Server,U,L);
					_ ->
						ok
					end;
				0 ->
					clear_user_all_redis_key(true,Server,U),
                    management_cmd:do_kick_user(U,Server);
                _ ->
                    ok
				end end,Res);
		_ ->
			ok
		end.	

clear_invalid_redis_key(Server) ->
    case catch  ejabberd_odbc:sql_query(Server,[<<"select hire_flag,username from users whre hire_flag > 0;">>]) of
    {selected,[<<"hire_flag">>,<<"username">>],Res} when is_list(Res) ->
    	lists:foreach(fun([_Hf,U]) ->
				case ejabberd_sm:get_user_resources(U,Server)  of
				[] ->
					clear_user_all_redis_key(false,Server,U);
				L when is_list(L) ->
					clear_user_invalid_redis_key(Server,U,L);
				_ ->
					ok
				end
			end,Res);
	_ ->
		ok
	end.

clear_user_all_redis_key(true,Server,U) ->
	catch redis_link:str_del(Server,5,U),
	clear_user_all_redis_key(false,Server,U);
clear_user_all_redis_key(_,Server,U) ->
	catch redis_link:str_del(Server,1,U),
	catch redis_link:str_del(Server,2,U),
	catch redis_link:str_del(Server,2,str:concat(U,<<"_tkey">>)).

clear_user_invalid_redis_key(Server,U,Rs) ->
	case catch redis_link:redis_cmd(Server,1,["HKEYS", U]) of
	{ok,Keys} ->
	 	Keys1 = Keys -- Rs,
		lists:foreach(fun(R) ->
			catch redis_link:hash_del(Server,1,U,R) end,Keys1);
	_ ->
		[]
	end,
	case catch redis_link:redis_cmd(Server,1,["HVALS", U]) of
	{ok,L} when is_list(L) ->
		case catch  redis_link:redis_cmd(Server,2,["HKEYS", U]) of
		{ok,L1} when is_list(L1) ->
			L2 = L1 -- L,
			lists:foreach(fun(Key) ->
					catch redis_link:hash_del(Server,2,U,Key) end,L2);
		  _ ->
		  	ok
		end;
	   _ ->
	   	ok
	end.


clear_live_not_active_muc(Server) ->
    Now = mod_time:get_timestamp(),
    catch ejabberd_odbc:sql_query(Server,
        [<<"delete from muc_spool where created_at <'">>,ejabberd_public:format_time(Now-3600*12),<<"';">>]),
    lists:foreach(fun(Muc) ->
       {Name,Host} = Muc#muc_online_room.name_host,
       Pid = Muc#muc_online_room.pid,
       case catch ejabberd_odbc:sql_query(Server,
            [<<"select extract(epoch from date_trunc('second', create_time)) from muc_room_history where muc_room_name = '">>,
                    Name,<<"' order by id desc limit 1;">>]) of
       {selected, _, [[T]]}  ->
            Time = binary_to_integer(T),
            case Now - Time > 3600*36 of
            true ->
               gen_fsm:send_all_state_event(Pid, {stop, <<"management close">>});
            _ ->
                ok
            end;   
       _ ->
            gen_fsm:send_all_state_event(Pid, {stop, <<"management close">>})
       end end,ets:tab2list(muc_online_room)). 
             
            

