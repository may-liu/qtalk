-module(mod_day_check).

-behaviour(gen_server).
-behaviour(gen_mod).

-export([start/2,stop/1,start_link/2,init/1]).

-export([handle_call/3, handle_cast/2,
 	    handle_info/2, terminate/2, code_change/3]).

-export([find_max_memory_process/0]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("ejabberd_extend.hrl").

-define(SERVER, ?MODULE).

-record(state,{server,day_timer}).


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
    	    erlang:start_timer(10000,self(),first_day_check);
		End_timestamp ->
			Count_time = End_timestamp - Timestamp,
    	    erlang:start_timer(Count_time*1000,self(),first_day_check)
    	    %%erlang:start_timer(Count_time,self(),first_day_check)
		end,
	{ok, #state{server = Server,day_timer = Tref_day}}.


handle_call(stop, _From, State=#state{day_timer = Day_tref}) ->
    {ok, cancel} = timer:cancel(Day_tref),
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, _TimerRef, first_day_check},State=#state{server = Server}) ->
	New_day_tref = erlang:start_timer(86400*1000,self(),day_check),
	check_muc_last(Server),
	clear_muc_users_dimission(Server),
	NewState = State#state{day_timer = New_day_tref},
	{noreply, NewState};
handle_info({timeout, _TimerRef, day_check},State=#state{server = Server}) ->
	New_day_tref = erlang:start_timer(86400*1000,self(),day_check),
	check_muc_last(Server),
	clear_muc_users_dimission(Server),
	NewState = State#state{day_timer = New_day_tref},
	{noreply,NewState};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

clear_muc_users_dimission(Server) ->
	LServer = jlib:nameprep(Server),
	catch odbc_queries:clear_muc_users(LServer),
	catch odbc_queries:clear_spool(LServer),
	catch odbc_queries:clear_user_mac_key(LServer),
	catch odbc_queries:clear_muc_spool(LServer).

get_muc_name_and_time(Server) ->
    LServer = jlib:nameprep(Server),
    case catch ejabberd_odbc:sql_query(LServer,
		[<<"select r.name, extract(epoch from date_trunc('second', r.created_at)) from muc_room r join muc_room a on r.name = a.name ">>]) of
%%				and r.name not in (select muc_name from muc_white_list)">>]) of
		{selected,_,SRes} when is_list(SRes)    ->
			SRes;   
		_ ->    
			[]
		end.

get_proc_name(Host) ->
    gen_mod:get_module_proc(Host, ?MODULE).


check_muc_last(Server) ->
	LServer = jlib:nameprep(Server),
	Now = mod_time:get_timestamp(),
	ServerHost =  str:concat(<<"conference.">>,Server),
	lists:foreach(
			fun([Room,CreateTime]) ->
				IntegerCreTime = 
					case CreateTime of
					null ->
						Now;
					_ ->
						binary_to_integer(CreateTime)
					end,	
				LastTime = 
					case catch odbc_queries:get_muc_msg_last_timestamp(LServer,Room) of 
					{selected,[<<"m_timestamp">>],[[Timestamp]]} ->
						IntegerMsgTime = binary_to_integer(Timestamp),
						case IntegerCreTime > IntegerMsgTime of
						true ->
							IntegerCreTime;
						false ->
							IntegerMsgTime
						end;
					{selected,[<<"m_timestamp">>],[]} ->
						IntegerCreTime;
					Reason ->
						?ERROR_MSG("Get Muc last timestamp error,Reason is ~p ~n", [Reason]),
						IntegerCreTime
					end,
				Time_diff = Now - LastTime,
				if Time_diff  > 86400*14 ->
					case mnesia:dirty_read(muc_online_room, {Room,ServerHost}) of
					[] ->
						ok;
					[M] ->
						Pid = M#muc_online_room.pid,
						mod_muc:room_destroyed(LServer, Room,Pid, ServerHost),
						mod_muc:forget_room(ServerHost, LServer, Room)
					end,
					?INFO_MSG("14 Days left,but no message ,Del Muc Room ~s ~n",[Room]),
					catch odbc_queries:del_muc_users(LServer,<<"muc_room_users">>,Room),
					catch odbc_queries:del_muc_vcard_info(LServer,Room);
				true ->
					ok
				end
			end,get_muc_name_and_time(LServer)).

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
