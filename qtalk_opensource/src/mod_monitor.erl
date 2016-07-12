-module(mod_monitor).

-behaviour(gen_server).
-behaviour(gen_mod).

-export([start/2,stop/1,start_link/2,init/1]).

-export([handle_call/3, handle_cast/2,
 	    handle_info/2, terminate/2, code_change/3]).
-export([count_user_login_out/3,count_user_login_in/3,monitor_count/4,monitor_size/3,monitor_value/3,user_online_day_sum/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

-define(SERVER, ?MODULE).

-record(state,{server,minute_timer,day_timer}).
-record(online_status,{user,online_time,last_login_in,lats_login_out}).
-record(monitor_rec,{key,count=0 ,time=0}).
-record(monitor_val,{key,value}).

start(Host,Opts) ->
	Proc = get_proc_name(Host),
	create_ets_table(),
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
	case mod_time:get_timestamp_of_end_day(Timestamp) of
	0 ->
        Tref_day = erlang:start_timer(10000,self(),first_day_sum),
		Tref_day;
	End_timestamp ->
		Count_time = End_timestamp - Timestamp,
        Tref_day = erlang:start_timer(Count_time*1000,self(),first_day_sum),
		Tref_day
	end,
    Tref_Min = erlang:start_timer(60*1000,self(),minute_count),
	{ok, #state{server = Server,day_timer = Tref_day,minute_timer = Tref_Min}}.


handle_call(stop, _From, State=#state{day_timer = Day_tref,minute_timer = Min_tref}) ->
    {ok, cancel} = timer:cancel(Day_tref),
    {ok, cancel} = timer:cancel(Min_tref),
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({monitor_many,Key,Count,Time},State) ->
	case catch ets:lookup(monitor_count,Key) of 
	[] ->
		catch ets:insert(monitor_count,#monitor_rec{key = Key,count = Count,time = Time});
	[MonitorV] ->
		NewMV = #monitor_rec{key = Key,
							 count = MonitorV#monitor_rec.count + Count,
							 time =  MonitorV#monitor_rec.time + Time
							},
		catch ets:insert(monitor_count,NewMV)
	end,
	{noreply,State};
handle_cast({monitor_size,Key,Value},State) ->
	catch ets:insert(monitor_value,#monitor_val{key = Key,value = Value}),
	{noreply,State};
handle_cast({monitor_value,Key,Value},State) ->
	case catch ets:lookup(monitor_value,Key) of 
	[] ->
		catch ets:insert(monitor_value,#monitor_val{key = Key,value = Value});
	[MonitorV] ->
		NewMV = #monitor_val{key = Key,
							 value =  MonitorV#monitor_val.value + Value
							 },
		catch ets:insert(monitor_value,NewMV)
	end,
	{noreply,State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, _TimerRef, first_day_sum},State=#state{server = Server}) ->
	user_online_day_sum(Server),
	New_day_tref = erlang:start_timer(86400*1000,self(),day_sum),
	NewState = State#state{day_timer = New_day_tref},
	{noreply, NewState};
handle_info({timeout, _TimerRef, day_sum},State=#state{server = Server}) ->
	New_day_tref = erlang:start_timer(86400*1000,self(),day_sum),
	user_online_day_sum(Server),
	NewState = State#state{day_timer = New_day_tref},
	{noreply,NewState};
handle_info({timeout, _TimerRef, minute_count},State) ->
	catch monitor_size(State#state.server,<<"user_login_Value">>,ejabberd_sm:get_vh_session_number(State#state.server)),
	catch monitor_size(State#state.server,<<"muc_permanent_room_Value">>,mnesia:table_info(muc_room,size)),
	catch monitor_size(State#state.server,<<"muc_online_room_Value">>,mnesia:table_info(muc_online_room,size)),
	resave_ets_info(),			
    erlang:start_timer(60*1000,self(),minute_count),
	{noreply,State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

count_user_login_in(Host,User,_Time) ->
	note_user_login_in(User,Host).

count_user_login_out(Host,User,_Time) ->
	note_user_login_out(User,Host).

monitor_count(Host,Key,Count,Time) ->
	gen_server:cast(get_proc_name(Host), {monitor_many,Key,Count,Time}).

monitor_size(Host,Key,Value) ->
	gen_server:cast(get_proc_name(Host), {monitor_size,Key,Value}).

monitor_value(Host,Key,Value) ->
	gen_server:cast(get_proc_name(Host), {monitor_value,Key,Value}).

get_proc_name(Host) ->
	gen_mod:get_module_proc(Host, ?MODULE).

create_ets_table() ->
	catch ets:new(user_online_status,[set,named_table,public,{keypos, 2}]),
	catch ets:new(monitor_count,[set,named_table,public,{keypos,2}]),
	catch ets:new(monitor_value,[set,named_table,public,{keypos,2}]),
	catch ets:new(monitor_sum,[set,named_table,public,{keypos,2}]).

note_user_login_in(User,Server) ->
    case ejabberd_sm:get_user_resources(User,Server) of
    [] ->
        login_in_time_sum(User);
    _ ->
        ok
    end.	

login_in_time_sum(User) ->
	case ets:lookup(user_online_status,User) of
    [] ->
		Now =  mod_time:get_timestamp(),
		User_online_status =
			#online_status{user = User,online_time = 0,last_login_in = Now},
	    catch ets:insert(user_online_status,User_online_status);
    [Online_status] ->
		Now =  mod_time:get_timestamp(),
		User_online_status = Online_status#online_status{last_login_in = Now},
		catch ets:insert(user_online_status,User_online_status)
		end.

note_user_login_out(User,Server) ->
    case ejabberd_sm:get_user_resources(User,Server) of
    [] ->
        login_out_time_sum(User);
    _ ->
        ok
    end.	

login_out_time_sum(User) ->
	case ets:lookup(user_online_status,User) of
	[] ->
		User_online_status =
			#online_status{user = User,online_time = 0,last_login_in = mod_time:get_timestamp()},
		catch ets:insert(user_online_status,User_online_status);
    [Online_status] ->
		Now = mod_time:get_timestamp(),
		Count_online_time = Online_status#online_status.online_time + Now - Online_status#online_status.last_login_in,
	    User_online_status = Online_status#online_status{online_time = Count_online_time,last_login_in = Now},
        catch ets:insert(user_online_status,User_online_status)
	    end.

user_online_day_sum(Server) ->	
	All_status_users = ets:select(user_online_status, [{#online_status{user = '$1',_ = '_'},[],['$1']}]),
	Date = 
		case mod_time:timestamp_to_datetime(mod_time:get_timestamp() - 3600) of
		{{Year,Month,Day},{_,_,_}} ->
			list_to_binary(io_lib:format("~p-~p-~p",[Year,Month,Day]));
		_ ->
			<<"1970-1-1">>
		end,
	?DEBUG("Day ~p online sum begin ",[Date]),
	lists:foreach(fun(User) ->  day_end_online_count(User,Server,Date) end,All_status_users).

day_end_online_count(User,Server,Date) ->
	case ejabberd_sm:get_user_resources(User,Server) of
	[] ->
		relogin_out_time_sum(Server,User,Date,false);
	_ ->
		relogin_out_time_sum(Server,User,Date,true)
	end.

relogin_out_time_sum(Server,User,Date,Re_login_flag) ->
	case ets:lookup(user_online_status,User) of
	[] ->
		[];
	[Online_status] ->
		Now = mod_time:get_timestamp(),	
		Count_online_time = 
			case Re_login_flag of
			true ->
				Online_status#online_status.online_time + Now - Online_status#online_status.last_login_in;
			_ ->
				Online_status#online_status.online_time
			end,
        Last_login_in = 
			case Re_login_flag of
        	true ->
				Now;
    	    _ ->
				0
			end,
		case Count_online_time of
		0 ->
			ok;
		N when is_integer(N)  ->
			case catch odbc_queries:insert_day_online(Server,User,Date,N) of
			{updated, 1} -> 
					ok;
             Error -> ?DEBUG("Pgsql error  is ~p ~n",[Error])
			 end;
		_ ->
			ok
		end,
	    User_online_status =
			Online_status#online_status{online_time = 0,last_login_in = Last_login_in},
		catch ets:insert(user_online_status,User_online_status)
	end.

resave_ets_info() ->
	Count_info = ets:tab2list(monitor_count),
	lists:foreach(fun(Ci) ->
			case ets:lookup(monitor_sum,Ci#monitor_rec.key) of
			[] ->
				ets:insert(monitor_sum,Ci);
			_ ->
				Time = 
					case Ci#monitor_rec.count of
					0 ->
						Ci#monitor_rec.time;
					_ ->
						Ci#monitor_rec.time / Ci#monitor_rec.count
					end,
					ets:insert(monitor_sum,#monitor_rec{key = Ci#monitor_rec.key,count = Ci#monitor_rec.count,
													time = Time})
			end,
		   	ets:insert(monitor_count,#monitor_rec{key = Ci#monitor_rec.key,count = 0,time = 0})
			end,Count_info),
	Value_info = ets:tab2list(monitor_value),
	lists:foreach(fun(Vi) ->
				ets:insert(monitor_sum,Vi) 
			end,Value_info).
	
