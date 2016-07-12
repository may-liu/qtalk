-module(async_sql).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		                terminate/2,code_change/3]).
-export([update_readmark_by_id/2,update_user_muc_readmark/4,run_sql/2]).
-export([insert_subscribe_msg/5]).

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-include("ejabberd.hrl").
-include("logger.hrl").

-record(state, {host,pid}).
-record(subscribe_users,{room,users}).

start_link(Host,Opts) ->
    gen_server:start_link(?MODULE, [Host,Opts], []).

init([Host,_Opts]) ->
	mod_async_sql:add_pid(Host,self()),
    {ok, #state{host = Host,pid = self()}}.

handle_call(Msg, _From, State) ->
    {reply, {ok, Msg}, State}.

handle_cast({update_readmark_id,Host,IDs},State) ->
	do_update_readmark_id(Host,IDs),
	{noreply,State};
handle_cast({update_readmark_date,Host,Muc,User,Time},State) ->
	do_update_readmark_date(Host,Muc,User,Time),
	{noreply,State};
handle_cast({insert_subscribe_msg,Host,Room,User,Nick,Xml},State) ->
    do_insert_subscribe_msg(Host,Room,User,Nick,Xml),
    {noreply,State};
handle_cast({sql,Host,Sql},State) ->
	do_run_sql(Host,Sql),
	{noreply,State};
handle_cast(stop, State) ->
	mod_async_sql:remove_pid(State#state.host,self()),
    {stop, normal, State}.

handle_info(_From,State) ->
    {noreply,State}.

terminate(_Reason, State) ->
	mod_async_sql:remove_pid(State#state.host,self()),
	{ok,State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

update_readmark_by_id(Host,IDs) ->
	Spid = mod_async_sql:get_random_pid(Host),
	gen_server:cast(Spid, {update_readmark_id,Host,IDs}).

do_update_readmark_id(Host,IDs) ->
	Updates  = 
		lists:foldl(fun(ID,Acc) ->
			case Acc of [] -> 
					[<<" msg_id = '">> ,ID, <<"'">>] ;
			   	_ -> 
					[<<" msg_id = '">> ,ID, <<"' or ">>] ++ Acc
			end end,[],IDs),
	case catch ejabberd_odbc:sql_query(Host,
		[<<"update msg_history set read_flag = 1 where ">>,Updates,<<";">>]) of	
	{updated,1} -> 
		ok;
	Error ->
		?DEBUG("Update read_flag error ~p ~n",[Error])
	end.

update_user_muc_readmark(Host,Muc,User,Time) ->
	Spid = mod_async_sql:get_random_pid(Host),
	catch gen_server:cast(Spid, {update_readmark_date,Host,Muc,User,Time}).

do_update_readmark_date(Host,Muc,User,Time) ->
	case catch ejabberd_odbc:sql_query(Host,
		[<<"update muc_room_users set date = ">>,integer_to_binary(Time),<<" where muc_name = '">>,Muc,<<"' and username = '">>,User,<<"';">>]) of 
	{updated,1} ->
		ok;
	Error ->
		 ?DEBUG("Update muc read_date error ~p ~n",[Error])
	end.
%%异步插入更新
run_sql(Host,Sql) ->
	Spid = mod_async_sql:get_random_pid(Host), 
	catch gen_server:cast(Spid, {sql,Host,Sql}).

do_run_sql(Host,Sql) ->
	catch ejabberd_odbc:sql_query(Host,Sql).

insert_subscribe_msg(Host,Room,User,Nick,Xml) ->
	Spid = mod_async_sql:get_random_pid(Host),
	catch gen_server:cast(Spid, {insert_subscribe_msg,Host,Room,User,Nick,Xml}).

do_insert_subscribe_msg(Host,Room,User,Nick,Xml) ->
	List_Sub_Users = 
		case catch ets:lookup(muc_subscribe_users,Room) of
		[] ->
			[];
		[SU] when is_record(SU,subscribe_users) ->
			get_muc_subscribe_user(SU,User,Host);
		_ ->
			[]
		end,
	Sub_Users = list_to_binary(List_Sub_Users),
	case size(Sub_Users) of
	0 ->
		ok;
	_ ->
		case catch odbc_queries:insert_subscribe_msg(Host,Sub_Users,Room,Nick,Xml) of
		{selected,[<<"spilt_users_to_insert_xml">>],[[<<"1">>]]} ->
			{atomic, ok};
		Reason ->
			?INFO_MSG("insert_subscribe_msg  ~p ~n",[Reason])
		end
	end.

get_muc_subscribe_user(Users,User,Server) ->
	lists:foldl(fun(U,Acc) ->
		case User =/= U of
		true ->
			case ejabberd_sm:get_user_away_rescources(U,Server) of
			[] ->
				Acc;
			L  when is_list(L) ->
				case lists:filter(fun(Rs) ->
						str:str(Rs,<<"PC_Client">>) =/= 0 orelse str:str(Rs,<<"mac">>) =/= 0
						orelse str:str(Rs,<<"none">>) =/= 0 end,L) of
				[] ->
					Acc;
				_ ->
					case Acc of
					[] ->
						[U];
					_ ->
						lists:append([U,<<",">>],Acc)
					end
				end;
			_ ->
				Acc
			end;
		_ ->
			Acc
		end end,[],Users).
