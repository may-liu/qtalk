-module(asy_sql).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		                terminate/2,code_change/3]).
-export([update_readmark_by_id/2,update_user_muc_readmark/4,run_sql/2]).
-behaviour(gen_server).

-define(SERVER, ?MODULE).
-include("ejabberd.hrl").
-include("logger.hrl").

-record(state, {host,pid}).

start_link(Host,Opts) ->
    gen_server:start_link(?MODULE, [Host,Opts], []).

init([Host,_Opts]) ->
	mod_asy_sql:add_pid(Host,self()),
    {ok, #state{host = Host,pid = self()}}.

handle_call(Msg, _From, State) ->
    {reply, {ok, Msg}, State}.

handle_cast({update_readmark_id,Host,IDs},State) ->
	do_update_readmark_id(Host,IDs),
	{noreply,State};
handle_cast({update_readmark_date,Host,Muc,User,Time},State) ->
	do_update_readmark_date(Host,Muc,User,Time),
	{noreply,State};
handle_cast({sql,Host,Sql},State) ->
	do_run_sql(Host,Sql),
	{noreply,State};
handle_cast(stop, State) ->
	mod_asy_sql:remove_pid(State#state.host,self()),
    {stop, normal, State}.

handle_info(_From,State) ->
    {noreply,State}.

terminate(_Reason, State) ->
	mod_asy_sql:remove_pid(State#state.host,self()),
	{ok,State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

update_readmark_by_id(Host,IDs) ->
	Spid = mod_asy_sql:get_random_pid(Host),
	gen_server:cast(Spid, {update_readmark_id,Host,IDs}).

do_update_readmark_id(Host,IDs) ->
	Updates  = 
		lists:foldl(fun(ID,Acc) ->
			case Acc of [] -> 
					[<<" msg_id = '">> ,ID, <<"'">>] ;
			   	_ -> 
				%%	Acc ++ [<<"or msg_id = '">> ,ID, <<"'">>]
					[<<" msg_id = '">> ,ID, <<"' or ">>] ++ Acc 
			end end,[],IDs),
	case catch ejabberd_odbc:sql_query(Host,
		[<<"update msg_history set read_flag = 1 where (">>,Updates,<<") and read_flag = 0;">>]) of	
	{updated,1} -> 
		ok;
	Error ->
		?DEBUG("Error ~p ~n",[Error])
	end.

update_user_muc_readmark(Host,Muc,User,Time) ->
	Spid = mod_asy_sql:get_random_pid(Host),
	catch gen_server:cast(Spid, {update_readmark_date,Host,Muc,User,Time}).

do_update_readmark_date(Host,Muc,User,Time) when is_integer(Time) ->
	ITime = case catch Time < 1800000000 of
   			true ->  Time * 1000;
  			_ ->   Time
	        end,
        T_Time = ITime div 100 *100,
        Bin_T = integer_to_binary(ITime),
        Bin_T2 = integer_to_binary(T_Time),
	case catch ejabberd_odbc:sql_query(Host,
%		[<<"update muc_room_users set date = ">>,integer_to_binary(Time),<<" where muc_name = '">>,Muc,<<"' and username = '">>,User,<<"';">>]) of 
		[<<"update muc_room_users set date = ">>,Bin_T,<<" where muc_name = '">>,Muc,
				<<"' and username = '">>,User,<<"' and  date < ">>,Bin_T2,<<";">>]) of 
	{updated,1} ->
		ok;
	A ->
        Sql = [<<"update muc_room_users set date = ">>,Bin_T,<<" where muc_name = '">>,Muc,<<"' and username = '">>,User,<<"' and  date < ">>,Bin_T2,<<";">>],

		 ?DEBUG("A ~p,~p ~n",[A,list_to_binary(Sql)])
	end;
do_update_readmark_date(Host,Muc,User,Time) ->
    ?INFO_MSG("update_readmark_date Time is ~p ~n",[Time]).
    

%%异步插入更新
run_sql(Host,Sql) ->
	Spid = mod_asy_sql:get_random_pid(Host), 
	catch gen_server:cast(Spid, {sql,Host,Sql}).

do_run_sql(Host,Sql) ->
	catch ejabberd_odbc:sql_query(Host,Sql).
