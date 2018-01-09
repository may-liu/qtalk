-module(mod_update).

-behaviour(gen_server).
-behaviour(gen_mod).

-export([start/2,stop/1,start_link/1,init/1]).

-export([handle_call/3, handle_cast/2,
 	    handle_info/2, terminate/2, code_change/3]).

-export([update_whitelist/1,update_blacklist/1]).
-export([update_user_mask/4,del_user_mask/4,get_user_vcard_version/1,update_virtual_users/1]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("qunar_ejabberd_extend.hrl").
-record(state, {sql_tref,ets_tref1,client_info_tref,blacklist_tref, iplimit_tref, server}).

-define(SERVER, ?MODULE).
-define(PROCNAME, ?SERVER).

start(Host,Opts) ->
         Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
         ChildSpec = {Proc,{?MODULE, start_link, [{Host,Opts}]}, permanent, infinity,worker,[Proc]},
         {ok,_Pid} = supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
		table_handle:stop_ets_table(),
    	Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    	supervisor:terminate_child(ejabberd_sup, Proc),
    	supervisor:delete_child(ejabberd_sup, Proc).

start_link({Server,Opts}) ->
	 gen_server:start_link({local, ?SERVER}, ?MODULE, [{Server,Opts}], []).

init([{Server,Opts}]) ->
	create_ets_table(),
	_Time = get_update_time(Opts),
	{ok, TRef_sql} = timer:send_interval(7200*1000, update_pgsql),
    {ok, BlacklistTRef} = timer:send_interval(3600*1000, update_blacklist),
    {ok, IpLimitTRef} = timer:send_interval(600 * 1000, update_iplimit),
	{ok, #state{sql_tref = TRef_sql,
				   server = Server, blacklist_tref = BlacklistTRef, iplimit_tref = IpLimitTRef},0}.

handle_call(stop, _From, State=#state{sql_tref = TRef_sql, blacklist_tref = BlacklistTRef, iplimit_tref = IpLimitTRef}) ->
	{ok, cancel} = timer:cancel(TRef_sql),
	{ok, cancel} = timer:cancel(BlacklistTRef),
	{ok, cancel} = timer:cancel(IpLimitTRef),
	{stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.


handle_cast({update_virtual_session,Session},State) ->
    catch spawn(mod_extend_iq, update_virtual_session,[Session]),
    ?DEBUG("start Session ~p ~n",[Session]),
    {noreply, State};
handle_cast({delete_virtual_session,From,To},State) ->
    catch spawn(mod_extend_iq, end_virtual_session,[From,To]),
    ?DEBUG("end Session ~p , ~p ~n",[From,To]),
    {noreply, State};
handle_cast({update_virtual_user,Virtual_user},State) ->
    catch spawn(?MODULE,update_virtual_user,[State#state.server,Virtual_user,true]),
    {noreply, State};
handle_cast(_Msg, State) ->    
    {noreply, State}.

handle_info(timeout, State=#state{server = LServer}) ->
	catch ejabberd_s2s:update_s2s_mapperd_host(LServer),
	update_department_pgsql(LServer),
    update_blacklist(LServer),
	update_user_list(LServer),
	update_whitelist(LServer),
	update_vcard_version(LServer),
	update_user_sn(LServer),
	update_mac_push_notice(LServer),
	subscription:update_subscription_info(LServer),
	subscription:update_user_robots(LServer),
	iplimit_util:update_iplimit(LServer),
	update_multiple_users(LServer),
	update_user_mask_list(LServer),
    update_virtual_users(LServer),
%%	update_mac_sub_users(LServer),
	{noreply, State};
handle_info(update_pgsql, State=#state{server = LServer}) ->
	update_department_pgsql(LServer),
	update_user_list(LServer),
	update_user_sn(LServer),
	update_user_mask_list(LServer),
	{noreply, State};
handle_info(update_blacklist, State=#state{server = LServer}) ->
	update_whitelist(LServer),
    update_blacklist(LServer),
	update_vcard_version(LServer),
	subscription:update_subscription_info(LServer),
	subscription:update_user_robots(LServer),
    {noreply, State};
handle_info(update_iplimit, State=#state{server = LServer}) ->
    iplimit_util:update_iplimit(LServer),
	update_mac_push_notice(LServer),
    {noreply, State};
handle_info(update_user_info,State=#state{server = LServer}) ->
	mod_day_check:clear_muc_users_dimission(LServer),
	{noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

update_multiple_users(LServer) ->
	catch ets:insert(multiple_users,{<<"dan.liu">>,1}),
	catch ets:insert(multiple_users,{<<"ping.xue">>,1}),
	catch ets:insert(multiple_users,{<<"lilulucas.li">>,1}),
	catch ets:insert(multiple_users,{<<"xin.xie">>,1}),
	catch ets:insert(multiple_users,{<<"xinbo.wang">>,1}).

update_department_pgsql(LServer) ->
     case catch odbc_queries:get_department_info(jlib:nameprep(LServer)) of
     {selected,[<<"dep1">>,<<"dep2">>,<<"dep3">>,<<"dep4">>,<<"dep5">>,<<"username">>,<<"name">>,<<"department">>,<<"fpinyin">>,<<"spinyin">>], SRes}
      when is_list(SRes) ->
			ets:delete_all_objects(roster_name_nick),
      		lists:foreach(fun([D1,D2,D3,D4,D5,J,N,D,_Fp,_Sp]) -> 
				catch ets:insert(department_users,#department_users{dep1 = D1,dep2 = D2,dep3 = D3,dep4 = D4,dep5 = D5,user = J}),
				catch ets:insert(nick_name,{N,J}),
				catch ets:insert(roster_name_nick,{J,N,D})
		   	end,SRes);
      _ ->
                         []
      end.	

get_update_time(Opts) ->
    gen_mod:get_opt(update_time_interval, Opts,
	    fun(A) -> A end,
            500000).

create_ets_table() ->
	table_handle:create_mnesia_table(),
	table_handle:create_ets_table(),
    iplimit_util:create_ets().

update_mac_sub_users(LServer) ->
	case catch odbc_queries:get_mac_users(LServer) of
	{selected,[<<"user_name">>],SRes}  when is_list(SRes) ->
		lists:foreach(fun([U]) ->
				mnesia:dirty_write(#mac_sub_users{user = U,key = <<"">> }) 
				end,SRes);
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
    
update_mac_push_notice(LServer) ->
    ets:delete_all_objects(mac_push_notice),
	case catch ejabberd_odbc:sql_query(jlib:nameprep(LServer),
		 [<<"select user_name,shield_user  from mac_push_notice ;">>]) of
	{selected,[<<"user_name">>,<<"shield_user">>],SRes}
	when is_list(SRes) ->
		lists:foreach(fun([User,Shield]) ->
			ets:insert(mac_push_notice,#mac_push_notice{user = {User,Shield}}) end,SRes);
	Error ->
		?DEBUG("Get blacklist error for ~p ~n",[Error])
	end.

update_user_list(LServer) ->
    ets:delete_all_objects(userlist),
	case catch odbc_queries:list_users(jlib:nameprep(LServer)) of
	{selected,[<<"username">>],SRes}
	when is_list(SRes) ->
		lists:foreach(fun([Username]) ->
			ets:insert(userlist,{Username}) end,SRes);
	Error ->
		?DEBUG("Get userlist error for ~p ~n",[Error])
	end.

update_whitelist(LServer) ->
    ets:delete_all_objects(whitelist),
	case catch odbc_queries:get_white_list_users(jlib:nameprep(LServer)) of
	{selected,[<<"username">>, <<"single_flag">>],SRes}
	when is_list(SRes) ->
		lists:foreach(fun([Username, SingleFlag]) ->
			ets:insert(whitelist,{Username, SingleFlag}) end,SRes);
	Error ->
		?DEBUG("Get whitelist error for ~p ~n",[Error])
	end.

update_user_sn(LServer) ->
       ets:delete_all_objects(sn_user),
       case catch ejabberd_odbc:sql_query(LServer,[<<"select sn,hire_type from users where hire_flag > 0;">>]) of
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

update_black_version(Server) ->
	case catch ejabberd_odbc:sql_query(Server,[<<"select version from black_version;">>]) of
	{selected, [<<"version">>],Res}  when is_list(Res) ->
		ets:delete_all_objects(black_version),
		lists:foreach(fun(V) ->
			ets:insert(black_version,{V,1}) end,Res);
	_ ->
		ok
	end.

update_user_mask_list(Server) ->
	case catch ejabberd_odbc:sql_query(Server,[<<"select user_name,masked_user from mask_users;">>]) of
	{selected, [<<"user_name">>,<<"masked_user">>], SRes} when is_list(SRes) ->
		catch ets:delete_all_objects(user_mask_list),
		catch ets:delete_all_objects(shield_user),
		lists:foreach(fun([U,M]) ->
					update_user_mask(Server,U,M,false) end,SRes);
	_ ->
		ok
	end.
					

update_user_mask(Server,User,Mask,Sql_flag) ->
	case catch ets:lookup(user_mask_list,User) of
	[] ->
		case Sql_flag of
		true ->
			catch ejabberd_odbc:sql_query(Server,
				[<<"insert into mask_users(user_name,masked_user) values ('">>,User,<<"','">>,Mask,<<"');">>]);
		_ ->
			ok
		end,
		catch ets:insert(user_mask_list,{User,[Mask]}),
		catch ets:insert(shield_user,{list_to_binary(lists:sort([User,Mask])),1});
	[{User,L}] ->
		case catch lists:member(Mask,L) of
		false ->
			case Sql_flag of
			true ->
				catch ejabberd_odbc:sql_query(Server,
					[<<"insert into mask_users(user_name,masked_user) values ('">>,User,<<"','">>,Mask,<<"');">>]);
			_ ->
				ok
			end,
			catch ets:insert(user_mask_list,{User,[Mask] ++ L}),
			catch ets:insert(shield_user,{list_to_binary(lists:sort([User,Mask])),1});
		true ->
			ok
		end;
	_ ->
		ok
	end.
		 	
del_user_mask(Server,User,Mask,Sql_flag) ->
	case catch ets:lookup(user_mask_list,User) of
	[] ->
		ok;
	[{User,L}] ->
		case catch lists:member(Mask,L) of
		true ->
			case Sql_flag of
			true ->
				catch ejabberd_odbc:sql_query(Server,
					[<<"delete from mask_users where user_name = '">>,User,<<"' and masked_user = '">>,Mask,<<"';">>]);
			_ ->
				ok
			end,
			NewUL = L -- [Mask],
			case NewUL of
			[] ->
				ets:delete(user_mask_list,User);
			_ ->
				ets:insert(user_mask_list, {User,NewUL})
			end,
			del_ets_shield_user(User,Mask);
		_ ->
			ok
		end;
	_ ->
		ok
	end.

del_ets_shield_user(User,Mask) ->
	case catch ets:lookup(user_mask_list,Mask) of	
	[] ->
		catch ets:delete(shield_user,list_to_binary(lists:sort([User,Mask])));
	[{Mask,L}] when is_list(L) ->
		case catch lists:member(User,L) of
		false ->	
			catch ets:delete(shield_user,list_to_binary(lists:sort([User,Mask])));
		_ ->
			ok
		end;
	_ ->
		ok
	end.

update_vcard_version(Server) ->
	case catch ejabberd_odbc:sql_query(Server,[<<"select username,version,url from vcard_version;">>]) of
	{selected,_,SRes} when is_list(SRes) ->
		lists:foreach(fun([U,V,L]) ->
			ets:insert(vcard_version,#vcard_version{user = U,version = V,url = L}) end,SRes);
	_ ->
		ok
	end.

get_user_vcard_version(User) ->
	case catch ets:lookup(vcard_version,User) of
	[] ->
		{<<"1">>,<<"">>};
	[Vcard] when is_record(Vcard,vcard_version) ->
		{Vcard#vcard_version.version,Vcard#vcard_version.url};
	_ ->
		{<<"1">>,<<"">>}
	end.


update_virtual_user(Server,User,Flag) ->
    case Flag of
    false ->
        catch ets:delete(virtual_user,User);
    _ ->
        ok
    end,
    case catch ejabberd_odbc:sql_query(Server,[<<"select real_user,on_duty_flag from virtual_user_list where virtual_user = '">>,User,<<"';">>]) of
    {selected,_,SRes} when is_list(SRes) ->
        Users = 
            lists:flatmap(fun([U,F]) ->
                case F of
                <<"1">> ->
                    [U];
                _ ->
                    []
                end end,SRes),
        catch ets:insert(virtual_user,{User,Users});
    _ ->
        ok
    end.

update_virtual_users(Server) ->
    catch ets:delete_all_object(virtual_user),
    case catch ejabberd_odbc:sql_query(Server,[<<"select distinct(virtual_user) from virtual_user_list;">>]) of
    {selected,_,SRes} when is_list(SRes) ->
        lists:foreach(fun([U]) ->
            update_virtual_user(Server,U,false) end,SRes);
    _ ->
        ok
    end.
    
