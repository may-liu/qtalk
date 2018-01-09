-module(mod_extend_iq).

-behavior(gen_mod).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").


-define(SUPERVISOR, ejabberd_sup).

-define(DEFAULT_SEND_PINGS, false).

-define(DEFAULT_PING_INTERVAL, 60).

-define(DICT, dict).

%% gen_mod callbacks
-export([start/2, stop/1]).


%% Hook callbacks
-export([get_key/3,set_blocked_user/3,
	 cancel_blocked_user/3,get_recent_contact/3,get_muc_contact/3]).

-export([mac_push_notice/3,get_mac_push_notice/1,get_mac_push_notice/2,set_mac_push_notice/2,cancel_mac_push_notice/2]).
-export([handle_mask_user/3,handle_virtual_user/3,end_virtual_session/2]).
-export([update_virtual_session/1,end_virtual_session/2,handle_outdate_virtual_session/1,end_virtual_sessions/1]).


-record(mac_push_notice,{user}).
-record(virtual_session, {key,session,vuser,ruser,consumer,stime}).

%%====================================================================
stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),

    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
				     ?NS_KEY),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,
				     ?NS_KEY),

    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
				     ?NS_SET_BLOCKED),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,
				     ?NS_SET_BLOCKED),
    
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
				     ?NS_CANCEL_BLOCKED),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,
				     ?NS_CANCEL_BLOCKED),

    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
				     ?NS_RECENT_CONTACT),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,
				     ?NS_RECENT_CONTACT),

    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
				     ?NS_MUC_CONTACT),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,
				     ?NS_MUC_CONTACT),

    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
				     ?NS_MASK_USER),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,
				     ?NS_MASK_USER),

    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
				     ?NS_VIRTUAL_USER),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,
				     ?NS_VIRTUAL_USER),

    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
				     ?NS_MAC_PUSH_NOTICE),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,
				     ?NS_MAC_PUSH_NOTICE).

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,one_queue),

    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
				  ?NS_KEY, ?MODULE, get_key, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_KEY, ?MODULE, get_key, IQDisc),

    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
				  ?NS_SET_BLOCKED, ?MODULE, set_blocked_user, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_SET_BLOCKED, ?MODULE, set_blocked_user, IQDisc),

    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
				  ?NS_CANCEL_BLOCKED, ?MODULE, cancel_blocked_user, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_CANCEL_BLOCKED, ?MODULE, cancel_blocked_user, IQDisc),

    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
				  ?NS_MASK_USER, ?MODULE, handle_mask_user, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_MASK_USER, ?MODULE, handle_mask_user, IQDisc),

    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
				  ?NS_MUC_CONTACT, ?MODULE, get_muc_contact, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_MUC_CONTACT, ?MODULE, get_muc_contact, IQDisc),

    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
				  ?NS_MUC_CONTACT, ?MODULE, get_muc_contact, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_MUC_CONTACT, ?MODULE, get_muc_contact, IQDisc),

    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
				  ?NS_VIRTUAL_USER, ?MODULE, handle_virtual_user, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_VIRTUAL_USER, ?MODULE, handle_virtual_user, IQDisc),

    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
				  ?NS_MAC_PUSH_NOTICE, ?MODULE, mac_push_notice, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_MAC_PUSH_NOTICE, ?MODULE, mac_push_notice, IQDisc).

get_key(From, _To,
	#iq{type = Type, sub_el = SubEl} = IQ) ->
    case {Type, SubEl} of
      {get, #xmlel{name = <<"key">>}} ->
	  IQ#iq{type = result, sub_el = [make_iq_key_reply(From)]};
      _ ->
	  IQ#iq{type = error,
		sub_el = [SubEl, ?ERR_FEATURE_NOT_IMPLEMENTED]}
    end.

mac_push_notice(From, _To,
	#iq{type = Type, sub_el = SubEl} = IQ) 	->
	case {Type, SubEl} of
	{get, #xmlel{name = <<"mac_push_notice">>}} ->
		IQ#iq{type = result, sub_el = [get_mac_push_notice(From)]};
	{get, #xmlel{name = <<"get_mac_push_notice">>}} ->
		IQ#iq{type = result, sub_el = [get_mac_push_notice(From,SubEl)]};
	{set, #xmlel{name = <<"mac_push_notice">>}} ->
		IQ#iq{type = result, sub_el = [set_mac_push_notice(From,SubEl)]};
	{set, #xmlel{name = <<"cancel_mac_push_notice">>}} ->
		IQ#iq{type = result, sub_el = [cancel_mac_push_notice(From,SubEl)]};
	_ ->
		IQ#iq{type = error,sub_el = [SubEl, ?ERR_FEATURE_NOT_IMPLEMENTED]}
	end.

set_blocked_user(From, _To,
	#iq{type = Type, sub_el = SubEl} = IQ) ->
	case {Type,SubEl} of
	  {set,#xmlel{name = <<"block_user">>}} ->
	  	 IQ#iq{type = result, sub_el = [set_block_user(From,SubEl)]};
      _ ->
	  IQ#iq{type = error,
		sub_el = [SubEl, ?ERR_FEATURE_NOT_IMPLEMENTED]}
    end.

cancel_blocked_user(From, _To,
	#iq{type = Type, sub_el = SubEl} = IQ) ->
	case {Type,SubEl} of
	  {set,#xmlel{name = <<"block_user">>}} ->
	  	 IQ#iq{type = result, sub_el = [cancel_block_user(From,SubEl)]};
      _ ->
	  IQ#iq{type = error,
		sub_el = [SubEl, ?ERR_FEATURE_NOT_IMPLEMENTED]}
    end.

get_recent_contact(From, _To,
	#iq{type = Type, sub_el = SubEl} = IQ) ->
	case {Type,SubEl} of
	  {get,#xmlel{name = <<"recent_contact_user">>}} ->
	  	 IQ#iq{type = result, sub_el = [get_recent_contact_user(From)]};
      _ ->
	  IQ#iq{type = error,
		sub_el = [SubEl, ?ERR_FEATURE_NOT_IMPLEMENTED]}
    end.
		
get_muc_contact(From, _To,
	#iq{type = Type, sub_el = SubEl} = IQ) ->
	case {Type,SubEl} of
	  {get,#xmlel{name = <<"recent_contact_muc">>}} ->
	  	 IQ#iq{type = result, sub_el = [get_muc_recent_contact(From)]};
      _ ->
	  IQ#iq{type = error,
		sub_el = [SubEl, ?ERR_FEATURE_NOT_IMPLEMENTED]}
    end.

make_iq_key_reply(From) ->
	Resource = From#jid.resource,
	User = From#jid.user,
	LServer = jlib:nameprep(From#jid.server),
	V =
		case redis_link:hash_get(LServer,1,User,Resource) of
		{ok,undefined} ->
			<<"">>;
		{ok,Key} ->
			Key;
		_ ->
			<<"">>
		end,
	#xmlel{name = <<"key">>,
			attrs = [{<<"xmlns">>,?NS_KEY},{<<"value">>,V}],children = []}.


set_block_user(From,El) ->
	User = From#jid.user,
	LServer = jlib:nameprep(From#jid.server),
	V =
		case xml:get_tag_attr_s(<<"jid">>,El) of
	 	<<"">> -> 
			<<"Null">>;
		J ->
			case catch ejabberd_odbc:sql_query(LServer,
					[<<"insert into user_block_list(username,blockuser) values ('">>,User,<<"','">>,J,<<"');">>]) of
			{updated,1} ->
				<<"sucess">>;
			{error,Reason}  ->
				case  proplists:get_value(code,Reason) of
				<<"23505">> ->
					<<"sucess">>;
				_ ->
					<<"failed">>
				end;
			_ ->
				<<"failed">>
			end
		end,
				
    #xmlel{name = <<"block_user">>,
			attrs = [{<<"xmlns">>,?NS_SET_BLOCKED},
						{<<"result">>,V}],
			children = []}.

cancel_block_user(From,El) ->
	User = From#jid.user,
	LServer = jlib:nameprep(From#jid.server),
	V =
		case xml:get_tag_attr_s(<<"jid">>,El) of
	 	<<"">> -> 
			<<"null">>;
		J ->
			case catch ejabberd_odbc:sql_query(LServer,
					[<<"delete from user_block_list where username = '">>,User,<<"' and blockuser = '">>,J,<<"';">>]) of
			{updated,1} ->
				<<"sucess">>;
			_ ->
				<<"failed">>
			end
		end,
    #xmlel{name = <<"block_user">>,
			attrs = [{<<"xmlns">>,?NS_CANCEL_BLOCKED},
						{<<"result">>,V}],
			children = []}.

get_recent_contact_user(From) ->
	User = From#jid.user,
	LServer = jlib:nameprep(From#jid.server),
	Concat_users = 
		case catch odbc_queries:get_concats(LServer,User) of
		{selected, [<<"u">>], SRes} when is_list(SRes) ->
			lists:usort(lists:concat(SRes));
		_ ->
			[]
		end,
	Block_users = 
		case catch ejabberd_odbc:sql_query(LServer,
			[<<"select blockuser from user_block_list where username = '">>,User,<<"';">>]) of
		{selected, [<<"blockuser">>], SRes1} when is_list(SRes1) ->
			lists:concat(SRes1);
		_  ->
			[]
		end,
	Final_users = 
		lists:foldl(fun(U,Acc) ->
			case lists:member(U,Acc) of
			true ->
				lists:delete(U,Acc);
			_ ->
				Acc
			end end,Concat_users,Block_users),
				
	#xmlel{name = <<"recent_contact_user">>,
			attrs = [{<<"xmlns">>,?NS_RECENT_CONTACT},
						{<<"conctat_user">>,list_to_binary(spell_user(Final_users))},{<<"block_user">>,list_to_binary(spell_user(Block_users))}],
			children = []}.

get_muc_recent_contact(From) ->
	User = From#jid.user,
	LServer = jlib:nameprep(From#jid.server),
	Rooms = 
		case catch odbc_queries:get_muc_concats(LServer,User) of
		{selected, [<<"muc_name">>], SRes} when is_list(SRes) ->
			lists:usort(lists:concat(SRes));
		_ ->
			[]
		end,
	#xmlel{name = <<"recent_contact_muc">>,
			attrs = [{<<"xmlns">>,?NS_MUC_CONTACT},
						{<<"conctat_rooms">>,list_to_binary(spell_user(Rooms))}],
			children = []}.

	
spell_user(User) ->
	lists:foldl(fun(U,Acc) ->
		case Acc of 
		[] ->
			[U];
		_ ->
			lists:concat([Acc,[<<",">>,U]])
		 end end,[],User).

get_mac_push_notice(From) ->
	User = From#jid.user,
	LServer = jlib:nameprep(From#jid.server),
	UserList = 
		case catch ets:select(mac_push_notice, [{#mac_push_notice{user = {'$1','$2'}, _ = '_'},[{'==', '$1', User}], ['$2']}]) of
		[] ->
			[];
		UL when is_list(UL) ->
			UL;
		_ ->
			[]
		end,
    #xmlel{name = <<"mac_push_notice">>,
			attrs =
%%			   	[{<<"xmlns">>,?NS_MAC_PUSH_NOTICE},
%%		    {<<"shield_user">>,list_to_binary(spell_user(UserList))}],
		lists:flatmap(fun(U) ->
				[{<<"xmlns">>,?NS_MAC_PUSH_NOTICE},{<<"shield_user">>,U}] end,UserList),
		children = []}.


get_mac_push_notice(From,El) ->
	User = From#jid.user,
	LServer = jlib:nameprep(From#jid.server),
	V =
		case xml:get_tag_attr_s(<<"jid">>,El) of
	 	<<"">> -> 
			<<"no_jid">>;
		J ->
			case ets:lookup(mac_push_notice,{User,J}) of
			[] ->
				<<"no_shield">>;
			_ ->
				<<"shield">>
			end
		end,
    #xmlel{name = <<"get_mac_push_notice">>,
			attrs = [{<<"xmlns">>,?NS_MAC_PUSH_NOTICE},
						{<<"result">>,V}],
			children = []}.

set_mac_push_notice(From,El) ->
        User = From#jid.user,
        LServer = jlib:nameprep(From#jid.server),
        V =
                case xml:get_tag_attr_s(<<"jid">>,El) of
                <<"">> ->
                        <<"failed">>;
                J ->
                        case ets:lookup(mac_push_notice,{User,J}) of
                        [] ->
                                catch ets:insert(mac_push_notice,#mac_push_notice{user = {User,J}}),
                                case catch ejabberd_odbc:sql_query(LServer,
                                        [<<"insert into mac_push_notice(user_name,shield_user) values ('">>,User,<<"','">>,J,<<"');">>]) of
                                {updated, 1} -> <<"sucess">>;
                                _ -> <<"failed">>
                                end;
                        _ ->
                                <<"sucess">>
                        end
                end,
    #xmlel{name = <<"mac_push_notice">>,
                        attrs = [{<<"xmlns">>,?NS_MAC_PUSH_NOTICE},
                                                {<<"result">>,V}],
                        children = []}.

cancel_mac_push_notice(From,El) ->
	User = From#jid.user,
	LServer = jlib:nameprep(From#jid.server),
	V =
		case xml:get_tag_attr_s(<<"jid">>,El) of
	 	<<"">> -> 
			<<"failed">>;
		J ->
			case catch ejabberd_odbc:sql_query(LServer,
				[<<"delete from mac_push_notice where user_name = '">>,User,<<"' and shield_user = '">>,J,<<"';">>]) of
			{updated, 1} ->
				case ets:lookup(mac_push_notice,{User,J}) of
				[] ->
					<<"sucess">>;	
				_->
					catch ets:delete(mac_push_notice,{User,J}),
					<<"sucess">>
				end;
			_ -> 
				<<"sucess">>
			end	
		end,
    #xmlel{name = <<"calcel_mac_push_notice">>,
			attrs = [{<<"xmlns">>,?NS_MAC_PUSH_NOTICE},
						{<<"result">>,V}]}.

handle_mask_user(From, _To,
	#iq{type = Type, sub_el = SubEl} = IQ) 	->
	case {Type, SubEl} of
	{get, #xmlel{name = <<"mask_user">>}} ->
		IQ#iq{type = result, sub_el = get_mask_user(From)};
	{set, #xmlel{name = <<"mask_user">>}} ->
		IQ#iq{type = result, sub_el = [set_mask_user(From,SubEl)]};
	{set, #xmlel{name = <<"cancel_mask_user">>}} ->
		IQ#iq{type = result, sub_el = [cancel_mask_user(From,SubEl)]};
	_ ->
		IQ#iq{type = error,sub_el = [SubEl, ?ERR_FEATURE_NOT_IMPLEMENTED]}
	end.

handle_virtual_user(From, To,
	#iq{type = Type, sub_el = SubEl} = IQ) 	->
	case {Type, SubEl} of
	{get, #xmlel{name = <<"real_user_start_session">>}} ->
		IQ#iq{type = result, sub_el = real_user_start_session(From,SubEl)};
	{get, #xmlel{name = <<"get_virtual_user">>}} ->
		IQ#iq{type = result, sub_el = get_virtual_user(From)};
	{get, #xmlel{name = <<"get_virtual_user_role">>}} ->
		IQ#iq{type = result, sub_el = get_virtual_user_role(From)};
	{set, #xmlel{name = <<"virtual_user_end_session">>}} ->
		IQ#iq{type = result, sub_el = virtual_user_end_session(From,SubEl)};
	{set, #xmlel{name = <<"real_user_end_session">>}} ->
		IQ#iq{type = result, sub_el = real_user_end_session(From,SubEl)};
	_ ->
		IQ#iq{type = error,sub_el = [SubEl, ?ERR_FEATURE_NOT_IMPLEMENTED]}
    end.

get_mask_user(From) ->
	User = From#jid.user,
	LServer = jlib:nameprep(From#jid.server),
	UL = 
		case ets:lookup(user_mask_list,jlib:jid_to_string({User,LServer,<<"">>})) of
		[{_,L}] when is_list(L) ->
			L;
		_ ->
			[]
		end,
	Res = 
        lists:map(fun(U) ->
	    #xmlel{name = <<"get_mask_user">>,
				attrs = 	
					[{<<"xmlns">>,?NS_MASK_USER},{<<"masked_user">>,U}],
		    children = []} end,UL),
    [#xmlel{name = <<"query">>,attrs = [{<<"xmlns">>,<<"jabber:x:mask_user_v2">>}],children = []}]++ Res.


set_mask_user(From,El) ->
	LServer = jlib:nameprep(From#jid.server),
	User = jlib:jid_to_string({From#jid.luser,LServer,<<"">>}),
	V =
		case xml:get_tag_attr_s(<<"jid">>,El) of
	 	<<"">> -> 
			<<"failed">>;
		J ->
			XMLNS = ?NS_MASK_USER,
			Name = <<"mask_user">>,
			Attrs = [{<<"jid">>,J}],
			send_presence_notice(From,XMLNS,Name,Attrs),
			mod_update_v2:update_user_mask(LServer,User,J,true),
			lists:foreach(fun(ONode) ->
				catch sync_ets_cache:send_sync_node_notcie(LServer,ONode,mod_update_v2,update_user_mask,[LServer,User,J,false]) end,nodes(visible)),
			<<"success">>
		end,
    #xmlel{name = <<"mask_user">>,
			attrs = [{<<"xmlns">>,?NS_MASK_USER},
						{<<"result">>,V}]}.

cancel_mask_user(From,El) ->
	LServer = jlib:nameprep(From#jid.server),
	User = jlib:jid_to_string({From#jid.luser,LServer,<<"">>}),
	V =
		case xml:get_tag_attr_s(<<"jid">>,El) of
	 	<<"">> -> 
			<<"failed">>;
		J ->
			XMLNS = ?NS_MASK_USER,
			Name = <<"cancel_masked_user">>,
			Attrs = [{<<"jid">>,J}],
			send_presence_notice(From,XMLNS,Name,Attrs),
			mod_update_v2:del_user_mask(LServer,User,J,true),
			lists:foreach(fun(ONode) ->
				catch sync_ets_cache:send_sync_node_notcie(LServer,ONode,mod_update_v2,del_user_mask,[LServer,User,J,false]) end,nodes(visible)),
			<<"success">>
		end,
    #xmlel{name = <<"cancel_mask_user">>,
			attrs = [{<<"xmlns">>,?NS_MASK_USER},
						{<<"result">>,V}]}.

send_presence_notice(From,XMLNS,Name,Attrs) ->
	Presence_packet =
	 	#xmlel{name = <<"presence">>,
			    attrs =[{<<"xmlns">>,XMLNS}],
				children = [
					#xmlel{name = Name,
							attrs = Attrs,  
							children = []}]},
	ejabberd_router:route(From,jlib:jid_remove_resource(From),Presence_packet).
	
		
real_user_start_session(From,El) ->
    Res = do_real_user_start_session(From,El),
 	[#xmlel{name = <<"start_session">>,
			attrs = [{<<"xmlns">>,?NS_VIRTUAL_USER},
						Res]}].

do_real_user_start_session(From,El) ->
    case xml:get_tag_attr_s(<<"jid">>,El) of
    false ->
        {<<"result">>,<<"start session failed">>};
    JID ->
        Session = 
            case ets:lookup(virtual_session, list_to_binary(lists:sort([From#jid.luser,JID]))) of
            S when is_record(S,virtual_session) ->
                S;
            _ ->
               init_session(From#jid.luser,JID) 
            end,
        case Session of
        undefined ->
            {<<"result">>,<<"start session failed">>};
        _ ->
            {<<"result">>,<<"start session sucess">>},{<<"session">>,Session#virtual_session.session},{<<"real_user">>,Session#virtual_session.ruser}
        end
    end.

init_session(From,To) ->
    UUID = list_to_binary(uuid:to_string(uuid:random())),
    case assign_virtual_user(To) of
    [] ->
        undefined;
    Assign_User ->
        Session = #virtual_session{key = list_to_binary(lists:sort([From,To])),session = UUID,
                                vuser = To,ruser = Assign_User,
                                consumer = From,stime = mod_time:get_timestamp()},
        update_virtual_session(Session),
        update_nodes_virtual_session(Session),
        Session
    end.


assign_virtual_user(Virtual) ->
    case catch ets:lookup(virtual_user,Virtual) of
    [] ->
        [];
    [{_,L}] when is_list(L) ->
        lists:nth(erlang:phash(os:timestamp(), length(L)), L);
    _ ->
        []
    end.

update_virtual_session(Session) ->
    catch ets:insert(virtual_session,Session).

update_nodes_virtual_session(Session) ->
    L = nodes('visible'),
    lists:foreach(fun(N) ->
        Node =  list_to_binary(atom_to_list(N)),
        case str:str(Node,<<"ejabberd">>) of
        0 ->
            ok;
        _ ->
            catch gen_server:cast({'mod_update_v2',N},{update_virtual_session,Session})
        end end,L).
        

virtual_user_end_session(From,El) ->
    Res = 
        case xml:get_tag_attr_s(<<"jid">>,El) of
        <<"">> ->
            <<"failed">>;
        JID ->
            do_virtual_user_end_session(From#jid.luser,JID)    
        end,
 	[#xmlel{name = <<"end_session">>,
			attrs = [{<<"xmlns">>,?NS_VIRTUAL_USER},
						{<<"result">>,Res}]}].

do_virtual_user_end_session(From,To) ->
    end_virtual_session(From,To),
    end_nodes_virtual_session(From,To),
    <<"success">>. 

end_virtual_session(From,To) ->
    catch ets:delete(virtual_session, list_to_binary(lists:sort([From,To]))).

end_nodes_virtual_session(From,To) ->
    L = nodes(),
    lists:foreach(fun(N) ->
        Node =  list_to_binary(atom_to_list(N)),
        case str:str(Node,<<"ejabberd">>) of
        0 ->
            ok;
        _ ->
            catch gen_server:cast({'mod_update_v2',N},{delete_virtual_session,From,To})
        end end,L).

real_user_end_session(From,El) ->
    Res = 
        case xml:get_tag_attr_s(<<"jid">>,El) of
        <<"">> ->
            <<"failed">>;
        JID ->
            do_real_user_end_session(From#jid.luser,JID)
        end,
    [#xmlel{name = <<"end_session">>,
	    attrs = [{<<"xmlns">>,?NS_VIRTUAL_USER},
		    {<<"result">>,Res}]}].
    

do_real_user_end_session(From,To)->
    end_virtual_session(From,To),
    end_nodes_virtual_session(From,To),
    <<"success">>.


get_virtual_user(From) ->
    Res = 
        case catch ejabberd_odbc:sql_query(From#jid.lserver,[<<"select distinct(virtual_user) from virtual_user_list;">>]) of
%        case catch ets:lookup(virtual_user,To) of
%%        [{_,L}] when is_list(L)  ->
        {selected,_,L} when is_list(L)->
            lists:map(fun(U) ->
                #xmlel{name = <<"virtual_user">>,
                        attrs =
                      [{<<"xmlns">>,?NS_VIRTUAL_USER},{<<"virtual_user">>,U}],
              children = []} end,L);
        _ ->
            []
        end,
    Res.

get_virtual_user_role(From) ->
    case catch  ejabberd_odbc:sql_query(From#jid.lserver,
        [<<"select virtual_user from virtual_user_list where real_user = '">>,From#jid.luser,<<"' and on_duty_flag = 1;">>]) of
    {selected,_,L} when is_list(L)->
        lists:map(fun(R) ->
            #xmlel{name = <<"on_duty_virtual_user">>,
                attrs =
                    [{<<"xmlns">>,?NS_VIRTUAL_USER},{<<"virtual_user">>,R}],children = []} end,L);
    _ ->
        []
    end.
        
    

handle_outdate_virtual_session(Server) ->
    Time = mod_time:get_timestamp(),
    Session_list = 
        lists:flatmap(fun(S) ->
            case (Time - S#virtual_session.stime > 1800*1000) of
            true ->
                catch ets:delete(virtual_session,S#virtual_session.key),
                send_end_session_presence(Server,S#virtual_session.vuser,S#virtual_session.consumer,stime),
                [S#virtual_session.key];
            _ ->
                []     
            end end,ets:tab2list(virtual_session)),
    L = nodes(),
    lists:foreach(fun(N) ->
        Node =  list_to_binary(atom_to_list(N)),
        case str:str(Node,<<"ejabberd">>) of
        0 ->
            ok;
        _ ->
            catch gen_server:cast({'mod_update_v2',N},{delete_virtual_sessions,Session_list})
        end end,L). 
           
end_virtual_sessions(Keys) ->
    lists:foreach(fun(K) ->
        catch ets:delete(virtual_session,K)
        end,Keys). 

send_end_session_presence(Server,Vuser,Consumer,stime) ->
    Packet =
       #xmlel{name = <<"presence">>,
              attrs = [{<<"xmlns">>,?NS_VIRTUAL_USER}, {<<"end_session">>, Vuser} , {<<"reason">>,<<"out_date">>}],  children = []},
    From = jlib:make_jid(Consumer,Server,<<"">>),
    ?DEBUG("Packet ~p ~n",[Packet]),
    ejabberd_router:route(From, From, Packet).
