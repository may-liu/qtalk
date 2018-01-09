-module(http_muc_session).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("http_req.hrl").
-include("jlib.hrl").

-export([clean_session_list/1,make_muc_presence/0,update_user_presence_a/4,update_user_presence_a/5,update_pg_muc_register/3,kick_muc_user/2]).
-export([remove_presence_a/4,get_value/3,delete_unavailable_user/4,remove_muc_users/4,make_muc_persistent/0,make_invite_iq/2]).
-export([handle_add_muc_users/4,make_create_muc_iq/0,make_register_muc_iq/0,make_del_register_muc_iq/0]).
-export([check_muc_exist/2]).

-record(session, {sid, usr, us, priority, info, show}).
-record(muc_online_room,
          {name_host = {<<"">>, <<"">>} :: {binary(), binary()} | '$1' |
                          {'_', binary()} | '_',
                       pid = self() :: pid() | '$2' | '_' | '$1'}).

clean_session_list(Ss) ->
    clean_session_list(lists:keysort(#session.usr, Ss), []).

clean_session_list([], Res) -> Res;
clean_session_list([S], Res) -> [S | Res];
clean_session_list([S1, S2 | Rest], Res) ->
	if S1#session.usr == S2#session.usr ->
    	if S1#session.sid > S2#session.sid ->
        	clean_session_list([S1 | Rest], Res);
	        	true -> clean_session_list([S2 | Rest], Res)
      		 end;
		true -> clean_session_list([S2 | Rest], [S1 | Res])
    end.

make_muc_presence() ->
		#xmlel{name = <<"presence">>,attrs = [{<<"priority">>,<<"5">>}],
			children = [#xmlel{name = <<"x">>, attrs =[{<<"xmlns">>,	?NS_MUC}], children = []}]}.

%%send_muc_opts(Server,Owner,To_owner) ->
%%	ok.

update_user_presence_a(Server,User,Muc,Domain) ->
	case catch  mnesia:dirty_index_read(session,{User,Server}, #session.us) of
	{'EXIT', _Reason} -> [];
	Ss when is_list(Ss) ->
      [ element(2, S#session.sid) ! {update_presence_a,{Muc,Domain,User}} || S <- clean_session_list(Ss), is_integer(S#session.priority)];
	_ ->
		[]
	end.

update_user_presence_a(Server,User,R,Muc,Domain) ->
	case catch  mnesia:dirty_index_read(session,{User,Server,R}, #session.usr) of
	{'EXIT', _Reason} -> [];
	[S] when is_record(S,session) ->
       element(2, S#session.sid) ! {update_presence_a,{Muc,Domain,User}};
	_ ->
		[]
	end.

update_pg_muc_register(Server,Muc_id,Muc_member) ->
	lists:foreach(fun(U) ->
		case catch odbc_queries:insert_muc_users(Server,<<"muc_room_users">>,Muc_id,U,Server) of
		{updated,1} ->
			ok;
		Error ->
			?DEBUG("Update Reason ~p ~n",[Error])
		end end,Muc_member).

kick_muc_user(Server,User) ->
	jlib:iq_to_xml(
	 #iq{type = set, sub_el = [
    #xmlel{name = <<"query">>,
		attrs = [{<<"xmlns">>,?NS_MUC_ADMIN}], children = [
		#xmlel{name = <<"item">>,attrs = [{<<"nick">>,User},{<<"role">>,<<"none">>}], children = [{<<"reason">>,<<"ads">>}]}]}]}).

remove_presence_a(Server,User,Muc,Domain) ->
	case catch  mnesia:dirty_index_read(session,{User,Server}, #session.us) of
	{'EXIT', _Reason} -> [];
	Ss when is_list(Ss) ->
      [ element(2, S#session.sid) ! {remove_presence_a,{Muc,Domain,User}} || S <- clean_session_list(Ss), is_integer(S#session.priority)];
	_ ->
		[]
	end.

get_value(Key,Args,Default) ->
	case proplists:get_value(Key,Args) of
	undefined ->
		Default;
	V ->
		V
	end.

delete_unavailable_user(Server,Muc_id,Domain,User) ->
	 case mnesia:dirty_read(muc_online_room, {Muc_id, Domain}) of
	 [] ->
	 	ok;
	 [Muc] ->
	 	Muc#muc_online_room.pid ! {delete_unavailable_user,{User,Server,<<"">>}}
	 end.
	 	
remove_muc_users(Server,Users,Muc_id,Domain) ->
	case catch mnesia:dirty_read(muc_online_room, {Muc_id, Domain}) of
	[] ->
		lists:foreach(fun(U) ->
			catch odbc_queries:del_muc_user(Server,<<"muc_room_users">>,Muc_id,U) end,Users);
	[Muc] ->
		lists:foreach(fun(U) ->
			case jlib:make_jid(U,Server,<<"">>) of
			error ->
				ok;
			Muc_user ->
				Muc#muc_online_room.pid ! {http_del_user,Muc_user}
			end,
			catch remove_presence_a(Server,U,Muc_id,Domain) 
			end,Users)
	end.	

make_muc_persistent() ->
	jlib:iq_to_xml(
	 #iq{type = set, sub_el = [
    #xmlel{name = <<"query">>,
		attrs = [{<<"xmlns">>,?NS_MUC_OWNER}], children = [
		#xmlel{name = <<"x">>,attrs = [{<<"xmlns">>,?NS_XDATA},{<<"type">>,<<"submit">>}], 
			children = [
			 #xmlel{name = <<"field">>,attrs = [{<<"var">>,<<"FORM_TYPE">>}],
					children = [#xmlel{name = <<"value">>,attrs = [],children = [{xmlcdata,<<"http://jabber.org/protocol/muc#roomconfig">>}]}]},
			 #xmlel{name = <<"field">>,attrs = [{<<"var">>,<<"muc#roomconfig_persistentroom">>}],
					children = [#xmlel{name = <<"value">>,attrs = [],children = [{xmlcdata,<<"1">>}]}]},
			 #xmlel{name = <<"field">>,attrs = [{<<"var">>,<<"muc#roomconfig_publicroom">>}],
					children = [#xmlel{name = <<"value">>,attrs = [],children = [{xmlcdata,<<"1">>}]}]}
			]}]}]}).

%%	[{xmlel,<<"value">>,[],[{xmlcdata,<<"http://jabber.org/protocol/muc#roomconfig">>}]}]
%%	#xmlel{name = <<"value">>,attrs = [],children = [{xmlcdata,<<"http://jabber.org/protocol/muc#roomconfig">>}]}

make_invite_iq(Users,Server) ->
	jlib:iq_to_xml(
	 #iq{type = set, sub_el = [
    #xmlel{name = <<"query">>,
		attrs = [{<<"xmlns">>,?NS_MUC_INVITE_V2}], children = 
        lists:map(fun(U) ->
            JID = jlib:jid_to_string({U,Server,<<"">>}),
    		#xmlel{name = <<"invite">>,attrs = [{<<"jid">>,JID}], 	children = []} end,Users) }]}).	

make_create_muc_iq() ->
    jlib:iq_to_xml(
       #iq{type = set, sub_el = [
        #xmlel{name = <<"query">>,
            attrs = [{<<"xmlns">>,?NS_CREATE_MUC}], children = []}]}).

make_register_muc_iq() ->
    jlib:iq_to_xml(
       #iq{type = set, sub_el = [
        #xmlel{name = <<"query">>,
            attrs = [{<<"xmlns">>,?NS_MUC_REGISTER}], children = []}]}).

make_del_register_muc_iq() ->
    jlib:iq_to_xml(
        #iq{type = set, sub_el = [
            #xmlel{name = <<"query">>,
                attrs = [{<<"xmlns">>,?NS_MUC_DEL_REGISTER}], children = []}]}).

    

handle_add_muc_users(Server,Muc_id,Domain,Jid) ->
    case catch mnesia:dirty_read(muc_online_room, {Muc_id, Domain}) of
    [] ->
        catch odbc_queries:insert_muc_users(Server,<<"muc_room_users">>,Muc_id,Jid#jid.luser,Jid#jid.lserver);
    [Muc] ->
        Muc#muc_online_room.pid ! {http_add_user,Jid}
    end.

check_muc_exist(Server,Muc) ->
    case catch ejabberd_odbc:sql_query(Server,
        [<<"select name from muc_room where name = '">>,Muc,<<"';">>]) of
    {selected,[<<"name">>],[[Muc]]} ->
        true;
    _ ->
        false
    end.
