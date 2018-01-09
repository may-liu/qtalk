-module(ejabberd_public).

-include("qunar_ejabberd_extend.hrl").
-include("jlib.hrl").
-include("logger.hrl").

-export([get_user_nick/1,make_message_packet/4,get_user_room_rescource/5,make_sent_packet/2]).
-export([is_conference_server/1,set_redis_user_key/6,clear_redis_user_key/3]).
-export([get_pg_default_val/2,make_revoke_packet/3,judge_spec_jid/2]).
-export([is_local_host/2,format_time/1]).
-export([check_user_reg_muc/3,pg2timestamp/1,clear_ets_muc_room_users/3]).
-export([send_http_offline_msg/2,get_xml_attrs_id/1,get_xml_attrs_to/2,get_xml_attrs_from/2]).
-export([get_sub_xmlns_name/1,to_integer/1,check_virtual_JID/3,tokens_jid/1]).

get_user_nick(User) ->
	case catch ets:lookup(roster_name_nick,User) of
	[{_,Name,_}] -> 
		Name;
	_ ->
        subscription:get_subscription_cn_name(User)
	end.

make_message_packet(Type,Msg,Extend_Info,undefined) ->
	Bid = list_to_binary("http_" ++ integer_to_list(random:uniform(65536)) ++ integer_to_list(mod_time:get_exact_timestamp())),
	xml:to_xmlel(
			{xmlel	,<<"message">>,	[{<<"type">>,Type}],
				[{xmlel,<<"body">>,[{<<"id">>,Bid},{<<"msgType">>,<<"1">>},{<<"extendInfo">>,Extend_Info}],[{xmlcdata, Msg}]}]});
make_message_packet(Type,Msg,Extend_Info,Msg_Type) ->
	Bid = list_to_binary("http_" ++ integer_to_list(mod_time:get_exact_timestamp())),
	xml:to_xmlel(
			{xmlel	,<<"message">>,	[{<<"type">>,Type}],
				[{xmlel,<<"body">>,[{<<"id">>,Bid},{<<"msgType">>,Msg_Type},{<<"extendInfo">>,Extend_Info}],[{xmlcdata, Msg}]}]}).

make_revoke_packet(ID,From,To) ->
	{{Year, Month, Day},{Hour, Minute, Second}} = calendar:now_to_universal_time(os:timestamp()),
	E_info = rfc4627:encode({obj, [{<<"messageId">>, ID}, {<<"fromId">>, jlib:jid_to_string(From)}]}),
	xml:to_xmlel(
			{xmlel	,<<"message">>,	[{<<"type">>,<<"revoke">>},{<<"to">>,jlib:jid_to_string(To)}],
				[{xmlel,<<"body">>,[{<<"id">>,ID},{<<"msgType">>,<<"-1">>},{<<"extendInfo">>,E_info}],
					[{xmlcdata, <<"[撤销一条消息]"/utf8>>}]},
				 {stime,<<"stime">>,[{<<"xmlns">>, ?NS_TIME91},
				 					 {<<"stamp">>,
						iolist_to_binary(io_lib:format("~4..0w~2..0w~2..0wT~2..0w:~2..0w:~2..0w",[Year, Month, Day, Hour, Minute,Second]))}
									 ],[]}]}).

make_sent_packet(JID,ID) ->
   xml:to_xmlel(
		{xmlel ,<<"message">>, [{<<"type">>,<<"mstat">>},{<<"to">>,jlib:jid_to_string(JID)}],
		             [{xmlel,<<"body">>,[{<<"id">>,ID},{<<"stat">>,<<"sent">>}],[]}]}).

get_user_room_rescource(Server,User,Host,Room,Domain) when Server =:= Domain ->
	case mod_muc_room:muc_user_online_rescource(Host,User,Room) of
    [] ->
    	case mnesia:dirty_read(muc_online_room, {Room,Domain}) of
        [M] when is_record(M,muc_online_room) ->
        	Pid = M#muc_online_room.pid,
            case gen_fsm:sync_send_all_state_event(Pid,{get_muc_user_rescource,User,Host}) of
            {ok,Ret} ->
            	Ret;
             _ ->
                []
             end;
          _ ->
                []
        end;
	N when is_list(N) ->
    	N;
    _ ->
        []
    end;
get_user_room_rescource(Server,User,Host,Room,Domain)->
	case ejabberd_sm:get_user_resources(User,Host) of
	[] ->
		[<<"">>];
	L ->
		L
	end.

judge_version_to_login(Resource) ->
	case str:tokens(Resource,<<"]">>) of
	[] ->
   		true;	
	[Resource] ->
		true;
	L when is_list(L) ->
		L1th = lists:nth(1,L),
		Version = str:substr(L1th,3,size(L1th)),
		case catch ets:lookup(black_version,Version) of
		[{Version,_}] ->
			false;
		_ ->
			true
		end;
	_ ->
		true
	end.

is_conference_server(Server) ->
	str:str(Server,<<"conference">>) =/= 0.

set_redis_user_key(Server,User,Resource,Key,Mac_key,Time) ->
    catch redis_link:hash_set(Server,1,User,Resource,Key),
    catch redis_link:expire_time(Server,1,User,Time),
    catch redis_link:hash_set(Server,2,User,Key,Mac_key),
    catch redis_link:expire_time(Server,2,User,Time).

clear_redis_user_key(Server,User,Resource) ->
    case catch redis_link:hash_get(Server,1,User,Resource) of
    {ok,undefined} ->
                    ok;
    {ok,Key} ->
                    redis_link:hash_del(Server,1,User,Resource),
                    redis_link:hash_del(Server,2,User,Key);
     _ ->
                    ok
     end.

get_pg_default_val(null,Default) ->
	Default;
get_pg_default_val(V,_) ->
	V.

judge_spec_jid(From,To) ->
	if From =:= <<"ops-robot">> orelse From =:= <<"qunar-message">> orelse To =:= <<"ops-robot">> orelse To =:= <<"qunar-message">> ->
		true;
	true ->
		false
	end.

is_local_host(Local,Server) ->
	Server =:= Local orelse Server =:= str:concat(<<"conference.">>,Local).

format_time(Time) when is_binary(Time)->
	handle_time(binary_to_integer(Time));
format_time(Time) when is_integer(Time)->
	handle_time(Time);
format_time(Time) when is_list(Time) ->
	handle_time(list_to_integer(Time));
format_time(Time) ->
	<<"error">>.

handle_time(Time) ->
	case Time > 3000000000 of
	true ->
		Msec = Time rem 1000,
		Time1 = Time div 1000,
		do_format_time(Time1,Msec);
	_ ->
		do_format_time(Time,0)
	end.

do_format_time(Time,Msec) ->
	{{Y,M,D},{H,Mi,S}} = mod_time:timestamp_to_datetime(Time),
	list_to_binary(
			[integer_to_list(Y),"-",integer_to_list(M),"-",integer_to_list(D)," ",
				       integer_to_list(H),":",integer_to_list(Mi),":",integer_to_list(S),msec_time(Msec),integer_to_list(Msec)]).


msec_time(Msec) when Msec > 100 ->
	".";
msec_time(Msec) when Msec > 10 ->
	".0";
msec_time(Msec) ->
	".00".

check_user_reg_muc(Server,Muc,User) ->
	case catch ejabberd_odbc:sql_query(Server,
			[<<"select host from muc_room_users where username = '">>,User,<<"' and muc_name = '">>,Muc,<<"';">>]) of
	{selected,  _ , []}  ->
		false;
	{selected,  _ , _ } ->
		true;
	_ ->
		false
	end.

pg2timestamp(Time) when is_binary(Time)->
	do_pg2timestamp(binary_to_integer(Time));
pg2timestamp(Time) when is_list(Time)->
	do_pg2timestamp(list_to_integer(Time));
pg2timestamp(Time) when is_integer(Time) ->
	do_pg2timestamp(Time);
pg2timestamp(Time)  ->
	 <<"error">>.
	
do_pg2timestamp(T) ->
	Time = 
		if T > 1000000000000 ->
			T /1000;
		true ->
			T/1
		end,
	Str = str:concat(float_to_binary(Time),<<")">>),
	str:concat(<<"to_timestamp(">>,Str).
		

clear_ets_muc_room_users(Muc,User,Server) ->
	case catch ets:lookup(muc_users,Muc) of
	[{_,UL}] when is_list(UL) andalso  UL /= [] ->
		case lists:delete({User,Server},UL) of
		UL ->
			ok;
		UDL when is_list(UDL) ->
			case UDL of
			[] ->
				ets:delete(muc_users,Muc);
			_ ->
				ets:insert(muc_users,{Muc,UDL})
			end;
		_ ->
			ok
		end;
	_ ->
		ok
	end.

	
send_http_offline_msg(Server,Body) ->
	Url = "http://xxxxxxxxxxxxxx.send.qunar",
    Header = [],
    Type = "application/json",
    HTTPOptions = [],
    Options = [],
    case catch http_client:http_post(Server,Url,Header,Type,Body,HTTPOptions,Options) of
    {ok, {_Status,_Headers, Res}} ->
     	case rfc4627:decode(Res) of
        {ok,{obj,Args},_} ->
			?DEBUG("res ~p ~n",[Args]),
       % 	Data = proplists:get_value("data",Args),
        %    {obj,Urls} = Data,
         %   proplists:get_value(binary_to_list(Body),Urls);
			ok;
         _ ->
         	ok
         end;
      _ ->
         ok
      end.

get_xml_attrs_id(Packet) ->
    case  xml:get_attr(<<"id">>,Packet#xmlel.attrs) of
    false ->
            integer_to_binary(mod_time:timestamp());
    {Value,I} ->
            I
    end.

get_xml_attrs_to(Packet,DT) ->
    case  xml:get_attr(<<"to">>,Packet#xmlel.attrs) of
    {Value,To} ->
        To;
    _ ->
        To = jlib:jid_to_string(jlib:make_jid(DT)),
        ?INFO_MSG("To ~p ~n",[To]),
        To
    end.
get_xml_attrs_from(Packet,DF) ->
    case  xml:get_attr(<<"from">>,Packet#xmlel.attrs) of
    {Value,From} ->
            From;
    _ ->
        JID = jlib:jid_to_string(jlib:make_jid(DF)),
        ?INFO_MSG("From ~p ~n",[JID]),
        JID
    end.


get_sub_xmlns_name(#xmlel{children = Els}) ->
    get_sub_xmlns_name1(Els).

get_sub_xmlns_name1( [El | Els]) ->
    case is_record(El,xmlel) of
    true ->
        case xml:get_attr(<<"xmlns">>, El#xmlel.attrs) of
        {value, XMLNS} ->
            {El#xmlel.name,XMLNS};
        _ -> get_sub_xmlns_name1(Els)
        end;
    _ ->
        get_sub_xmlns_name1(Els)
    end;
get_sub_xmlns_name1([]) -> false.

to_integer(Str) when is_binary(Str) ->
    binary_to_integer(Str);
to_integer(Str) when is_list(Str) ->
    list_to_integer(Str);
to_integer(Str) when is_integer(Str) ->
    Str;
to_integer(_) ->
    0.

check_virtual_JID(El,JID,FromJID) ->
    case catch ets:lookup(virtual_list,FromJID#jid.luser) of
    [{_,L}] when is_list(L) ->
        do_check_virtual_jid(lists:member({JID#jid.luser,JID#jid.lserver},FromJID),FromJID,El);
    _ ->
        'invalid-from'
    end.
    
do_check_virtual_jid(true,FromJID,El) ->
    log_jid_and_msg_id_info(FromJID,El);
do_check_virtual_jid(_,FromJID,_) ->
    'invalid-from'. 

log_jid_and_msg_id_info(FromJID,El) ->
    case catch xml:get_tag_attr_s(<<"id">>,xml:get_subtag(El,<<"body">>)) of
    Msgid when is_binary(Msgid) ->
       case catch ejabberd_odbc:sql_query(FromJID#jid.lserver,
            [<<"insert into virtual_user_msgid_relation(realuser,realserver,msgid) values('">>,
                    FromJID#jid.luser,<<"','">>,FromJID#jid.lserver,<<"','">>,Msgid,<<"');">>]) of
        {updated,1} ->
                El;
        _ ->
            'invalid-from'
        end;
    _ ->
        'invalid-from'
    end.

tokens_jid(Jid) ->
    case catch str:tokens(Jid,<<"@">>) of         
    [Jid] ->
        Jid;
    [] ->
        Jid;
    L when is_list(L) ->
        lists:nth(1,L);
    _ ->
        Jid
    end.
    

make_msg_id() ->
	list_to_binary("http_" ++ uuid:to_string(uuid:random())).
