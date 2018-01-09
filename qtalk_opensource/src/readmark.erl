-module(readmark).

-export([readmark_message/3,route_message/3]).

-include("jlib.hrl").
-include("logger.hrl").

-record(session, {sid, usr, us, priority, info, show}).

readmark_message(From,To,Packet) ->
	case xml:get_tag_attr_s(<<"read_type">>, Packet) of
	<<"0">> ->
		mark_all_msg(From,To,Packet);
	<<"1">> ->
		chat_readmark_msg(From,To,Packet);
	<<"2">> ->
		groupchat_readmark_msg(From,To,Packet);
	_ ->
		ok
	end.

mark_all_msg(From,To,Packet) ->
	Body = xml:get_subtag_cdata(Packet, <<"body">>),
	case rfc4627:decode(Body) of    
	{ok,{obj,Args},[]} ->
		Time = 
			case proplists:get_value("T",Args) of
			undefined ->
				proplists:get_value("t",Args);
			V ->
				V
			end,
		Time_msg = erlang:trunc(Time / 1000),
		update_all_msg(From,Time,Time_msg),
		route_message(From, To, Packet);
	_ ->
		ok
	end.
			
chat_readmark_msg(From,To,Packet) ->
	Body = xml:get_subtag_cdata(Packet, <<"body">>),
	LServer = To#jid.lserver,
	case rfc4627:decode(Body) of	
	{ok,Json,[]} ->
		lists:foreach(fun({obj,Args}) ->
				ID = proplists:get_value("id",Args),
				case str:str(ID,<<"http">>) of
				0 ->
					msg_id_queue:queue_in(ID,LServer);
				_ ->
					ok
				end	end,Json),
		route_message(From, To, Packet);	
	_ ->
		ok
	end.

update_msg_readmark(Host,IDs) ->
	catch asy_sql:update_readmark_by_id(Host,IDs).


groupchat_readmark_msg1(From,To,Packet) when From#jid.lserver == To#jid.lserver ->
	Body = xml:get_subtag_cdata(Packet, <<"body">>),
	User = From#jid.user,
	Server = From#jid.lserver,
	case catch rfc4627:decode(Body) of	
	{ok,Json,[]} ->
			Other_domain_Mucs = 
				lists:foldl(fun({obj,Args},Acc) ->
					Muc  = proplists:get_value("id",Args),
					Time = proplists:get_value("t",Args),
					Local_muc_domain = str:concat(<<"conference.">>,Server),
					Muc_Domain = 
						case proplists:get_value("domain",Args) of
						undefined ->
							Local_muc_domain;
						V ->
							V
						end,		
					case Muc_Domain =/= Local_muc_domain of
					true ->
						case proplists:get_value(Muc_Domain,Acc) of
						undefined ->
							Acc ++ [{Muc_Domain,[{obj,Args}]}];	
						Ls ->
							lists:keyreplace(Muc_Domain, 1, Acc,{Muc_Domain,Ls ++ [{obj,Args}]}) 
						end;
					_ ->
						update_muc_readmark(Server,Muc,User,Time),
						Acc
					end end,[],Json),
			lists:foreach(fun({Domain,Ls}) ->
%%					NewPacket = replace_packet_cdata(Packet,rfc4627:encode(Ls)),
				NewPacket = ejabberd_public:make_message_packet(<<"chat">>,rfc4627:encode(Ls),<<"">>,<<"1">>),
				case jlib:make_jid(<<"">>,Domain,<<"">>) of
				error ->
					ok;
				To_JID ->
					ejabberd_router:route(From,To_JID,NewPacket)
				end end,Other_domain_Mucs),
		route_message(From, To, Packet);
	_ ->
		ok
	end;
groupchat_readmark_msg1(From,To,Packet) ->
	 Body = xml:get_subtag_cdata(Packet, <<"body">>),
	 User = From#jid.user,
	 case rfc4627:decode(Body) of
	{ok,Json,[]} ->
		try
		LServer = str:substr(To#jid.lserver,12,size(To#jid.lserver)-11),
		lists:foreach(fun({obj,Args}) ->
			Muc  = proplists:get_value("id",Args),
			Time = proplists:get_value("t",Args),
			update_muc_readmark(LServer,Muc,User,Time) end,Json)
		catch _:_ ->
			ok
		end;
	_ ->
		ok
	end.

update_muc_readmark(Host,Muc,User,Time) ->
	catch asy_sql:update_user_muc_readmark(Host,Muc,User,Time).

update_all_msg(From,Time,TimeStamp) ->
	Time_msg = ejabberd_public:format_time(TimeStamp),
	Sql1 = [<<"update msg_history set read_flag = 1 where m_to = '">>,From#jid.luser, 
			<<"' and read_flag = 0 and create_time < '">>,Time_msg,<<"';">>],
	Sql2 = [<<"update muc_room_users set date = ">>,integer_to_binary(Time),<<" where username = '">>,From#jid.luser, <<"' and date < ">>,integer_to_binary(Time),<<";">>],
	catch asy_sql:run_sql(From#jid.lserver,Sql1),
	catch asy_sql:run_sql(From#jid.lserver,Sql2).

route_message(From, To, Packet) ->
	LUser = To#jid.luser,
	LServer = To#jid.lserver, 
	PrioRes = ejabberd_sm:get_user_present_resources(LUser, LServer),
	case catch lists:max(PrioRes) of
	{Priority, _R}  when is_integer(Priority), Priority >= 0 ->
			lists:foreach(fun ({P, R}) when P == Priority ->
				LResource = jlib:resourceprep(R),
				USR = {LUser, LServer, LResource},
				case mnesia:dirty_index_read(session, USR, #session.usr)  of
				[] ->
				     ok; % Race condition
				Ss ->
				    Session = lists:max(Ss),
				    Pid = element(2, Session#session.sid),
				    Pid ! {route, From, To, Packet}
				end;
				({_Prio, _Res}) -> ok
				end,
			PrioRes);
	_ ->
		ok
	end.

replace_packet_cdata(Packet,New_CData) ->
	case xml:get_subtag(Packet,<<"body">>) of
	false ->
		Packet;
	Body_Xml ->
		try 
			#xmlel{name = Name, attrs = Attrs,children = Els} = Body_Xml,
			NewEls = xml:remove_cdata(Els),
			Packet1 = xml:remove_subtags_by_name(Packet,<<"body">>),
			xml:append_subtags(Packet1,[#xmlel{name = Name, attrs = Attrs,children = [xml:to_xmlel({xmlcdata, New_CData})]}])
		catch _:_ ->
			Packet
		end
	end.

groupchat_readmark_msg(From,To,Packet) ->
	Body = xml:get_subtag_cdata(Packet, <<"body">>),
    LServer = To#jid.lserver,
    User = To#jid.user,
    case rfc4627:decode(Body) of
	{ok,Json,[]} ->
    	lists:foreach(fun({obj,Args}) ->
        	Muc  = proplists:get_value("id",Args),
            Time = proplists:get_value("t",Args),
            update_muc_readmark(LServer,Muc,User,Time) end,Json),
            route_message(From, To, Packet);
     _ ->
            ok
    end.

