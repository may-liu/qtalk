-module(revoke).

-export([revoke_message/3,revoke_groupchat_message/4]).

-include("jlib.hrl").
-include("logger.hrl").

revoke_message(From,To,Packet) ->
	Type = xml:get_tag_attr_s(<<"type">>, Packet),
	case  ejabberd_public:is_conference_server(From#jid.lserver) of
	true ->
		ok;
	_ ->
		revoke_chat_message(From,To,Packet)
	end,
	readmark:route_message(From,To,Packet).	

revoke_chat_message(From,To,Packet) ->
%	#xmlel{name = Name, attrs = Attrs, children = Els} = Packet,
	Args = get_revoke_message_args(Packet),
	Msg_id = proplists:get_value("messageId",Args),		
    
    {MegaSecs, Secs,MicroSec} = os:timestamp(),
    Time = MegaSecs * 1000000 + Secs,

	Revoke_pkt = ejabberd_public:make_revoke_packet(Msg_id,From,To,{MegaSecs, Secs,MicroSec}),
%	_fromId = proplists:get_value("fromId",Args),
%	_message = proplists:get_value("message",Args),
	update_msg_by_id(From#jid.lserver,From,To,Revoke_pkt,Msg_id,Time).


revoke_groupchat_message(Server,From,To,Packet) ->
%%	#xmlel{name = Name, attrs = Attrs, children = Els} = Packet,
	Args = get_revoke_message_args(Packet),
	Msg_id = proplists:get_value("messageId",Args),
    {MegaSecs, Secs,MicroSec} = os:timestamp(),
	Revoke_pkt = ejabberd_public:make_revoke_packet(Msg_id,From,To,{MegaSecs, Secs,MicroSec}),
    Time = MegaSecs * 1000000 + Secs,
	?DEBUG("revoke muc message ~p,Msg_id ~p  ~n",[Revoke_pkt,Msg_id]),
	update_muc_msg_by_id(Server,From,To,Revoke_pkt,Msg_id,Time).

get_revoke_message_args(Packet) ->
	Body = xml:get_subtag_cdata(Packet, <<"body">>),
	case rfc4627:decode(Body) of  
  	{ok,{obj,Args},[]} ->
		Args;
	_ ->
		[]
	end.	
		
update_msg_by_id(Server,From,To,Packet,Msg_id,ETime) ->
	case catch ejabberd_odbc:sql_query(Server,
		[<<"select m_from,m_to,m_body ,msg_id ,extract(epoch from create_time)::bigint from msg_history where msg_id = '">>,Msg_id,<<"';">>]) of
	{selected, _ , [[F,T,B,ID,Time]]}  ->
		Time1 = mod_time:get_timestamp(),
		Time2 = binary_to_integer(Time),
		if (Time1 - Time2 < 120) ->
			case catch ejabberd_odbc:sql_query(Server,
				[<<"insert into revoke_msg_history(m_from,m_to,m_body,msg_id) values ('">>,F,<<"','">>,T,<<"','">>,
					ejabberd_odbc:escape(B),<<"','">>,ID,<<"');">>]) of
				{updated,_} ->
					 case catch ejabberd_odbc:sql_query(Server,
						 [<<"update msg_history set m_from = '">>,ejabberd_odbc:escape(From#jid.luser),
				 			<<"',from_host = '">>,ejabberd_odbc:escape(From#jid.lserver),
				 			<<"',m_to = '">>,ejabberd_odbc:escape(To#jid.luser),
							<<"',to_host = '">>,ejabberd_odbc:escape(To#jid.lserver),
							<<"',m_body = '">>,ejabberd_odbc:escape( xml:element_to_binary(Packet)),
					 		<<"',create_time = ">>,ejabberd_public:pg2timestamp(ETime),
                            <<" where msg_id = '">>,Msg_id,<<"';">>]) of
					 {updated,_} ->
					 	ok;
					  Err ->
					  	    ?DEBUG("Error ~p ~n",[Err])
					  end;
				_ ->
					ok
				end;
		true ->
			ok
		end;
	_ ->
		ok
	end.

update_muc_msg_by_id(Server,From,To,Packet,Msg_id,ETime) ->
	case catch ejabberd_odbc:sql_query(Server,
		[<<"select nick,muc_room_name,packet,msg_id,extract(epoch from create_time)::bigint from muc_room_history where msg_id = '">>,
			Msg_id,<<"';">>]) of
	{selected, _ , [[N,M,P,ID,Time]]}  ->
		Time1 = mod_time:get_timestamp(),
		Time2 = binary_to_integer(Time),
        Nick1 = ejabberd_public:get_user_nick(From#jid.luser),
		case  (Time1 - Time2 < 120 andalso (Nick1 =:= N orelse Nick1 =:= From#jid.luser)) of
        true ->
			case catch ejabberd_odbc:sql_query(Server,
				[<<"insert into revoke_msg_history(m_from,m_to,m_body,msg_id) values ('">>,N,<<"','">>,M,<<"','">>,
					ejabberd_odbc:escape(P),<<"','">>,ID,<<"');">>]) of
				{updated,_} ->
					catch  ejabberd_odbc:sql_query(Server,
					[<<"update muc_room_history set packet = '">>,ejabberd_odbc:escape( xml:element_to_binary(Packet)),
					 <<"',create_time = ">>,ejabberd_public:pg2timestamp(ETime),<<" where msg_id = '">>,Msg_id,<<"';">>]),
                    true;
				_ ->
					true
				end;
		_  ->
			false
		end;
	_ ->
	    false
	end.
