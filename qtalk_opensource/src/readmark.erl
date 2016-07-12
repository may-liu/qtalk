-module(readmark).

-export([readmark_message/3]).

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
%%			update_msg_readmark(LServer,ID)
			msg_id_queue:queue_in(ID,LServer)
		   	end,Json),
		route_message(From, To, Packet);	
	_ ->
		ok
	end.

update_msg_readmark(Host,ID) ->
	catch async_sql:update_readmark_by_id(Host,ID).

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

update_muc_readmark(Host,Muc,User,Time) ->
	catch async_sql:update_user_muc_readmark(Host,Muc,User,Time).


update_all_msg(From,Time,Time_msg) ->
    Sql1 = [<<"update msg_history set read_flag = 1 where m_to = '">>,From#jid.luser,
	        <<"' and read_flag = 0 and m_timestamp < ">>,integer_to_binary(Time_msg),<<";">>],
	Sql2 = [<<"update muc_room_users set date = ">>,integer_to_binary(Time),<<" where username = '">>,From#jid.luser, <<"';">>],
		?DEBUG("Sql ~p ~n",[Sql1]),
	catch async_sql:run_sql(From#jid.lserver,Sql1),
	catch async_sql:run_sql(From#jid.lserver,Sql2).

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
