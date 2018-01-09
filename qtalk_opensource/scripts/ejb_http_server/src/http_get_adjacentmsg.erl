%% Feel free to use, reuse and abuse the code in this file.

-module(http_get_adjacentmsg).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-include("logger.hrl").
-export([get_msgs/3,get_muc_msgs/2]).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _} = cowboy_req:method(Req),
    catch ejb_monitor:monitor_count(<<"http_get_adjacent_msg">>,1),
	case Method of 
	<<"GET">> ->
    	{Host,_} =  cowboy_req:host(Req),
		{ok, Req2} = get_echo(Method,Host,Req),
		{ok, Req2, State};
	<<"POST">> ->
		HasBody = cowboy_req:has_body(Req),
		{ok, Req2} = post_echo(Method, HasBody, Req),	
		{ok, Req2, State};
	_ ->
		{ok,Req3} = echo(undefined, Req),
		{ok, Req3, State}
	end.

get_echo(<<"GET">>,_,Req) ->
	cowboy_req:reply(200, [{<<"content-type">>, <<"text/json; charset=utf-8">>}], <<"No GET method">>, Req).

post_echo(<<"POST">>, true, Req) ->
	{ok, Body, _} = cowboy_req:body(Req),
	case rfc4627:decode(Body) of
	{ok,{obj,Args} ,[]}  ->
			User = proplists:get_value("user", Args),
			Key = proplists:get_value("key", Args),
			Res =
		case http_utils:do_verify_user_key(User,Key) of	
%%			case true of
				true ->
					Time = proplists:get_value("time", Args),
					Flag = proplists:get_value("flag", Args),
					case Flag of 
					   	<<"0">> ->
							From = proplists:get_value("from", Args),
							To = proplists:get_value("to", Args),
							case User =:= From orelse User =:= To of
							true ->
								Res1 = 
									case catch get_msgs(From,To,integer_to_binary(Time)) of
									{selected, _ , SRes} when is_list(SRes) ->
										lists:flatmap(fun([F,T,B,Ti,_Id]) ->
										 [{obj,[{"F",F},{"T",T},{"B",B},{"D",Ti}]}] end,SRes);
									_ ->
										[]
									end,
						 		http_utils:gen_result(true, <<"0">>, <<"">>,Res1);
						 	_ ->
						 		http_utils:gen_result(false, <<"-1">>, <<"User and Key not match">>,<<"">>)
							end;
						<<"1">> ->
							Muc = proplists:get_value("muc", Args),
							Res1 =
								case catch get_muc_msgs(Muc,integer_to_binary(Time)) of
								{selected, _ , SRes} when is_list(SRes) ->
									lists:flatmap(fun([M,N,B,Ti,_ID]) ->
									 [{obj,[{"M",M},{"N",N},{"B",B},{"D",Ti}]}] end,SRes);
								_ ->
									[]
								end,
					 		http_utils:gen_result(true, <<"0">>, <<"">>,Res1);
						_ ->
						 	http_utils:gen_result(false, <<"-1">>, <<"No falg to choose">>,<<"">>)
						end;
					_ ->
						 http_utils:gen_result(false, <<"-1">>, <<"User and Key not match">>,<<"">>)
					end,
				cowboy_req:reply(200, [	{<<"content-type">>, <<"text/plain; charset=utf-8">>}],Res,Req);
	_ ->
		Res = <<"Body parse error">>,
		cowboy_req:reply(200,[{<<"content-type">>, <<"text/json; charset=utf-8">>}],
				http_utils:gen_result(false, <<"-1">>, Res,<<"">>), Req)
	end;
post_echo(<<"POST">>, false, Req) ->
    cowboy_req:reply(400, [], <<"Missing Post body.">>, Req);
post_echo(_, _, Req) ->
    cowboy_req:reply(405, Req).


echo(undefined, Req) ->
    cowboy_req:reply(400, [], <<"Missing parameter.">>, Req);
echo(Echo, Req) ->
    cowboy_req:reply(200, [
			        {<<"content-type">>, <<"text/plain; charset=utf-8">>}
	    			    ], Echo, Req).

terminate(_Reason, _Req, _State) ->
	ok.

get_msgs(From,To,Timestamp) ->
    Time = ejb_public:format_time(Timestamp),
	T = binary_to_integer(Timestamp),
	Month_ago = mod_time:get_timestamp() - 86400*30,
	if T > Month_ago -> 
		pg_odbc:sql_query(<<"ejb_http_server">>,
				[<<"select a.m_from,a.m_to,a.m_body,a.date_part,a.id from  
					((select m_from ,m_to,m_body,extract(epoch from create_time)::bigint,id from msg_history where 
					  	(m_from,m_to) in (('">>,From,<<"','">>,To,<<"'),('">>,To,<<"','">>,From,<<"')) 
							and create_time <= '">>,Time,<<"' order by create_time desc limit 5) 
				 union 
				    (select m_from,m_to,m_body,extract(epoch from create_time)::bigint,id from msg_history where 
				 		(m_from,m_to) in (('">>,From,<<"','">>,To,<<"'), ('">>,To,<<"','">>,From,<<"')) 
							and create_time > '">>,Time,<<"' order by create_time asc limit 5)) as a order by a.id asc">>]);
	true ->
		pg_odbc:sql_query(<<"ejb_http_server">>,
				[<<"select a.m_from,a.m_to,a.m_body,a.date_part,a.id from  
					((select m_from ,m_to,m_body,extract(epoch from create_time)::bigint,id from msg_history_backup where 
					  	(m_from,m_to) in (('">>,From,<<"','">>,To,<<"'),('">>,To,<<"','">>,From,<<"')) 
							and create_time <= '">>,Time,<<"' order by create_time desc limit 5) 
				 union 
				    (select m_from,m_to,m_body,extract(epoch from create_time)::bigint,id from msg_history_backup where 
				 		(m_from,m_to) in (('">>,From,<<"','">>,To,<<"'), ('">>,To,<<"','">>,From,<<"')) 
							and create_time > '">>,Time,<<"' order by create_time asc limit 5)) as a order by a.id asc">>])
	end.
			

get_muc_msgs(Muc,Timestamp) ->
    Time = ejb_public:format_time(Timestamp),
	Month_ago = mod_time:get_timestamp() - 86400*30,
	T = binary_to_integer(Timestamp),
	if T > Month_ago ->
		pg_odbc:sql_query(<<"ejb_http_server">>,
		[<<"select a.muc_room_name, a.nick,a.packet,a.date_part,a.id from  
				((select muc_room_name,nick,packet,extract(epoch from create_time)::bigint,id from muc_room_history where 
				 	 muc_room_name =  '">>,Muc,<<"' and  create_time > '">>,Time,<<"' order by create_time limit 5)
		 union (select muc_room_name,nick,packet,extract(epoch from create_time)::bigint,id from muc_room_history where 
			 		 muc_room_name =  '">>,Muc,<<"' and  create_time <= '">>, Time,<<"' order by create_time desc limit 5))
			  as a order by a.id asc; ">>]);
	true ->
		pg_odbc:sql_query(<<"ejb_http_server">>,
		[<<"select a.muc_room_name, a.nick,a.packet,a.date_part,a.id from  
				((select muc_room_name,nick,packet,extract(epoch from create_time)::bigint,id from muc_room_history_backup where 
				 	 muc_room_name =  '">>,Muc,<<"' and  create_time > '">>,Time,<<"' order by create_time limit 5)
		 union (select muc_room_name,nick,packet,extract(epoch from create_time)::bigint,id from muc_room_history_backup where 
			 		 muc_room_name =  '">>,Muc,<<"' and  create_time <= '">>, Time,<<"' order by create_time desc limit 5))
			  as a order by a.id asc; ">>])
	end.
		
