%% Feel free to use, reuse and abuse the code in this file.

-module(http_get_adjacentmsg).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-include("ejabberd.hrl").
-include("logger.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _} = cowboy_req:method(Req),
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
	Servers = ejabberd_config:get_myhosts(),
	LServer = lists:nth(1,Servers),
	case rfc4627:decode(Body) of
	{ok,{obj,Args} ,[]}  ->
			User = proplists:get_value("user", Args),
			Key = proplists:get_value("key", Args),
			Res =
%%		case http_utils:do_verify_user_key(LServer,User,Key) of	
				case true of
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
									case catch get_msgs(LServer,From,To,integer_to_binary(Time)) of
									{selected, _ , SRes} when is_list(SRes) ->
										lists:flatmap(fun([F,T,B,Ti]) ->
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
								case catch get_muc_msgs(LServer,Muc,integer_to_binary(Time)) of
								{selected, _ , SRes} when is_list(SRes) ->
									lists:flatmap(fun([M,N,B,Ti]) ->
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
	A ->
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

get_msgs(LServer,From,To,Time) ->
	ejabberd_odbc:sql_query(LServer,
			[<<"select a.m_from,a.m_to,a.m_body,a.m_timestamp from  
				((select m_from ,m_to,m_body,m_timestamp from msg_history where 
				  	(m_from,m_to) in (('">>,From,<<"','">>,To,<<"'),('">>,To,<<"','">>,From,<<"')) 
						and m_timestamp <= ">>,Time,<<" order by m_timestamp desc limit 5) 
			 union 
			    (select m_from,m_to,m_body,m_timestamp from msg_history where 
			 		(m_from,m_to) in (('">>,From,<<"','">>,To,<<"'), ('">>,To,<<"','">>,From,<<"')) 
						and m_timestamp >">>,Time,<<" order by m_timestamp asc limit 5)) as a order by a.m_timestamp asc">>]). 

get_muc_msgs(LServer,Muc,Time) ->
	ejabberd_odbc:sql_query(LServer,
	[<<"select a.muc_room_name, a.nick,a.packet,a.m_timestamp from  
			((select muc_room_name,nick,packet,m_timestamp from muc_room_history where 
			 	 muc_room_name =  '">>,Muc,<<"' and  m_timestamp > ">>,Time,<<" order by m_timestamp limit 5)
	 union (select muc_room_name,nick,packet,m_timestamp from muc_room_history where 
		 		 muc_room_name =  '">>,Muc,<<"' and  m_timestamp < ">>, Time,<<" order by m_timestamp desc limit 5))
		  as a order by a.m_timestamp asc; ">>]).
