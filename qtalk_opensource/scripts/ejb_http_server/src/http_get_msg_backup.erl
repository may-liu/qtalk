%% Feel free to use, reuse and abuse the code in this file.
-module(http_get_msg_backup).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("logger.hrl").
-include("http_req.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _} = cowboy_req:method(Req),
    catch ejb_monitor:monitor_count(<<"http_get_msg_backup">>,1),
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
		{ok,Req2} = echo(undefined, Req),
		{ok, Req2, State}
	end.
get_echo(<<"GET">>,_,Req) ->
	cowboy_req:reply(200, [
	           {<<"content-type">>, <<"text/plain; charset=utf-8">>}], 
				http_utils:gen_result(false, 1, <<"no get method">>,[]) , Req).

post_echo(<<"POST">>, true, Req) ->
	{ok, Body, _} = cowboy_req:body(Req),
	case rfc4627:decode(Body) of
	{ok,{obj,Args} ,[]}  ->
		Ret =
			case http_utils:verify_user_key(Req) of	
			true ->
					{User,_} = cowboy_req:qs_val(<<"u">>, Req),
			 		do_get_backup_msg(User,Args);
			_ ->
				http_utils:gen_result(false, 2, <<"Tkey check error">>,<<"">>)	
			end,
		cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain; charset=utf-8">>}],Ret, Req);
	_ ->
		Rslt = <<"Body parse error">>,
		cowboy_req:reply(200,[{<<"content-type">>, <<"text/json; charset=utf-8">>}],
				http_utils:gen_result(false, 3, Rslt,<<"">>), Req)
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

do_get_backup_msg(User,Args) ->
	
	From = pg_odbc:escape(proplists:get_value("from",Args)),
	To = pg_odbc:escape(proplists:get_value("to",Args)),
	Start_Time = proplists:get_value("start_time",Args),
	End_Time = proplists:get_value("end_time",Args),
	Type = proplists:get_value("type",Args),
	case User =:= From of
	true ->
		Msg = get_msg_backup(Type,From,To,Start_Time,End_Time),
		http_utils:gen_result(true, 0, <<"">>,Msg);
	_ ->
		http_utils:gen_result(false,2,<<"u and from not match">>,<<"">>)
	end.


get_msg_backup(<<"1">>,From,To,Start_Time,End_Time) ->
      Time1 = ejb_public:format_time(Start_Time),
      Time2 = ejb_public:format_time(End_Time),
	case catch pg_odbc:sql_query(<<"ejb_http_server">>,
		[<<"select from_host,m_from,to_host,m_to,m_body from msg_history_backup where (m_from = '">>,From,<<"' or m_to = '">>,
            From,<<"')">>,<<" and create_time > '">>,Time1,	<<"' and create_time < '">>,Time2 ,<<"' order by id asc ;">>]) of
	 {selected,  _ , SRes } when is_list(SRes) ->
	 		lists:flatmap(fun([FH,F,TH,T,B]) ->
					[{obj, [{"From", F},{"From_Host",FH},{"To",T},{"To_Host",TH}, {"Msg", B}]}] end,SRes);
	 _ ->
	 	[]
	end;
get_msg_backup(_,From,To,Start_Time,End_Time) ->
    Time1 = ejb_public:format_time(Start_Time),
    Time2 = ejb_public:format_time(End_Time),
	case catch pg_odbc:sql_query(<<"ejb_http_server">>,
			[<<"select from_host,m_from,to_host,m_to,m_body from msg_history_backup where (m_from,m_to) in 
				(('">>,From,<<"','">>,To,<<"'),('">>, To,<<"','">>,From,<<"')) and create_time > '">>,
				Time1 ,<<"' and m_timestamp < '">>,	Time2 ,<<"' order by id asc ;">>]) of
	 {selected,  _ , SRes } when is_list(SRes) ->
	 		lists:flatmap(fun([FH,F,TH,T,B]) ->
					[{obj, [{"From", F},{"From_Host",FH},{"To",T},{"To_Host",TH}, {"Msg", B}]}] end,SRes);
	 _ ->
	 	[]
	end.
