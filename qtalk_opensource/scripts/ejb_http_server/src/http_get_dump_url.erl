%% Feel free to use, reuse and abuse the code in this file.
-module(http_get_dump_url).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("logger.hrl").
-include("ejb_http_server.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _ } = cowboy_req:method(Req),
    catch ejb_monitor:monitor_count(<<"http_get_dump_url">>,1),
	case Method of 
	<<"GET">> ->
	    {Host,_} =  cowboy_req:host(Req),
		{ok, Req1} = get_echo(Method,Host,Req),
		{ok, Req1, State};
	<<"POST">> ->
		HasBody = cowboy_req:has_body(Req),
		{ok, Req2} = post_echo(Method, HasBody, Req),
		{ok, Req2, State};
	_ ->
		{ok,Req3} = echo(undefined, Req),
		{ok, Req3, State}
	end.
    	
get_echo(<<"GET">>,_,Req) ->
		cowboy_req:reply(200, [
			{<<"content-type">>, <<"text/plain; charset=utf-8">>}
		], <<"No GET method">>, Req).

post_echo(<<"POST">>, true, Req) ->
	{ok, Body, _} = cowboy_req:body(Req),
	case rfc4627:decode(Body) of
	{ok,{obj,Args},[]}  ->
		Rslt =	get_dump_url(Args),
		cowboy_req:reply(200, [{<<"content-type">>, <<"text/json; charset=utf-8">>}], Rslt, Req);
	 _ ->
		Rslt = <<"Body parse error">>,
		cowboy_req:reply(200,[{<<"content-type">>, <<"text/json; charset=utf-8">>}],Rslt, Req)
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

get_dump_url(Args) ->
	Start_time = integer_to_binary(proplists:get_value("start_time",Args)+3600*8),
	End_time   = integer_to_binary(proplists:get_value("end_time",Args)+3600*8),
	Ret = 
		case catch pg_odbc:sql_query(<<"ejb_http_server">>,	
				[<<"select user_name,dump_url from win_dump_url where extract(epoch from date_trunc('second', dump_time)) > ">>,
					Start_time,<<" and extract(epoch from date_trunc('second', dump_time)) < ">>,End_time ,<<";">>]) of
		{selected,_,Res} when is_list(Res) ->
			lists:flatmap(fun([User,Url]) ->
				[{obj,[{"user",User},{"dump_url",Url}]}] end,Res);
		A ->
			?DEBUG("A ~p ~n",[A]),
			[]
		end,
	?DEBUG("Ret ~p ~n",[Ret]),
	http_utils:gen_result(true, 0, <<"">>,Ret).

