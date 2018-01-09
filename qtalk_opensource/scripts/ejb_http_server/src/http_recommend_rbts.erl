%% Feel free to use, reuse and abuse the code in this file.

-module(http_recommend_rbts).
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("ejb_http_server.hrl").
-include("logger.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _ } = cowboy_req:method(Req),
    catch ejb_monitor:monitor_count(<<"http_recommend_rbts">>,1),
	case Method of 
	<<"GET">> ->
		{ok, Req1} = echo(<<"No Get Method!">>,Req),
		{ok, Req1, State};
	<<"POST">> ->
		HasBody = cowboy_req:has_body(Req),
		{ok, Req1} = post_echo(Method, HasBody, Req),
		{ok, Req1, State};
	_ ->
		{ok,Req1} = echo(undefined, Req),
		{ok, Req1, State}
	end.
post_echo(<<"POST">>,true,Req) ->	
	{ok, PBody, _} = cowboy_req:body(Req),
	Header = cowboy_req:get(headers,Req),
	Body = 
		case catch proplists:get_value(<<"content-encoding">>,Header) of 
		<<"gzip">> ->
			zlib:gunzip(PBody);
		_ ->
			PBody
		end,	
	case rfc4627:decode(Body) of
	{ok,{obj,Args},[]}  ->
		Type = proplists:get_value("type",Args),
		Res =	case Type of 
	   			1 ->
					get_recommend_rbts(<<"ejb_http_server">>,Args);
				_ ->
					http_utils:gen_result(true, <<"0">>,<<"">>,[])
				end,
		cowboy_req:reply(200, [{<<"content-type">>, <<"text/json; charset=utf-8">>}], Res, Req);
	_ ->
		cowboy_req:reply(200, [{<<"content-type">>, <<"text/json; charset=utf-8">>}], 
					http_utils:gen_result(false, <<"-1">>,<<"Json format error.">>,<<"">>), Req)
	end;
post_echo(<<"POST">>, false, Req) ->
	cowboy_req:reply(400, [], http_utils:gen_result(false, <<"-1">>,<<"Missing Post body.">>,<<"">>), Req);
post_echo(_, _, Req) ->
	cowboy_req:reply(405, Req).
										

echo(undefined, Req) ->
	cowboy_req:reply(400, [], http_utils:gen_result(false, <<"-1">>,<<"Missing Post body.">>,<<"">>), Req);
echo(Echo, Req) ->
    cowboy_req:reply(200, [
			        {<<"content-type">>, <<"text/plain; charset=utf-8">>}
	    			    ], http_utils:gen_result(true, <<"0">>,Echo,<<"">>), Req).

terminate(_Reason, _Req, _State) ->
	ok.

get_recommend_rbts(LServer,Json) ->
	Res =
		case catch pg_odbc:sql_query(LServer,
			[<<"select en_name,rbt_body,rbt_version from robot_info where recommend = 1;">>] ) of 
		{selected, _ , SRes} when is_list(SRes) ->	
			lists:map(fun([En,Body,Ve]) ->
				{obj,[{"rbt_name",En},{"rbt_body",Body},{"rbt_ver",http_utils:to_integer(Ve)}]} end,SRes);
		_ ->
			[]
		end,
	http_utils:gen_result(true, <<"0">>,<<"">>,Res).
	
