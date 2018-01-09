%% Feel free to use, reuse and abuse the code in this file.
-module(http_log_dump_url).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("logger.hrl").
-include("ejb_http_server.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _ } = cowboy_req:method(Req),
    catch ejb_monitor:monitor_count(<<"http_log_dump_url">>,1),
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
	{ok,Json,[]}  ->
		Rslt =
			%%case http_utils:verify_user_key(Req) of
			case true of
			true ->
				log_dump_url(Json);
			false ->
				http_utils:gen_result(false, 1, <<"Tkey check error">>,<<"">>)
			end,
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

log_dump_url(Json) ->
	lists:foreach(fun({obj,Args}) ->
    	Url  = proplists:get_value("url",Args),	
		User = proplists:get_value("user",Args),
		pg_odbc:sql_query(<<"ejb_http_server">>,	
				[<<"insert into win_dump_url(user_name,dump_url) values ('">>,User,<<"','">>,Url,<<"');">>])
		end,Json),
	http_utils:gen_result(true, 0, <<"">>,<<"Log dump_url sucess.">>).

