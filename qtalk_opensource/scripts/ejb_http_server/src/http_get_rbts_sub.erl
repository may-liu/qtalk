%% Feel free to use, reuse and abuse the code in this file.

-module(http_get_rbts_sub).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("logger.hrl").
-include("http_req.hrl").
-include("ejb_http_server.hrl").


init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _} = cowboy_req:method(Req),
    catch ejb_monitor:monitor_count(<<"http_subscription">>,1),
	case Method of 
	<<"GET">> ->
	    {Host,_Req} =  cowboy_req:host(Req),
		 Req1 = cowboy_req:reply(200, [ {<<"content-type">>, <<"text/json; charset=utf-8">>}], 
					http_utils:gen_result(false, <<"-1">>, <<"No Get Method">>,<<"">>) , Req),
		{ok, Req1, State};
	<<"POST">> ->
		HasBody = cowboy_req:has_body(Req),
		{ok, Req1} = post_echo(Method, HasBody, Req),
		{ok, Req1, State};
	_ ->
		{ok,Req1} = echo(undefined, Req),
		{ok, Req1, State}
	end.
    	
post_echo(<<"POST">>, true, Req) ->
    {ok, Body,_} = cowboy_req:body(Req),
	case rfc4627:decode(Body) of
	{ok,{obj,Json},[]} -> 
		case proplists:get_value("rbt_name",Json) of
		undefined ->
			cowboy_req:reply(200, [	{<<"content-type">>, <<"text/json; charset=utf-8">>}],
					http_utils:gen_result(false, <<"1">>, <<"">>,<<"no found rbt_name">>), Req);
		Rbt ->
			Rslt = 
				case check_rbt_auth(Rbt,Json)  of
				true ->
					get_rbt_sub(Rbt);
				false ->
					http_utils:gen_result(false, <<"-1">>, <<"Rbt auth error">>,<<"">>)
				end,
			cowboy_req:reply(200, [	{<<"content-type">>, <<"text/json; charset=utf-8">>}], Rslt, Req)
		end;
	_ ->
		 cowboy_req:reply(200, [ {<<"content-type">>, <<"text/json; charset=utf-8">>}], 
				 http_utils:gen_result(false, <<"-1">>, <<"Json format error">>,<<"">>), Req)
	end;
post_echo(<<"POST">>, false, Req) ->
	cowboy_req:reply(400, [], <<"Missing Post body.">>, Req);
post_echo(_, _, Req) ->
	cowboy_req:reply(405, Req).
										

echo(undefined, Req) ->
    cowboy_req:reply(400, [], <<"Missing parameter.">>, Req);
echo(Echo, Req) ->
    cowboy_req:reply(200, [
			        {<<"content-type">>, <<"text/json; charset=utf-8">>}
	    			    ], Echo, Req).

terminate(_Reason, _Req, _State) ->
	ok.

check_rbt_auth(Rbt,Json)  ->
	Password = 
		case proplists:get_value("password",Json,null) of
		<<>>  ->
			null;
		P ->
			P
		end,
	case catch ejb_odbc_query:check_rbts_auth(Rbt) of
	{selected,_, [[Password]]} ->
		true;
	_ ->
		false
	end.
	 	
get_rbt_sub(Rbt)->
	Users = 
		case catch  ets:select(user_rbts,[{#user_rbts{rbt = Rbt,user = '$1', _ = '_'},[], ['$1']}])  of
		L when is_list(L) ->
			L;
		_ ->
			[]
		end,
	Num = length(Users),
	Res = {obj,[{"num", Num},{"users",Users}]},
	http_utils:gen_result(true, <<"0">>, <<"">>,Res).
		
