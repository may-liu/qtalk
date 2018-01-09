%% Feel free to use, reuse and abuse the code in this file.

-module(http_get_user_status).
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-include("ejabberd.hrl").
-include("logger.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, Req2} = cowboy_req:method(Req),
	case Method of 
	<<"GET">> ->
	    {Host,Req3} =  cowboy_req:host(Req),
		{ok, Req4} = echo(<<"No Get Method!">>,Req),
		{ok, Req4, State};
	<<"POST">> ->
		HasBody = cowboy_req:has_body(Req),
		{ok, Req3} = post_echo(Method, HasBody, Req),
		{ok, Req3, State};
	_ ->
		{ok,Req3} = echo(undefined, Req),
		{ok, Req3, State}
	end.
post_echo(<<"POST">>,true,Req) ->	
	{ok, PBody, _} = cowboy_req:body(Req),
    {Host,_} =  cowboy_req:host(Req),
	Header = cowboy_req:get(headers,Req),
	Body = 
		case catch proplists:get_value(<<"content-encoding">>,Header) of 
		<<"gzip">> ->
			zlib:gunzip(PBody);
		_ ->
			PBody
		end,	
	Servers = ejabberd_config:get_myhosts(),
	LServer = lists:nth(1,Servers),
	Res = 
		case catch rfc4627:decode(Body) of
		{ok,{obj,Args},[]}  ->
			case proplists:get_value("type",Args) of
			<<"exact">> ->
				get_exact_user_status(LServer,Args);
			_ ->
				http_utils:gen_result(false, <<"1">>, <<"no allow get user status">>,[])
			end;
		_ ->
			case Body of 
			<<"require=all">> ->
				get_user_status(LServer,Body);
			_ ->
				http_utils:gen_result(false, <<"1">>, <<"no allow get user status">>,[])
			end
		end,	
	cowboy_req:reply(200, [{<<"content-type">>, <<"text/json; charset=utf-8">>}], Res, Req);
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

get_user_status(LServer,Body) ->
	Res = 
		lists:map(fun({N,J}) ->
			{obj,[{"U",J},{"S",
				case ejabberd_sm:get_user_away_rescources(J,LServer) of
				[] ->
					6;
				[<<"none">>] ->
					0;
				_ ->
					1
				end}]} end,	
				case catch ets:tab2list(nick_name) of
				L when is_list(L) ->
					L;
				_ ->
					[]
				end),
		http_utils:gen_result(true, <<"1">>, <<"">>,Res).

get_exact_user_status(LServer,Body) ->
	case proplists:get_value("users",Body) of
	undefined ->
		http_utils:gen_result(false, <<"1">>, <<"not find users">>,[]);
	Users ->
		Res = 
			lists:map(fun({obj,[{"u",U}]}) ->
			case ejabberd_sm:get_user_resources(U,LServer) of
			[] ->
				{obj,[{u,U},{s,0}]};
			_ ->
				{obj,[{u,U},{s,6}]}
			end end,Users),
		http_utils:gen_result(true,<<"0">>,<<"">>,Res)
	end.
