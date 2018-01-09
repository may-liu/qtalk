%% Feel free to use, reuse and abuse the code in this file.
-module(http_check_auth_v2).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("logger.hrl").
-include("ejb_http_server.hrl").

-export([check_token_md5/4,check_user_auth_v2/1]).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _ } = cowboy_req:method(Req),
    catch ejb_monitor:monitor_count(<<"http_check_auth_v2">>,1),
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
	case catch  rfc4627:decode(Body) of
	{ok,{obj,Args},[]}  ->
		Rslt =	check_user_auth_v2(Args),
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

check_user_auth_v2(Args) ->
	Ckey_base64 = proplists:get_value("ckey",Args),
    Ckey = base64:decode(Ckey_base64),
    CArgs = get_user_ckey_args(Ckey),
	User = proplists:get_value(<<"u">>,CArgs),
	Count = proplists:get_value(<<"t">>,CArgs),
	Token = proplists:get_value(<<"k">>,CArgs),
    
	do_check_user_auth(User,Count,Token).

do_check_user_auth(User,Count,Token) ->
	case catch redis_link:redis_cmd(2,["HKEYS",User]) of
	{ok,undefined} ->
		http_utils:gen_result(false,<<"1">>,<<"">>,[]);
	{ok,L} when is_list(L) ->
		case check_token_md5(Token,User,Count,L) of
		true ->
			http_utils:gen_result(true,<<"0">>,<<"">>,[]);
		_ ->
			http_utils:gen_result(false,<<"1">>,<<"">>,[])
		end;
	_ ->
		http_utils:gen_result(false,<<"1">>,<<"">>,[])
	end.
	

check_token_md5(Token,User,Count,L) ->
	case lists:filter(fun(I) ->
		V = list_to_binary(ejb_public:md5_hex(str:concat(I,Count))),
		V =:= str:to_lower(Token) end,L) =:= [] of
	true ->
		false;
	_ ->
		true
	end.
	
get_user_ckey_args(Ckey) ->
    L = str:tokens(Ckey,<<"&">>),
    lists:flatmap(fun(Args) ->
        case catch str:left(Args,2) of
        <<"u=">> ->
            User = str:substr(Args,3,(size(Args) - 2)),
            [{<<"u">>,User}];
        <<"k=">> ->
            Key = str:substr(Args,3,(size(Args) - 2)),
            [{<<"k">>,Key}];
        <<"t=">> ->
            T = str:substr(Args,3,(size(Args) - 2)),
            [{<<"t">>,T}];
        _ ->
            {}
        end end,L).
	
