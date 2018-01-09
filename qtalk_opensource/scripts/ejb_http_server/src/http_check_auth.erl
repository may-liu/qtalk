%% Feel free to use, reuse and abuse the code in this file.
-module(http_check_auth).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("logger.hrl").
-include("ejb_http_server.hrl").

-export([check_token_md5/4]).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _ } = cowboy_req:method(Req),
    catch ejb_monitor:monitor_count(<<"http_check_auth">>,1),
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
		Rslt =	check_user_auth(Args),
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

check_user_auth(Args) ->
	Count = proplists:get_value("t",Args),
	User = proplists:get_value("u",Args),
	Token = proplists:get_value("k",Args),
	do_check_user_auth(User,Count,Token).

do_check_user_auth(User,Count,Token) ->
	case catch redis_link:redis_cmd(2,["HKEYS",User]) of
	{ok,undefined} ->
		http_utils:gen_result(false,<<"1">>,<<"">>,[]);
	{ok,L} when is_list(L) ->
		case check_token_md5_v2(Token,User,Count,L) of
		true ->
			http_utils:gen_result(true,<<"0">>,<<"">>,[]);
		_ ->
			http_utils:gen_result(false,<<"1">>,<<"">>,[])
		end;
	_ ->
		http_utils:gen_result(false,<<"1">>,<<"">>,[])
	end.


check_token_md5(Token,User,Count,L) when is_binary(Count) ->
        ?DEBUG("Token ~p ,USer ~p ,Count ~p ,L ~p ~n",[Token,User,Count,L]),
        case lists:filter(fun(I) ->
                V = list_to_binary([<<"u=">>,User,<<"&k=">>,ejb_public:md5_hex(str:concat(I,Count))]),
                ?DEBUG("md5 ~p ~n",[base64:encode(V)]),
                base64:encode(V) =:= Token end,L) =:= [] of
        true ->
                false;
        _ ->
                true
        end;
check_token_md5(Token,User,Count,L) ->
        false.
	

check_token_md5_v2(Token,User,Count,L) when is_binary(Count)->
	?DEBUG("Token ~p ,USer ~p ,Count ~p ,L ~p ~n",[Token,User,Count,L]),
	case lists:filter(fun(I) ->
		V = str:to_upper(ejb_public:md5_hex(str:concat(I,Count))),
		?DEBUG(" V ~p,md5 ~p ~n",[V,base64:encode(V)]),
		do_check_key(V,Token) end,L) =:= [] of
	true ->
		false;
	_ ->
		true
	end;
check_token_md5_v2(Token,User,Count,L) when is_integer(Count)->
	?DEBUG("Token ~p ,USer ~p ,Count ~p ,L ~p ~n",[Token,User,Count,L]),
	case lists:filter(fun(I) ->
		V = str:to_upper(ejb_public:md5_hex(str:concat(I,integer_to_binary(Count)))),
		?DEBUG(" V ~p,md5 ~p ~n",[V,base64:encode(V)]),
		do_check_key(V,Token) end,L) =:= [] of
	true ->
		false;
	_ ->
		true
	end;
check_token_md5_v2(_,_,_,_) ->
    false.

	
do_check_key(Key,Token) when is_binary(Key) ->
    Key =:= Token;
do_check_key(Key,Token) when is_list(Key) ->
    list_to_binary(Key) =:= Token;
do_check_key(_Key,_Token) ->
    false.


    	
