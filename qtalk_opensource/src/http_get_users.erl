%%========================================================
%%获取用户信息接口
%%========================================================

-module(http_get_users).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("logger.hrl").
-include("http_req.hrl").
-include("ejabberd_extend.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _} = cowboy_req:method(Req),
	case Method of 
	<<"GET">> ->
	{ok, Req2} = get_echo(Method,Req),
	{ok, Req2, State};
   	<<"POST">> ->
   	HasBody = cowboy_req:has_body(Req),
   	{ok, Req2} = post_echo(Method, HasBody, Req),
	{ok, Req2, State};
	_ ->
	{ok,Req2} = echo(undefined, Req),
	{ok, Req2, State}
	end.
    	
get_echo(<<"GET">>,Req) ->
	Res = 
		case http_utils:verify_user_key(Req) of
		true ->
			http_utils:gen_result(true,0,get_department());
		_ ->
           	http_utils:gen_result(false, 1, <<"Not found Mac_Key">>)
		end,
		cowboy_req:reply(200, [
			{<<"content-type">>, <<"text/plain; charset=utf-8">>}
		], Res, Req);
get_echo(_,Req) ->
	cowboy_req:reply(405, Req).

echo(undefined, Req) ->
    cowboy_req:reply(400, [], <<"Missing parameter.">>, Req);
echo(Echo, Req) ->
    cowboy_req:reply(200, [
			        {<<"content-type">>, <<"text/plain; charset=utf-8">>}
	    			    ], Echo, Req).

post_echo(<<"POST">>, true, Req) ->
   	{ok, _, _} = cowboy_req:body_qs(Req),
	Res = 
		case http_utils:verify_user_key(Req) of
		true ->
			get_department();
		_ ->
			[]
		end,
	echo(http_utils:gen_result(true,0,Res), Req);
post_echo(<<"POST">>, false, Req) ->
    cowboy_req:reply(400, [], <<"Missing Post body.">>, Req);
post_echo(_, _, Req) ->
    cowboy_req:reply(405, Req).

terminate(_Reason, _Req, _State) ->
	ok.

get_department() ->
	case ets:lookup(cache_info,<<"list_depts">>) of
	[Cache_info] when is_record(Cache_info,cache_info) ->
		Cache_info#cache_info.cache;
	_ ->
		[]
	end.

