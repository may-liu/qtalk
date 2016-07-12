%% Feel free to use, reuse and abuse the code in this file.
%%========================================================
%%获取组织架构接口（树状）
%%========================================================
-module(http_get_deps).

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
    {Method, _} = cowboy_req:method(Req),
	{Host,_} =  cowboy_req:host(Req),
	{ok, Req2} = get_echo(Method,Host,Req),
	{ok, cowboy_req:compact(Req2), State};
	_ ->
	{ok,Req2} = echo(undefined, Req),
	{ok, Req2, State}
	end.
    	
get_echo(<<"GET">>,_Host,Req) ->
	Res = 
		case http_utils:verify_user_key(Req) of
		true ->
			http_utils:gen_result(true,0,get_departments());
		_ ->
			http_utils:gen_result(false,1,<<"Not found Mac_Key">>)
		end,
	cowboy_req:reply(200, [
		{<<"content-type">>, <<"text/plain; charset=utf-8">>}
	], Res, Req);
get_echo(<<"Get">>,_,Req) ->
	cowboy_req:reply(405, Req).

echo(undefined, Req) ->
    cowboy_req:reply(400, [], <<"Missing parameter.">>, Req);
echo(Echo, Req) ->
    cowboy_req:reply(200, [
			        {<<"content-type">>, <<"text/plain; charset=utf-8">>}
	    			    ], Echo, Req).

terminate(_Reason, _Req, _State) ->
	ok.

get_departments() ->
	case ets:lookup(cache_info,<<"json_tree_depts">>) of
	[Json_tree_depts] when is_record(Json_tree_depts,cache_info) ->
		Json_tree_depts#cache_info.cache;
	_ ->
		[]
	end.	
