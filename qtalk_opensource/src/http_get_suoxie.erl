%% Feel free to use, reuse and abuse the code in this file.
%%========================================================
%%获取用户缩写接口
%%========================================================
-module(http_get_suoxie).

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
	_ ->
	{ok,Req2} = echo(undefined, Req),
	{ok, Req2, State}
	end.
    	
get_echo(<<"GET">>,Req) ->
	Rslt = 
		case http_utils:verify_user_key(Req) of
		true ->
			case ets:lookup(cache_info,<<"abbreviates">>) of
			[Cache_info] when is_record(Cache_info,cache_info) ->
				Cache_info#cache_info.cache;
			_ ->
				[]
			end;
		_ ->
			[]
		end,

	cowboy_req:reply(200, [
		{<<"content-type">>, <<"text/plain; charset=utf-8">>}
	],http_utils:gen_result(true,0,Rslt), Req);
get_echo(_,Req) ->
	cowboy_req:reply(405, Req).

echo(undefined, Req) ->
    cowboy_req:reply(400, [], <<"Missing parameter.">>, Req);
echo(Echo, Req) ->
    cowboy_req:reply(200, [
			        {<<"content-type">>, <<"text/plain; charset=utf-8">>}
	    			    ], Echo, Req).

terminate(_Reason, _Req, _State) ->
	ok.

