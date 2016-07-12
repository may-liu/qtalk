%% Feel free to use, reuse and abuse the code in this file.
%%=========================================================
%%获取在线用户的状态信息接口
%%=========================================================
-module(http_get_online_user2).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("ejabberd.hrl").
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
	Res =
		case http_utils:verify_user_key(Req) of
		true ->
			case catch ets:lookup(cache_info,<<"online2">>) of
			[Online] when is_record(Online,cache_info) ->
				http_utils:gen_result(true,0,Online#cache_info.cache);
			_ ->
			    http_utils:gen_result(true,0,{obj,[{"Online_User_Status",[]}]})
			end;
		_ ->
			http_utils:gen_result(false, 1, <<"Not found Mac_Key">>)
		end,
	cowboy_req:reply(200, [{<<"content-type">>, <<"text/json; charset=utf-8">>}], Res, Req);
get_echo(_,Req) ->
	cowboy_req:reply(405, Req).

echo(undefined, Req) ->
    cowboy_req:reply(400, [], <<"Missing parameter.">>, Req);
echo(Echo, Req) ->
    cowboy_req:reply(200, [
			        {<<"content-type">>, <<"text/json; charset=utf-8">>}
	    			    ], Echo, Req).

terminate(_Reason, _Req, _State) ->
	ok.
