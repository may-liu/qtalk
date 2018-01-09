%% Feel free to use, reuse and abuse the code in this file.

-module(http_getonlineuser).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("logger.hrl").
-include("http_req.hrl").
-include("ejb_http_server.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, Req} = cowboy_req:method(Req),
    catch ejb_monitor:monitor_count(<<"http_get_online_user">>,1),
	case Method of 
	<<"GET">> ->
		{Host,_} =  cowboy_req:host(Req),
		{ok, Req2} = get_echo(Method,Host,Req),
		{ok, Req2, State};
	_ ->
		{ok,Req2} = echo(undefined, Req),
		{ok, Req2, State}
	end.
    	
get_echo(<<"GET">>,Host,Req) ->
	Req_compress = Req#http_req{resp_compress = true},
	case http_utils:check_version(Req) of
	false ->
		Rslt =
	   		case catch ets:lookup(cache_info,<<"online1">>) of
			[Online] when is_record(Online,cache_info) ->
				Online#cache_info.cache;
			_ ->
				[]
			end,
		cowboy_req:reply(200, [
			{<<"content-type">>, <<"text/json; charset=utf-8">>}
		], Rslt, Req_compress);
	 true ->
		Rslt =
			case http_utils:verify_user_key(Req) of
			true ->
	   			case catch ets:lookup(cache_info,<<"online1">>) of
				[Online] when is_record(Online,cache_info) ->
					Online#cache_info.cache;
				_ ->
					[]
				end;
			false ->
				[]
			end,
		cowboy_req:reply(200, [{<<"content-type">>, <<"text/json; charset=utf-8">>}], Rslt, Req_compress)
	end;
get_echo(<<"Get">>,_,Req) ->
	cowboy_req:reply(405, Req).

echo(undefined, Req) ->
    cowboy_req:reply(400, [], <<"Missing parameter.">>, Req);
echo(Echo, Req) ->
    cowboy_req:reply(200, [
			        {<<"content-type">>, <<"text/json; charset=utf-8">>}
	    			    ], Echo, Req).

terminate(_Reason, _Req, _State) ->
	ok.
