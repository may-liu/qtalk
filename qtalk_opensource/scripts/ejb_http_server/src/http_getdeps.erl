%% Feel free to use, reuse and abuse the code in this file.

-module(http_getdeps).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("ejb_http_server.hrl").
-include("logger.hrl").
-include("http_req.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _} = cowboy_req:method(Req),
    catch ejb_monitor:monitor_count(<<"http_get_deps">>,1),
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
    	
get_echo(<<"GET">>,Host,Req) ->
        Req_compress = Req#http_req{resp_compress = true},
	Rslt = 
		%case http_utils:verify_user_key(Req) of
        case true of
		true ->
			get_departments();
		_ ->
			[]
		end,
	cowboy_req:reply(200, [
		{<<"content-type">>, <<"text/plain; charset=utf-8">>}
	], Rslt, Req_compress);
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
