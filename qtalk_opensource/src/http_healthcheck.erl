%% Feel free to use, reuse and abuse the code in this file.

-module(http_healthcheck).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-include("ejabberd.hrl").
-include("logger.hrl").
-include("http_req.hrl").

-record(monitor_rec,{key,count,value,time}).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, Req2} = cowboy_req:method(Req),
	case Method of 
	<<"GET">> ->
	{ok, Req4} = get_echo(Method,<<"pidgin">>,Req2),
	{ok, Req4, State};
	_ ->
	{ok,Req3} = echo(undefined, Req2),
	{ok, Req3, State}
	end.
    	
get_echo(<<"GET">>,Client_type,Req) ->
	if Client_type == <<"pidgin">>; Client_type == <<"qtalk">> ->
		cowboy_req:reply(200, [
		], [], Req);
	 true ->
		Res2 = "Not found Client Type",
		cowboy_req:reply(200, [
			{<<"content-type">>, <<"text/plain; charset=utf-8">>}
		], Res2, Req)
	end;
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

