%% Feel free to use, reuse and abuse the code in this file.

-module(http_send_rbt_msg).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("ejb_http_server.hrl").
-include("logger.hrl").
-include("http_req.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _ } = cowboy_req:method(Req),
    catch ejb_monitor:monitor_count(<<"http_send_rbt_msg">>,1),
	case Method of 
	<<"GET">> ->
		{ok, Req1} = get_echo(Method,Req),
		{ok, Req1, State};
	<<"POST">> ->
		HasBody = cowboy_req:has_body(Req),
		{ok, Req1} = post_echo(Method, HasBody, Req),
		{ok, Req1, State};
	_ ->
		{ok,Req1} = echo(undefined, Req),
		{ok, Req1, State}
	end.
    	
get_echo(<<"GET">>,Req) ->
		cowboy_req:reply(200, [
			{<<"content-type">>, <<"text/json; charset=utf-8">>}
		], <<"No GET method">>, Req).

post_echo(<<"POST">>, true, Req) ->
    {ok, Body, _} = cowboy_req:body(Req),
	Ret = 
		case rfc4627:decode(Body) of
		{ok,{obj,Args} ,[]} ->
			User = proplists:get_value("From",Args),
		%	case iplimit_util:check_ips_limit(Req,<<"1">>,User) of
            case true of
			true ->
				case rpc_send_message(Args) of
				{error,_Reason} ->
					do_http_send_rbt_msg(Body);
				V ->
					V
				end;
			_ ->
				 http_utils:gen_result(false, <<"3">>, <<"ip is limited">>)
			end;
		_ ->
			 http_utils:gen_result(false, <<"-1">>, <<"Josn parse error">>)
		end,
	cowboy_req:reply(200, [ {<<"content-type">>, <<"text/json; charset=utf-8">>}], Ret, Req);
post_echo(<<"POST">>, false, Req) ->
	cowboy_req:reply(400, [], <<"Missing Post body.">>, Req);
post_echo(_, _, Req) ->
	cowboy_req:reply(405, Req).
										

echo(undefined, Req) ->
    cowboy_req:reply(400, [], <<"Missing parameter.">>, Req);
echo(Echo, Req) ->
    cowboy_req:reply(200, [
			        {<<"content-type">>, <<"text/json; charset=utf-8">>}
	    			    ], Echo, Req).

terminate(_Reason, _Req, _State) ->
	ok.

rpc_send_message(Json) ->
	case catch  ets:lookup(ejabberd_config,<<"node">>) of
	[Ejb_node] when is_record(Ejb_node,ejabberd_config) ->
		case rpc:call(Ejb_node#ejabberd_config.val,http_send_rbt_msg,http_send_message,[Json]) of
		{badrpc,Reason} ->
			{error,Reason};
		V ->
			V
		end;
	_ ->
		{error,<<"no found node">>}
	end.

do_http_send_rbt_msg(Body) ->
	Url1 = 
		case catch  ets:lookup(ejabberd_config,<<"http_server">>) of
		[Http_server] when is_record(Http_server,ejabberd_config) ->
			Http_server#ejabberd_config.val;
		_ ->
			"http://127.0.0.1/"
		end,
	Url = Url1 ++ "send_rbt_msg",
	Header = [],
	Type = "application/json",
	HTTPOptions = [],
	Options = [],
	case http_client:http_post(Url,Header,Type,Body,HTTPOptions,Options) of
	{ok, {_Status,_Headers, Rslt}} ->
		Rslt;
	_ ->
		http_utils:gen_result(false, <<"-1">>, <<"Send rbt_msg error">>)
	end.
