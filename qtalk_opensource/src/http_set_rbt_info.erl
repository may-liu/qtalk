%% Feel free to use, reuse and abuse the code in this file.
%%========================================================
%%设置机器人信息接口
%%========================================================

-module(http_set_rbt_info).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("ejabberd.hrl").
-include("logger.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _} = cowboy_req:method(Req),
	case Method of 
	<<"GET">> ->
		{ok, Req1} = echo(<<"No Get Method!">>,Req),
		{ok, Req1, State};
	<<"POST">> ->
		HasBody = cowboy_req:has_body(Req),
		{ok, Req1} = post_echo(Method, HasBody, Req),
		{ok, Req1, State};
	_ ->
		{ok,Req1} = echo(undefined, Req),
		{ok, Req1, State}
	end.
post_echo(<<"POST">>,true,Req) ->	
	{ok, PBody, _} = cowboy_req:body(Req),
	Header = cowboy_req:get(headers,Req),
	Body = 
		case catch proplists:get_value(<<"content-encoding">>,Header) of 
		<<"gzip">> ->
			zlib:gunzip(PBody);
		_ ->
			PBody
		end,	
	case catch rfc4627:decode(Body) of
	{ok,{obj,Args} ,[]}  ->
		Servers = ejabberd_config:get_myhosts(),
		LServer = lists:nth(1,Servers),
		Res = set_rbt_info(LServer,Args),
		cowboy_req:reply(200, [{<<"content-type">>, <<"text/json; charset=utf-8">>}], Res, Req);
	_ ->
		cowboy_req:reply(200, [{<<"content-type">>, <<"text/json; charset=utf-8">>}], 
					http_utils:gen_result(false, 1,<<"Json format error.">>), Req)
	end;
post_echo(<<"POST">>, false, Req) ->
	cowboy_req:reply(400, [], <<"Missing Post body.">>, Req);
post_echo(_, _, Req) ->
	cowboy_req:reply(405, Req).
										

echo(undefined, Req) ->
	cowboy_req:reply(400, [], <<"Missing Post body.">>, Req);
echo(Echo, Req) ->
    cowboy_req:reply(200, [
			        {<<"content-type">>, <<"text/plain; charset=utf-8">>}
	    			    ], http_utils:gen_result(true, 0,Echo), Req).

terminate(_Reason, _Req, _State) ->
	ok.

set_rbt_info(LServer,Args) ->
    Rbt_en_name = proplists:get_value("robotEnName",Args),
    Rbt_cn_name = proplists:get_value("robotCnName",Args),
    Request_url = proplists:get_value("requestUrl",Args),
    Robot_desc  = proplists:get_value("robotDesc",Args),
    New_json = proplists:delete("requestUrl",Args),
    Rbt_body = list_to_binary(rfc4627:encode({obj,New_json})),

	case catch ejabberd_odbc:sql_query(LServer,
		[<<"update robot_info set cn_name = '">>,Rbt_cn_name,<<"',request_url = '">>,Request_url,<<"',rbt_desc = '">>,Robot_desc,
		<<"',rbt_body = '">>,Rbt_body,<<"',rbt_version = rbt_version + 1 where en_name = '">>,Rbt_en_name,<<"';">>]) of
	{updated,1} ->
		http_utils:gen_result(true, 0, <<"sucesss">>);
	_ ->
		http_utils:gen_result(false, 1, <<"failed">>)
	end.
		
