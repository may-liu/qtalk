%%=====================================================
%%机器人(订阅号)注册接口
%%=====================================================
-module(http_rbtReg).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-include("ejabberd.hrl").
-include("logger.hrl").

-record(rbt_info,{name,url,body,version}).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _} = cowboy_req:method(Req),
	case Method of 
	<<"GET">> ->
		{ok, Req2} = echo(<<"No Get Method!">>,Req),
		{ok, Req2, State};
	<<"POST">> ->
		HasBody = cowboy_req:has_body(Req),
		{ok, Req2} = post_echo(Method, HasBody, Req),
		{ok, Req2, State};
	_ ->
		{ok,Req2} = echo(undefined, Req),
		{ok, Req2, State}
	end.
post_echo(<<"POST">>,true,Req) ->	
	{ok, Body, _} = cowboy_req:body(Req),
	case catch rfc4627:decode(Body) of
	{ok,Json,[]}  ->
		Servers = ejabberd_config:get_myhosts(),
		LServer = lists:nth(1,Servers),
		Res = rbtReg(LServer,Json),
		cowboy_req:reply(200, [{<<"content-type">>, <<"text/json; charset=utf-8">>}], Res, Req);
	_ ->
		cowboy_req:reply(200, [{<<"content-type">>, <<"text/json; charset=utf-8">>}], 
					http_utils:gen_result(false, 1,<<"Json format errror.">>), Req)
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

rbtReg(LServer,Json) ->
	{obj,Args} = Json ,
	Rbt_en_name = proplists:get_value("robotEnName",Args),
	Rbt_cn_name = proplists:get_value("robotCnName",Args),
	Request_url = proplists:get_value("requestUrl",Args),
	Robot_desc 	= proplists:get_value("robotDesc",Args),
	New_json = proplists:delete("requestUrl",Args),
	Rbt_body = list_to_binary(rfc4627:encode({obj,New_json})),
	
	case catch ejabberd_odbc:sql_query(LServer,
		[<<"insert into robot_info (en_name,cn_name,request_url,rbt_desc,rbt_body,rbt_version ) values 
			('">>,Rbt_en_name,<<"','">>,Rbt_cn_name,<<"','">>,
			 	Request_url,<<"','">>,Robot_desc,<<"','">>,Rbt_body,<<"',1);">>]) of
	{updated,1} ->
		ets:insert(rbt_info,#rbt_info{name = Rbt_en_name,url = Request_url,body = Rbt_body,version = 1}),
		http_utils:gen_result(true, 0, <<"Regist Robot sucess">>);
	_ ->
		http_utils:gen_result(false, 2,<<"Regist Robot failed">>)
	end.

