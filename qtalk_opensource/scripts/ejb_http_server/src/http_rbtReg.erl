-module(http_rbtReg).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("logger.hrl").
-include("ejb_http_server.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _} = cowboy_req:method(Req),
    catch ejb_monitor:monitor_count(<<"http_rbtReg">>,1),
	case Method of 
	<<"GET">> ->
	    {Host,_} =  cowboy_req:host(Req),
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
	{ok, Body, _} = cowboy_req:body(Req),
    {Host,_} =  cowboy_req:host(Req),
    Header = cowboy_req:get(headers,Req),
	case rfc4627:decode(Body) of
	{ok,Json,[]}  ->
		LServer = <<"ejb_http_server">>,
		Res = rbtReg(LServer,Json),
		cowboy_req:reply(200, [{<<"content-type">>, <<"text/json; charset=utf-8">>}], Res, Req);
	_ ->
		cowboy_req:reply(200, [{<<"content-type">>, <<"text/json; charset=utf-8">>}], 
					http_utils:gen_result(false, <<"-1">>,<<"Json format errror.">>), Req)
	end;
post_echo(<<"POST">>, false, Req) ->
	cowboy_req:reply(400, [], http_utils:gen_result(false, <<"-1">>,<<"Missing Post body.">>), Req);
post_echo(_, _, Req) ->
	cowboy_req:reply(405, Req).
										

echo(undefined, Req) ->
	cowboy_req:reply(400, [], http_utils:gen_result(false, <<"-1">>,<<"Missing Post body.">>), Req);
echo(Echo, Req) ->
    cowboy_req:reply(200, [
			        {<<"content-type">>, <<"text/plain; charset=utf-8">>}
	    			    ], http_utils:gen_result(true, <<"0">>,Echo), Req).

terminate(_Reason, _Req, _State) ->
	ok.

rbtReg(LServer,Json) ->
	{obj,Args} = Json ,
	Rbt_en_name = proplists:get_value("robotEnName",Args),
	Rbt_cn_name = proplists:get_value("robotCnName",Args),
	Request_url = proplists:get_value("requestUrl",Args),
	Robot_desc 	= proplists:get_value("robotDesc",Args),
	New_Args = proplists:delete("requestUrl",Args),
	Password = proplists:get_value("password",Args),
	New_json = proplists:delete("password",New_Args),
	Rbt_body = list_to_binary(rfc4627:encode({obj,New_json})),
	
	case catch pg_odbc:sql_query(LServer,
		[<<"insert into robot_info (en_name,cn_name,request_url,rbt_desc,rbt_body,rbt_version,password) values 
			('">>,Rbt_en_name,<<"','">>,Rbt_cn_name,<<"','">>,
			 	Request_url,<<"','">>,Robot_desc,<<"','">>,pg_odbc:escape(Rbt_body),<<"',1,'">>,Password,<<"');">>]) of
	{updated,1} ->
		ets:insert(rbt_info,#rbt_info{name = Rbt_en_name,url = Request_url,body = Rbt_body,version = 1}),
	%%	http_utils:gen_result(true, <<"0">>, {obj,New_json});
		http_utils:gen_result(true, <<"0">>, <<"Regist Robot sucess">>);
	A  ->
		?DEBUG("A ~p ~n",[A]),
		http_utils:gen_result(false, <<"-1">>,<<"Regist Robot failed">>)
	end.

