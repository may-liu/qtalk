%% Feel free to use, reuse and abuse the code in this file.
%%========================================================
%%获取历史消息接口（指定时间到当前）
%%========================================================
-module(http_get_history).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-include("ejabberd.hrl").
-include("logger.hrl").
-include("http_req.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    %%{Url,_Req_t} = cowboy_req:url(Req),
    do_handle(Req, State).

do_handle(Req, State) ->
	{Method, _} = cowboy_req:method(Req),
	case Method of 
	<<"GET">> ->
		{ok, Req2} = get_echo(Method,Req),
		{ok, Req2, State};
	<<"POST">> ->
		HasBody = cowboy_req:has_body(Req),
		{ok, Req2} = post_echo(Method, HasBody, Req),
		{ok, Req2, State};
	_ ->
		{ok,Req2} = echo(undefined, Req),
		{ok, Req2, State}
	end.
    	
get_echo(<<"GET">>,Req) ->
		cowboy_req:reply(200, [
			{<<"content-type">>, <<"text/json; charset=utf-8">>}
		], <<"No GET method">>, Req).

post_echo(<<"POST">>, true, Req) ->
    {ok, Body, _} = cowboy_req:body(Req),
    Servers = ejabberd_config:get_myhosts(),
    Server = lists:nth(1,Servers),
	case rfc4627:decode(Body) of
	{ok,{obj,Args},[]} -> 
		Res = 
			case http_utils:verify_user_key(Server,Req) of
			true ->
       			http_get_history(Server,Args);
			_ ->
           		http_utils:gen_result(false, 1, <<"Not found Mac_Key">>)
			end,
		cowboy_req:reply(200, [	{<<"content-type">>, <<"text/json; charset=utf-8">>}], Res, Req);
	_ ->
		 cowboy_req:reply(200, [ {<<"content-type">>, <<"text/json; charset=utf-8">>}],http_utils:gen_result(false, 1, <<"Josn parse error">>), Req)
	end;
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

http_get_history(Server,Args)->
	User = ejabberd_odbc:escape(proplists:get_value("User",Args)),
	Time = proplists:get_value("Time",Args),
	Res = 
		case catch odbc_queries:get_msg_info2(Server,User,Time,<<"asc">>) of
		{selected,[<<"m_from">>,<<"m_to">>,<<"m_body">>,<<"read_flag">>],SRes} 
		when is_list(SRes) ->
			lists:map(fun([Lfrom,Lto,Lbody,Rflag]) ->
					{obj,[{"F",Lfrom},{"T",Lto},{"B",Lbody},{"R",Rflag}]}
					 end ,SRes);
		_  ->
			[]
		end,
    http_utils:gen_result(ture, 0, Res).
