%% Feel free to use, reuse and abuse the code in this file.
%%========================================================
%%工具接口
%%========================================================
-module(http_management).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("http_req.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
   	handle(Req, State, iplimit_util:check_ip(Req)).

handle(Req, State, false) ->
    Req_Res = Req#http_req{resp_compress = true},
    Res = http_utils:gen_result(false, 3, <<"ip is limited">>),
    {ok, NewReq} = cowboy_req:reply(200, [
                                    {<<"content-type">>, <<"text/plain; charset=utf-8">>}
                                   ], Res, Req_Res),
    {ok, NewReq, State};
handle(Req, State, _) ->
	{Method, _ } = cowboy_req:method(Req),
	case Method of 
	<<"GET">> ->
		 Req2 = cowboy_req:reply(200, [ {<<"content-type">>, <<"text/json; charset=utf-8">>}], 
					http_utils:gen_result(false, 2, <<"No Get Method">>) , Req),
		{ok, Req2, State};
	<<"POST">> ->
		HasBody = cowboy_req:has_body(Req),
		{ok, Req2} = post_echo(Method, HasBody, Req),
		{ok, Req2, State};
	_ ->
		{ok,Req2} = echo(undefined, Req),
		{ok, Req2, State}
	end.
    	
get_echo(<<"GET">>,_,Req) ->
		cowboy_req:reply(200, [
			{<<"content-type">>, <<"text/json; charset=utf-8">>}
		], <<"No GET method">>, Req).

post_echo(<<"POST">>, true, Req) ->
    {ok, Body, _} = cowboy_req:body(Req),
	{Host,_} =  cowboy_req:host(Req),
    Servers = ejabberd_config:get_myhosts(),
    Server = lists:nth(1,Servers),
	case catch rfc4627:decode(Body) of
	{ok,  {obj,Args },[]} -> 
	    if Host =/= <<"api.test.com">> ->
			Res = do_cmd(Server,Args),
			cowboy_req:reply(200, [{<<"content-type">>, <<"text/json; charset=utf-8">>}], Res, Req);
		true ->
			Res = http_utils:gen_result(false, 4, <<"No Wlan Api">>),
			cowboy_req:reply(200, [	{<<"content-type">>, <<"text/json; charset=utf-8">>}], Res, Req)
		end;
	_ ->
		 cowboy_req:reply(200, [ {<<"content-type">>, <<"text/json; charset=utf-8">>}], <<"Josn parse error">>, Req)
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

do_cmd(Server,Args) ->
	case parse_cmd(Args) of 
	1 ->
		management_cmd:get_user_num(Server,Args);
	2 ->
		management_cmd:get_muc_opts(Server,Args);
	3 ->
		management_cmd:update_muc_opts(Server,Args);
	4 ->
		management_cmd:stop_muc(Server,Args);
	5 ->
		management_cmd:start_muc(Server,Args);
	6 ->
		management_cmd:remove_muc_user(Server,Args);
	7 ->
		management_cmd:judge_muc_online(Server,Args);
	8 ->
		management_cmd:destroy_muc(Server,Args);
	9 ->
		management_cmd:get_online_status(Server,Args);
	10 ->
		management_cmd:kick_user(Server,Args);
	11 ->
		management_cmd:get_user_mac_key(Server,Args);
	12 ->
		management_cmd:update_user_info(Server,Args);
	13 ->
		management_cmd:delete_user(Server,Args);
	14 ->
		%%单域作用有效
		management_cmd:restart_pgsql_odbc(Server,Args);
	15 ->
		%%单域作用有效
		management_cmd:update_ets_info(Server);
	16 ->
		management_cmd:get_user_registed_muc_num(Server,Args);
	17 ->
		management_cmd:insert_iplimit(Server,Args);
	18->
		management_cmd:delete_iplimit(Server,Args);
	19->
		management_cmd:get_user_rescource(Server,Args);
	_ ->
		http_utils:gen_result(false, 5, <<"No find cmd">>)
	end.

parse_cmd(Args) ->
	http_utils:to_integer(proplists:get_value("cmd",Args,0),0).

	
