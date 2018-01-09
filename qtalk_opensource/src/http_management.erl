%% Feel free to use, reuse and abuse the code in this file.

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
    {Url,_Req_t} = cowboy_req:url(Req),
    ?DEBUG("Run http ~p ~n",[Url]),
 %%   handle(Req, State, iplimit_util:check_ip(Req)).
    handle(Req, State, true).

handle(Req, State, false) ->
    Req_Res = Req#http_req{resp_compress = true},
    Res = http_utils:gen_result(false, <<"3">>, <<"ip is limited">>),
    {ok, NewReq} = cowboy_req:reply(200, [
                                    {<<"content-type">>, <<"text/plain; charset=utf-8">>}
                                   ], Res, Req_Res),
    {ok, NewReq, State};
handle(Req, State, _) ->
	{Method, Req2} = cowboy_req:method(Req),
	case Method of 
	<<"GET">> ->
	    {Host,Req3} =  cowboy_req:host(Req),
		 Req4 = cowboy_req:reply(200, [ {<<"content-type">>, <<"text/json; charset=utf-8">>}], 
					http_utils:gen_result(false, <<"-1">>, <<"No Get Method">>) , Req3),
		{ok, Req4, State};
	<<"POST">> ->
		HasBody = cowboy_req:has_body(Req2),
		{ok, Req3} = post_echo(Method, HasBody, Req2),
		{ok, Req3, State};
	_ ->
		{ok,Req3} = echo(undefined, Req2),
		{ok, Req3, State}
	end.
    	
get_echo(<<"GET">>,_,Req) ->
		cowboy_req:reply(200, [
			{<<"content-type">>, <<"text/json; charset=utf-8">>}
		], <<"No GET method">>, Req).

post_echo(<<"POST">>, true, Req) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    {Host,Req3} =  cowboy_req:host(Req),
	{Online,Req4} = cowboy_req:qs_val(<<"online">>, Req),
    Servers = ejabberd_config:get_myhosts(),
    Server = lists:nth(1,Servers),
	case rfc4627:decode(Body) of
	{ok,Json,[]} -> 
	    if Host =/= <<"xxxxxxxxx.com">> ->
			Res = do_cmd(Server,Json),
			cowboy_req:reply(200, [{<<"content-type">>, <<"text/json; charset=utf-8">>}], Res, Req);
		true ->
			Res = http_utils:gen_result(false, <<"-1">>, <<"No Wlan Api">>),
%%			{Key,NewReq1} = cowboy_req:qs_val(<<"k">>, NewReq),
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

do_cmd(Server,Json) ->
	[{obj,Args }] = Json ,
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
	20 ->
		management_cmd:get_user_rescource_list(Server,Args);
	21 ->
		management_cmd:migrate_one_muc_by_name(Server,Args);
	22 ->
		management_cmd:migrate_num_mucs(Server,Args);
    24 ->
        management_cmd:add_muc_users(Server,Args);
	_ ->
		http_utils:gen_result(false, <<"-1">>, <<"No find cmd">>)
	end.

parse_cmd(Args) ->
	http_utils:to_integer(proplists:get_value("cmd",Args),0).
