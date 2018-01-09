%% Feel free to use, reuse and abuse the code in this file.

-module(http_get_user_nick).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("logger.hrl").
-include("http_req.hrl").
-include("ejb_http_server.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
%%    handle(Req, State, iplimit_util:check_ip(Req)).
    catch ejb_monitor:monitor_count(<<"http_get_user_nick">>,1),
    handle(Req, State, true).

handle(Req, State, false) ->
    Req_Res = Req#http_req{resp_compress = true},
    Res = http_utils:gen_result(false, <<"3">>, <<"ip is limited">>),
    {ok, NewReq} = cowboy_req:reply(200, [
                                    {<<"content-type">>, <<"text/plain; charset=utf-8">>}
                                   ], Res, Req_Res),
    {ok, NewReq, State};
handle(Req, State, _) ->
	{Method, _} = cowboy_req:method(Req),
	case Method of 
	<<"GET">> ->
	    {Host,_} =  cowboy_req:host(Req),
		{ok, Req1} = get_echo(Method,Host,Req),
		{ok, Req1, State};
	<<"POST">> ->
		HasBody = cowboy_req:has_body(Req),
		{ok, Req1} = post_echo(Method, HasBody, Req),
		{ok, Req1, State};
	_ ->
		{ok,Req1} = echo(undefined, Req),
		{ok, Req1, State}
	end.
    	
get_echo(<<"GET">>,_,Req) ->
		cowboy_req:reply(200, [
			{<<"content-type">>, <<"text/json; charset=utf-8">>}
		], <<"No GET method">>, Req).

post_echo(<<"POST">>, true, Req) ->
    {ok, Body, _} = cowboy_req:body(Req),
	Ret = 
		case rfc4627:decode(Body) of
		{ok,{obj,Args},[]}  ->
		 	User = proplists:get_value("ID",Args),
			case catch ets:lookup(user_version,User) of
			[U_info] when is_record(U_info,user_version) ->
				case catch ets:lookup(vcard_version,User) of 
				[U_Vcard] when is_record(U_Vcard,vcard_version)->
					http_utils:gen_result(true,<<"0">>,<<"">>,{obj,[{"Nick",U_info#user_version.name},{"Vcard",U_Vcard#vcard_version.url}]});
				_ ->
					http_utils:gen_result(true,<<"0">>,<<"">>,{obj,[{"Nick",U_info#user_version.name},{"Vcard",<<"">>}]})
				end;
			_ ->
				 http_utils:gen_result(false, <<"1">>,<<"No found name by ID">>,<<"">>)
			end;
		_ ->
			http_utils:gen_result(false, <<"2">>,<<"Josn parse error">>,<<"">>)
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
