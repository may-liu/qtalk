%% Feel free to use, reuse and abuse the code in this file.

-module(http_get_increment_users).
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-include("ejabberd.hrl").
-include("logger.hrl").
-include("http_req.hrl").

-record(user_version,{user,name,version,hire_flag,fp,sp,type,dept}).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    {Url,_Req_t} = cowboy_req:url(Req),
	{Method, Req2} = cowboy_req:method(Req),
	case Method of 
	<<"GET">> ->
	    {Host,Req3} =  cowboy_req:host(Req),
		{ok, Req4} = echo(<<"No Get Method!">>,Req),
		{ok, Req4, State};
	<<"POST">> ->
		HasBody = cowboy_req:has_body(Req),
		{ok, Req3} = post_echo(Method, HasBody, Req),
		{ok, Req3, State};
	_ ->
		{ok,Req3} = echo(undefined, Req),
		{ok, Req3, State}
	end.
post_echo(<<"POST">>,true,Req) ->	
	{ok, PBody, _} = cowboy_req:body(Req),
    {Host,_} =  cowboy_req:host(Req),
	Header = cowboy_req:get(headers,Req),
	{Type,_ } = cowboy_req:qs_val(<<"type">>, Req),
	Body = 
		case catch proplists:get_value(<<"content-encoding">>,Header) of 
		<<"gzip">> ->
			zlib:gunzip(PBody);
		_ ->
			PBody
		end,	
    Req1 = Req#http_req{resp_compress = true},
	case rfc4627:decode(Body) of
	{ok,{obj,Args},[]}  ->
		Servers = ejabberd_config:get_myhosts(),
		LServer = lists:nth(1,Servers),
		Res =	case Type of 
	   			<<"1">>	 ->
					get_increment_users(LServer,Args);
				_ ->
					get_increment_users(LServer,Args)
				end,
		cowboy_req:reply(200, [{<<"content-type">>, <<"text/json; charset=utf-8">>}], Res, Req1);
	_ ->
		cowboy_req:reply(200, [{<<"content-type">>, <<"text/json; charset=utf-8">>}], 
					http_utils:gen_result(false, <<"-1">>,<<"Json format error.">>,<<"">>), Req)
	end;
post_echo(<<"POST">>, false, Req) ->
	cowboy_req:reply(400, [], http_utils:gen_result(false, <<"-1">>,<<"Missing Post body.">>,<<"">>), Req);
post_echo(_, _, Req) ->
	cowboy_req:reply(405, Req).
										

echo(undefined, Req) ->
	cowboy_req:reply(400, [], http_utils:gen_result(false, <<"-1">>,<<"Missing Post body.">>,<<"">>), Req);
echo(Echo, Req) ->
    cowboy_req:reply(200, [
			        {<<"content-type">>, <<"text/plain; charset=utf-8">>}
	    			    ], http_utils:gen_result(true, <<"0">>,Echo,<<"">>), Req).

terminate(_Reason, _Req, _State) ->
	ok.

get_increment_users(LServer,Args) ->
	Max_version = proplists:get_value("version",Args),
	Res =
		lists:flatmap(fun(User) ->
				case User#user_version.version > Max_version of
				true ->
					[{obj,[{"U",User#user_version.user},{"N",User#user_version.name},{"V",User#user_version.version},{"D",User#user_version.dept},
						{"T",User#user_version.type},{"F",User#user_version.fp},{"S",User#user_version.fp},{"H",User#user_version.hire_flag}]}];
				false ->
					[]
				end end,ets:tab2list(user_version)),
	http_utils:gen_result(true, <<"0">>,<<"">>,Res).
