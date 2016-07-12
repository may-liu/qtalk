%% Feel free to use, reuse and abuse the code in this file.
%%=========================================================
%%获取用户名片信息接口
%%=========================================================

-module(http_get_vcard_info).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("logger.hrl").
-include("ejabberd_extend.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _} = cowboy_req:method(Req),
	case Method of 
	<<"GET">> ->
	    {Host,_} =  cowboy_req:host(Req),
		{ok, Req2} = get_echo(Method,Host,Req),
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
			{<<"content-type">>, <<"text/plain; charset=utf-8">>}
		], <<"No GET method">>, Req).

post_echo(<<"POST">>, true, Req) ->
	{ok, Body, _} = cowboy_req:body(Req),
	case catch rfc4627:decode(Body) of
	{ok,Json,[]}  ->
		Rslt = 
			case http_utils:verify_user_key(Req) of
        	true ->
				get_update_version(Json);
			_ ->
				http_utils:gen_result(false, 1, <<"Not found Mac_Key">>)
			end,
		cowboy_req:reply(200, [{<<"content-type">>, <<"text/json; charset=utf-8">>}], Rslt, Req);
	_ ->
		Rslt = http_utils:gen_result(false, 2,<<"Body parse error">>),
		cowboy_req:reply(200,[{<<"content-type">>, <<"text/json; charset=utf-8">>}],Rslt, Req)
	end;
post_echo(<<"POST">>, false, Req) ->
	cowboy_req:reply(400, [], <<"Missing Post body.">>, Req);
post_echo(_, _, Req) ->
	cowboy_req:reply(405, Req).
										

echo(undefined, Req) ->
    cowboy_req:reply(400, [], <<"Missing parameter.">>, Req);
echo(Echo, Req) ->
    cowboy_req:reply(200, [
			        {<<"content-type">>, <<"text/plain; charset=utf-8">>}
	    			    ], Echo, Req).

terminate(_Reason, _Req, _State) ->
	ok.

get_update_version(Json)->
        UserStatus =
	   		lists:flatmap(fun({obj,Args}) ->
				User  = proplists:get_value("user",Args),
				Vs  = proplists:get_value("version",Args),
				case catch ets:lookup(vcard_version,User) of
				[] ->
					%%编外人员
						[{obj, [{"U",User},{"V",<<"1">>},{"Url",<<"">>},
						{"commenturl",<<"">>}]}];
				[Vv] when is_record(Vv,vcard_version)  ->
						IVs = http_utils:to_integer(Vs),
						IVv = http_utils:to_integer(Vv#vcard_version.version),
						if IVs < IVv -> 
							case Vv#vcard_version.url of
							null ->
								[{obj, [{"U",User},{"V",Vv#vcard_version.version},{"Url",<<"">>},
								{"commenturl",<<"https://test/dianping/user_comment.php">>}]}];
							_ ->
								[{obj, [{"U",User},{"V",Vv#vcard_version.version},{"Url",Vv#vcard_version.url},
								{"commenturl",<<"https://test/dianping/user_comment.php">>}]}]
							end;
						true ->
							[]
						end;
					_ ->
						[]
					end end,Json),
		http_utils:gen_result(true, 0, UserStatus).

