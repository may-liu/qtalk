%% Feel free to use, reuse and abuse the code in this file.
%%========================================================
%%设置用户名片接口
%%========================================================

-module(http_set_vcard_info).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("ejabberd.hrl").
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
			{<<"content-type">>, <<"text/json; charset=utf-8">>}
		], <<"No GET method">>, Req).

post_echo(<<"POST">>, true, Req) ->
    {ok, Body, _} = cowboy_req:body(Req),
	Server = lists:nth(1, ejabberd_config:get_myhosts()),
	case rfc4627:decode(Body) of
	{ok,Json,[]} -> 
			Res = 
				case http_utils:verify_user_key(Server,Req) of
				true ->
					{User,_} = cowboy_req:qs_val(<<"u">>, Req),
        			set_update_version(Server,Json,User);
				false ->
	           		http_utils:gen_result(false, 1, <<"Not found Mac_Key">>)
				end,
		cowboy_req:reply(200, [	{<<"content-type">>, <<"text/json; charset=utf-8">>}], Res, Req);
	_ ->
		 cowboy_req:reply(200, [ {<<"content-type">>, <<"text/json; charset=utf-8">>}], 
				 	 http_utils:gen_result(false, 2,<<"Josn parse error">>), Req)
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

set_update_version(Server,Json,User)->
        UserStatus = lists:flatmap(
                        fun({obj,Args}) ->
							User = proplists:get_value("user",Args),
							Url = proplists:get_value("url",Args),
						   	case catch odbc_queries:update_vcard_version(Server,User,Url) of
							{updated, 1} ->
								case odbc_queries:get_vcard_version_by_user(Server,User) of
								{selected,[<<"version">>],SRes} when is_list(SRes) ->
									[[V]] = SRes,
									ets:insert(vcard_version,#vcard_version{user = User,version = V,url = Url}),
									[{obj,[{"user",User},{"version",V}]}];
								_ ->
									[{obj,[{"user",User},{"version",<<"-1">>}]}]
								end;
							_ ->
								case catch odbc_queries:insert_vcard_version(Server,User,<<"2">>,Url) of
								{updated, 1} ->
									ets:insert(vcard_version,#vcard_version{user = User,version = <<"2">>,url = Url}),
									[{obj,[{"user",User},{"version",<<"2">>}]}];
								_ ->
									[{obj,[{"user",User},{"version",<<"-1">>}]}]
								end
						end end,Json),
        http_utils:gen_result(true,0,UserStatus).

