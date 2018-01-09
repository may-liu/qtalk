%% Feel free to use, reuse and abuse the code in this file.
-module(http_set_user_profile).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("logger.hrl").
-include("ejb_http_server.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _} = cowboy_req:method(Req),
    catch ejb_monitor:monitor_count(<<"http_set_user_profile">>,1),
	case Method of 
	<<"GET">> ->
	    {Host,_ } =  cowboy_req:host(Req),
		{ok, Req2 } = get_echo(Method,Host,Req),
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
    {Host,_ } =  cowboy_req:host(Req),
    {User,_} = cowboy_req:qs_val(<<"u">>, Req),
	case rfc4627:decode(Body) of
	{ok,{obj,Args},[]} -> 
		Rslt = 
			case http_utils:verify_user_key(Req) of
			true ->
				case  proplists:get_value("user",Args) of
				User ->
       			 	http_utils:gen_result(true, <<"0">>,<<"">>,set_user_profile(Args,User));
				_ ->
       			 	http_utils:gen_result(false, <<"2">>,<<"not allow to set other profile">>,<<"">>)
				end;
			false ->
           		http_utils:gen_result(false, <<"1">>, <<"Not found Mac_Key">>,<<"">>)
			end,
		cowboy_req:reply(200, [	{<<"content-type">>, <<"text/json; charset=utf-8">>}], Rslt, Req);
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

set_user_profile(Args,User)->
	Mood = proplists:get_value("mood",Args),
	case catch ejb_odbc_query:update_user_profile(User,Mood) of
	{updated, 1} ->
		case ejb_odbc_query:get_profile_by_user(User) of
		{selected,[<<"profile_version">>],SRes} when is_list(SRes) ->
		[[V]] = SRes,
				ets:insert(user_profile,#user_profile{user = User,version = V,mood = Mood}),
				{obj,[{"user",User},{"version",V}]};
		_ ->
				{obj,[{"user",User},{"version",<<"-1">>}]}
		end;
	_ ->
		case catch ejb_odbc_query:insert_user_profile(User,<<"2">>,Mood) of
		{updated, 1} ->
				ets:insert(user_profile,#user_profile{user = User,version = <<"2">>,mood = Mood}),
				{obj,[{"user",User},{"version",<<"2">>}]};
		_ ->
				{obj,[{"user",User},{"version",<<"-1">>}]}
		end
	end.

