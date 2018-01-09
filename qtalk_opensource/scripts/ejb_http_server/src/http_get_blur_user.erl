-module(http_get_blur_user).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("logger.hrl").
-include("http_req.hrl").
-include("ejb_http_server.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _} = cowboy_req:method(Req),
	case Method of 
	<<"GET">> ->
	    {Host,_ } = cowboy_req:host(Req),
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
	case rfc4627:decode(Body) of
	{ok,{obj,Args},[]} -> 
		User = proplists:get_value("user",Args),
		Rslt = 
			case iplimit_util:check_ips_limit(Req,<<"5">>,User) of
			true ->
        		Ret = http_get_blur_user(Args),
				http_utils:gen_result(true,0,<<"">>,Ret);
			_ ->
           		http_utils:gen_result(false, -1, <<"Not found Mac_Key">>)
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

http_get_blur_user(Args) ->
	Keyword = proplists:get_value("keyword",Args),
	case catch pg_odbc:sql_query(<<"ejb_http_server">>,
			[<<"select username,name,department from users where name like '%">>,Keyword,<<"%' and hire_flag > 0">>]) of
	{selected, _ , SRes} when is_list(SRes) ->
			lists:map(fun([U,N,D]) ->
				  case catch ets:lookup(vcard_version,U)  of
				  [] ->
				  	{obj,[{"U",U},{"N",N},{"D",D},{"P",<<"">>},{"M",get_user_mood(U)}]};
				  [Vv] when is_record(Vv,vcard_version)  ->
			  		 {obj,[{"U",U},{"N",N},{"D",D},{"P",handle_url(Vv#vcard_version.url)},{"M",get_user_mood(U)}]}
				   end end ,SRes);
	true ->
		[]
	end.

handle_url(null) ->
	<<"">>;
handle_url(Pic_url) ->
	Pic_url.

get_user_mood(User) ->
	case catch ets:lookup(user_profile,User) of
	[UP] when is_record(UP,user_profile)  ->
		if UP#user_profile.mood =:= null ->
				<<"">>;
		true ->
				UP#user_profile.mood
		end;
	_ ->
			<<"">>
	end.
		

