%% Feel free to use, reuse and abuse the code in this file.

-module(http_get_user_profile).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("logger.hrl").
-include("ejb_http_server.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _} = cowboy_req:method(Req),
    catch ejb_monitor:monitor_count(<<"http_get_user_profile">>,1),
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
    {Host,_ } =  cowboy_req:host(Req),
	case rfc4627:decode(Body) of
	{ok,Json,[]}  ->
		Rslt = 
			case http_utils:verify_user_key(Req) of
       		true ->
			%%	http_utils:gen_result(true,<<"0">>,<<"">>,get_user_profile(Json));
				{Plant,_} = cowboy_req:qs_val(<<"p">>, Req),
				case Plant =:= undefined andalso length(Json) =:= 1 of
				true ->
					http_utils:gen_result(true,<<"0">>,<<"">>,get_user_profile1(Json));
				_ ->
					http_utils:gen_result(true,<<"0">>,<<"">>,get_user_profile(Json))
				end;
			_ ->
				http_utils:gen_result(false, <<"-1">>, <<"Not found Mac_Key">>,[])
			end,
		cowboy_req:reply(200, [{<<"content-type">>, <<"text/json; charset=utf-8">>}], Rslt, Req);
	_ ->
		Rslt = <<"Body parse error">>,
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

get_user_profile(Json)->
	   	lists:flatmap(fun({obj,Args}) ->
			User  = proplists:get_value("user",Args),
			Uv  = proplists:get_value("version",Args),
			case catch ets:lookup(user_profile,User) of
			[UP] when is_record(UP,user_profile)  ->
					IUs = http_utils:to_integer(Uv),
					IUv = http_utils:to_integer(UP#user_profile.version),
					if IUs < IUv -> 
						case UP#user_profile.mood of
						null ->
							[{obj, [{"U",User},{"V",UP#user_profile.version},{"M",<<"">>}]}];
						_ ->
							[{obj, [{"U",User},{"V",UP#user_profile.version},{"M",UP#user_profile.mood}]}]
						end;
					true ->
						[]
					end;
				_ ->
					[]
				end end,Json).

get_user_profile1(Json)->
	   	lists:flatmap(fun({obj,Args}) ->
			User  = proplists:get_value("user",Args),
			Uv  = proplists:get_value("version",Args),
			case catch ets:lookup(user_profile,User) of
			[UP] when is_record(UP,user_profile)  ->
					case UP#user_profile.mood of
					null ->
						[{obj, [{"U",User},{"V",UP#user_profile.version},{"M",<<"">>}]},
						 {obj,[{"U",str:concat(User,<<"@ejabhost1">>)},{"V",UP#user_profile.version},{"M",<<"">>}]}];
					_ ->
						[{obj, [{"U",User},{"V",UP#user_profile.version},{"M",UP#user_profile.mood}]},
						 {obj, [{"U",str:concat(User,<<"@ejabhost1">>)},{"V",UP#user_profile.version},{"M",UP#user_profile.mood}]}]
					end;
			_ ->
					[{obj, [{"U",User},{"V",<<"0">>},{"M",<<"">>}]},
		  			 {obj, [{"U",str:concat(User,<<"@ejabhost1">>)},{"V",<<"0">>},{"M",<<"">>}]}]
			end end,Json).
