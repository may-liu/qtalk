%% Feel free to use, reuse and abuse the code in this file.

-module(http_get_user_status).

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
		Res = 
			case http_utils:verify_user_key(Req) of
			true ->
       			{Users,_} = cowboy_req:qs_val(<<"users">>, Req),
				case Users of
				undefined ->
					http_utils:gen_result(false,1, <<"No users list">>);	
				_ ->
        			http_utils:gen_result(true,0,get_user_status(Users))
				end;
			_ ->
				http_utils:gen_result(false, 2, <<"Not found Mac_Key">>)
			end,
		cowboy_req:reply(200, [
			{<<"content-type">>, <<"text/plain; charset=utf-8">>}
		], Res,Req);
get_echo(_,Req) ->
	cowboy_req:reply(405, Req).

post_echo(<<"POST">>, true, Req) ->
	Header = cowboy_req:get(headers,Req),
	{ok, Body, _} = cowboy_req:body(Req),
	Users = 
		case catch proplists:get_value(<<"content-encoding">>,Header) of 
		<<"gzip">> ->
			PBody = cow_qs:parse_qs(zlib:gunzip(Body)),
			proplists:get_value(<<"users">>, PBody);
		_ ->
			PBody = cow_qs:parse_qs(Body),
			proplists:get_value(<<"users">>, PBody)
		end,
	Res =
		case Users of
		undefined ->
			http_utils:gen_result(false, 1, <<"No users list">>);	
		_ ->
			http_utils:gen_result(true,0,get_user_status(Users))
		end,
		cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain; charset=utf-8">>}], Res, Req);
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

get_user_status(UserBList) ->
	UserStr = erlang:binary_to_list(UserBList),
	UserList = string:tokens(UserStr,","),
	UserStatus =
		lists:map(fun(User) ->
			BUser = list_to_binary(User),
				{obj, [{"U", BUser},{"S",
					case ejabberd_public:user_status(BUser) of
					0 ->
					    0;
					_ ->
					    6
					end}]}  end, UserList), 
	{obj,[{"data",UserStatus}]}.
