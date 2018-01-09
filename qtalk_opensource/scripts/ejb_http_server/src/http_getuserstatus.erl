%% Feel free to use, reuse and abuse the code in this file.

-module(http_getuserstatus).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("logger.hrl").
-include("http_req.hrl").
init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _ } = cowboy_req:method(Req),
    catch ejb_monitor:monitor_count(<<"http_get_user_status">>,1),
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
		{ok,Req2} = echo(undefined, Req),
		{ok, Req2, State}
	end.
    	
get_echo(<<"GET">>,Host,Req) ->
	Req_compress = Req#http_req{resp_compress = true},
    case http_utils:check_version(Req) of
	false ->
       	{Users,_} = cowboy_req:qs_val(<<"users">>, Req),
		Rslt =
			case Users of
			undefined ->
				[];
			_ ->
				 get_user_status(Users)	
			end,
       	cowboy_req:reply(200, [
                      {<<"content-type">>, <<"text/plain; charset=utf-8">>}
       	], Rslt, Req_compress);
	true ->
		Rslt = 
			case http_utils:verify_user_key(Req) of
			true ->
       			{Users,_ } = cowboy_req:qs_val(<<"users">>, Req),
				case Users of
				undefined ->
				%%	http_utils:gen_result(false, <<"-1">>, <<"No users list">>);	
					[];
				_ ->
        			get_user_status(Users)
				end;
			_ ->
%%				http_utils:gen_result(false, <<"-1">>, <<"Not found Mac_Key">>)
				[]
			end,
		cowboy_req:reply(200, [
			{<<"content-type">>, <<"text/plain; charset=utf-8">>}
		], Rslt,Req_compress)
	end;
get_echo(<<"Get">>,_,Req) ->
	cowboy_req:reply(405, Req).

post_echo(<<"POST">>, true, Req) ->
	Req_compress = Req#http_req{resp_compress = true},
	Header = cowboy_req:get(headers,Req),
	{ok, Body, _} = cowboy_req:body(Req),
    {Host,_ } =  cowboy_req:host(Req),
    case http_utils:check_version(Req) of
	false ->
		Users = 
			case catch proplists:get_value(<<"content-encoding">>,Header) of 
			<<"gzip">> ->
				PBody = cow_qs:parse_qs(zlib:gunzip(Body)),
				proplists:get_value(<<"users">>, PBody);
			_ ->
				PBody = cow_qs:parse_qs(Body),
				proplists:get_value(<<"users">>, PBody)
			end,
		Rslt =
			case Users of
			undefined ->
%%				http_utils:gen_result(false, <<"-1">>, <<"No users list">>);	
				[];
			_ ->
				get_user_status(Users)
			end,
		cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain; charset=utf-8">>}], Rslt, Req_compress);
	true ->
		Rslt = 
			case http_utils:verify_user_key(Req) of
			true ->
				Users = 
					case catch proplists:get_value(<<"content-encoding">>,Header) of 
					<<"gzip">> ->
						PBody = cow_qs:parse_qs(zlib:gunzip(Body)),
						proplists:get_value(<<"users">>, PBody);
					_ ->
						PBody = cow_qs:parse_qs(Body),
						proplists:get_value(<<"users">>, PBody)
					end,
				case Users of
				undefined ->
				%%	http_utils:gen_result(false, <<"-1">>, <<"No users list">>);	
					[];
				_ ->
					get_user_status(Users)
				end;
			_ ->
%%				http_utils:gen_result(false, <<"-1">>, <<"Not found Mac_Key">>)
				[]
			end,
		cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain; charset=utf-8">>}], Rslt, Req_compress)
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

get_user_status(UserBList) ->
        UserStr = erlang:binary_to_list(UserBList),
        UserList = string:tokens(UserStr,","),
        UserStatus = 
			lists:map(fun(User) ->
				BUser = list_to_binary(User),
                {obj, [{"U", BUser},{"S",
					 case ejb_update_cache:user_status(BUser) of
                     0 ->
                         0;
  					 _ ->
                         6
                     end}]}  end, UserList),
        list_to_binary(rfc4627:encode({obj,[{"data",UserStatus}]})).
