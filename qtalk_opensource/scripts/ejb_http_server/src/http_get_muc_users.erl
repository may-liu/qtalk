%% Feel free to use, reuse and abuse the code in this file.

-module(http_get_muc_users).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("logger.hrl").
-include("ejb_http_server.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _ } = cowboy_req:method(Req),
    catch ejb_monitor:monitor_count(<<"http_get_muc_vcard_domain">>,1),
	case Method of 
	<<"GET">> ->
	    {Host,_} =  cowboy_req:host(Req),
		{ok, Req1} = get_echo(Method,Host,Req),
		{ok, Req1, State};
	<<"POST">> ->
		HasBody = cowboy_req:has_body(Req),
		{ok, Req2} = post_echo(Method, HasBody, Req),
		{ok, Req2, State};
	_ ->
		{ok,Req3} = echo(undefined, Req),
		{ok, Req3, State}
	end.
    	
get_echo(<<"GET">>,_,Req) ->
		cowboy_req:reply(200, [
			{<<"content-type">>, <<"text/plain; charset=utf-8">>}
		], <<"No GET method">>, Req).

post_echo(<<"POST">>, true, Req) ->
	{ok, Body, _} = cowboy_req:body(Req),
	case rfc4627:decode(Body) of
	{ok,Json,[]}  ->
		Rslt =
			case http_utils:verify_user_key_pv1(Req) of
			true ->
				get_muc_users(Json);
			false ->
                case catch http_utils:verify_muc_acess_limit(Req) of
                true ->
                    get_muc_users(Json);
                _ ->
    			    http_utils:gen_result(false, 1, <<"Tkey  check error">>,<<"">>)
                end
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

get_muc_users(Json) ->
	{obj,Args} = Json,
	Muc = proplists:get_value("muc",Args),
	Ret = 
		case catch pg_odbc:sql_query(<<"ejb_http_server">>,
			[<<"select username from muc_room_users where muc_name = '">>,Muc,<<"'">>]) of
		{selected,_,Res} when is_list(Res)  ->
			lists:map(fun(U) ->
				User = list_to_binary(U),
		  		Nick = ejb_public:get_user_nick(User),
				{obj,[{"U",User},{"N",Nick}]} end,Res);
		_ ->
			[]
		end,
	http_utils:gen_result(true, 0, <<"">>,Ret).	


