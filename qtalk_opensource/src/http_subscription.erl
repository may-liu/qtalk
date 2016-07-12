%% Feel free to use, reuse and abuse the code in this file.
%%========================================================
%%订阅消息接口
%%========================================================
-module(http_subscription).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("http_req.hrl").

-record(user_rbts,{name,rbt}).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _} = cowboy_req:method(Req),
	case Method of 
	<<"GET">> ->
		 Req1 = cowboy_req:reply(200, [ {<<"content-type">>, <<"text/json; charset=utf-8">>}], 
					http_utils:gen_result(false,-1, <<"No Get Method">>) , Req),
		{ok, Req1, State};
	<<"POST">> ->
		HasBody = cowboy_req:has_body(Req),
		{ok, Req2} = post_echo(Method, HasBody, Req),
		{ok, Req2, State};
	_ ->
		{ok,Req2} = echo(undefined, Req),
		{ok, Req2, State}
	end.
    	
post_echo(<<"POST">>, true, Req) ->
    {ok, Body,_} = cowboy_req:body(Req),
	{U,_} = cowboy_req:qs_val(<<"u">>, Req),
    Servers = ejabberd_config:get_myhosts(),
    Server = lists:nth(1,Servers),
	case catch rfc4627:decode(Body) of
	{ok,{obj,Args },[]} -> 
		Res = 
			case http_utils:verify_user_key(Server,Req) of
			true ->
				subscription(Server,U,Args);
			false ->
				http_utils:gen_result(false, 1, <<"Not found Mac_Key">>)
			end,
		cowboy_req:reply(200, [	{<<"content-type">>, <<"text/json; charset=utf-8">>}], Res, Req);
	_ ->
		 cowboy_req:reply(200, [ {<<"content-type">>, <<"text/json; charset=utf-8">>}], 
				 http_utils:gen_result(false, 2, <<"Json format error">>), Req)
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

subscription(Server,U,Args) ->
	case check_user(U,Args) of
	true ->
		case parse_method(Args) of 
		1 ->
			set_subscription(Server,Args);
		2 ->
			del_subscription(Server,Args);
		3 ->
			get_subscription(Server,Args);
		_ ->
			http_utils:gen_result(false, 3, <<"No find Method">>)
		end;
	false ->
		http_utils:gen_result(false, 4, <<"User not match u">>)
	end.
		

parse_method(Args) ->
	case proplists:get_value("method",Args) of
	<<"add">> ->
		1;
	<<"del">> ->
		2;
	<<"get">> ->
		3;
	_ ->
		0
	end.

check_user(_User,_Json) ->
%%	User =:=  proplists:get_value("user",Json),
	true.

set_subscription(Server,Args) ->
	User =  proplists:get_value("user",Args),
	Rbt_name = proplists:get_value("rbt",Args),	
	case catch ets:lookup(rbt_info,Rbt_name) of
	[] ->
		http_utils:gen_result(false, 5,<<"Rbt not exit">>);
	_ ->
		case catch ejabberd_odbc:sql_query(Server,
			[<<"insert into robot_pubsub (rbt_name,user_name) values ('">>,Rbt_name,<<"','">>,User,<<"');">>]) of
		{updated,1} ->
			ets:insert(user_rbts,#user_rbts{name = User,rbt = Rbt_name}), 
			http_utils:gen_result(true, 0,<<"Add subscription sucess">>);
		_ ->
			http_utils:gen_result(false, 7,<<"Add subscription failed">>)
		end
	end.
	
del_subscription(Server,Args) ->
	User =  proplists:get_value("user",Args),
	Rbt_name = proplists:get_value("rbt",Args),	
	case catch ejabberd_odbc:sql_query(Server,
		[<<"delete from robot_pubsub where rbt_name = '">>,Rbt_name, <<"' and user_name = '">>,User,<<"';">>]) of
	{updated,1} ->
	 	ets:delete_object(user_rbts,#user_rbts{name = User,rbt = Rbt_name}), 
		http_utils:gen_result(true, 0,<<"Del subscription sucess">>);
	_ ->
		http_utils:gen_result(false, 8,<<"Del subscription failed">>)
	end.

get_subscription(_Server,Args) ->	
	User =  proplists:get_value("user",Args),
	case catch ets:select(user_rbts,[{#user_rbts{name = User,rbt = '$1', _ = '_'},[], ['$1']}]) of
	L when is_list(L) ->
		http_utils:gen_result(true, 0,L);
	_ ->
		http_utils:gen_result(false, 9,<<"Get user robots failed">>)
	end.
		
