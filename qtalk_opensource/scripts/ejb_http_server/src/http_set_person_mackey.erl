%% Feel free to use, reuse and abuse the code in this file.

-module(http_set_person_mackey).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("logger.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _ } = cowboy_req:method(Req),
    catch ejb_monitor:monitor_count(<<"http_set_person_mackey">>,1),
	case Method of 
	<<"GET">> ->
	{ok, Req2} = get_echo(Method,Req),
	{ok, Req2, State};
	_ ->
	{ok,Req2} = echo(undefined, Req),
	{ok, Req2, State}
	end.
    	
get_echo(<<"GET">>,Req) ->
		{Users,_} = cowboy_req:qs_val(<<"username">>, Req),
		{Domain,_} = cowboy_req:qs_val(<<"domain">>, Req),
    	{Mackey,_} = cowboy_req:qs_val(<<"mackey">>, Req),
    	{Ios,_} = cowboy_req:qs_val(<<"os">>, Req),
    	{Version,_} = cowboy_req:qs_val(<<"version">>, Req),
	?DEBUG("Set mackey User ~p,Doamin ~p,Macke ~p,Plat ~p ~n",[Users,Domain,Mackey,Ios]),
    	Rslt =
    		case {Users,Domain,Mackey} of
    		{undefined,undefined,undefined} ->
    			<<"Not get full info">>;
   			 _ ->
    			set_user_mackey(Users,Domain,Mackey,Ios,Version)
			end,
        cowboy_req:reply(200, [
				            {<<"content-type">>, <<"text/plain; charset=utf-8">>}
							        ], Rslt, Req);
get_echo(<<"Get">>,Req) ->
	cowboy_req:reply(405, Req).

echo(undefined, Req) ->
    cowboy_req:reply(400, [], <<"Missing parameter.">>, Req);
echo(Echo, Req) ->
    cowboy_req:reply(200, [
			        {<<"content-type">>, <<"text/plain; charset=utf-8">>}
	    			    ], Echo, Req).

terminate(_Reason, _Req, _State) ->
	ok.

set_user_mackey(User,Host,Mackey,Os,Version) when User =/= undefined,Host =/= undefined, Mackey =/= undefined ->
	Os1 = 
		case Os of 
		undefined ->
			<<"ios">>;
		_ ->
			Os
		end,
	Version1 = 
		case Version of
		undefined ->
			<<"">>;
		_ ->
			Version
		end,
	case catch pg_odbc:sql_query(<<"ejb_http_server">>, 
			[<<"update person_user_mac_key set mac_key  = '">>, Mackey, <<"', version = '">>, Version1,
		   		<<"' where user_name = '">>, User, <<"' and host = '">>, Host,<<"' and os = '">>,Os1,<<"';">>]) of   
	{updated,1} ->
		http_utils:gen_result(true, <<"0">>, <<"success">>);
	_ ->
		case catch pg_odbc:sql_query(<<"ejb_http_server">>,
			[<<"insert into person_user_mac_key(user_name, host, mac_key, os, version) values ('">>, 
				User,<<"','">>, Host, <<"','">>, Mackey, <<"','">>, Os1, <<"','">>, Version1, <<"');">>]) of 
		{updated,1} -> 
			http_utils:gen_result(true, <<"0">>, <<"success">>);	
		_ ->
			http_utils:gen_result(false, <<"-1">>, <<"failed">>)
		end
	end;
set_user_mackey(_,_,_,_,_) ->
	 http_utils:gen_result(false, <<"-1">>, <<"failed">>).
