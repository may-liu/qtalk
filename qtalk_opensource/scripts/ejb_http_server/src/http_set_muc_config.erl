%% Feel free to use, reuse and abuse the code in this file.
-module(http_set_muc_config).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("logger.hrl").
-include("ejb_http_server.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _ } = cowboy_req:method(Req),
    catch ejb_monitor:monitor_count(<<"http_set_muc_config">>,1),
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
	case http_utils:verify_user_key(Req) of
	true ->
		{ok, Body, _} = cowboy_req:body(Req),
		case rfc4627:decode(Body) of
		{ok,Json,[]}  ->
    	    {User,_} = cowboy_req:qs_val(<<"u">>, Req),
			Rslt =	set_user_config(User,Json),
			cowboy_req:reply(200, [{<<"content-type">>, <<"text/json; charset=utf-8">>}], Rslt, Req);
		 _ ->
   		    Rslt =  http_utils:gen_result(false,1, <<"Body parse error">>,[]),
			cowboy_req:reply(200,[{<<"content-type">>, <<"text/json; charset=utf-8">>}],Rslt, Req)
		end;
	_ ->
   		    Rslt =  http_utils:gen_result(false,1, <<"Tkey check error">>,[]),
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

set_user_config(User,Json) ->
	Res = 
		lists:flatmap(fun({obj,Args}) ->
    		GroupId = proplists:get_value("groupid", Args),
			Key = proplists:get_value("key",Args),
			Value  = proplists:get_value("value",Args),
			do_set_user_config(User,GroupId,Key,Value) end,Json),
    http_utils:gen_result(true,0,<<"">>,Res).

do_set_user_config(User,GroupId,Key,Value) when GroupId =:= undefined;Key =:= undefined; Value =:= undefined ->
	[{obj,[{"groupid",GroupId},{"key", Key}, {"version", <<"-1">>}]}];
do_set_user_config(User,GroupId,Key,Value)  ->
	case catch pg_odbc:sql_query(<<"ejb_http_server">>,
			[<<"update group_extent set update_at = 'now()', version = version + 1, value  = '">>, pg_odbc:escape(Value), 
				<<"' where key = '">>, pg_odbc:escape(Key), <<"' and group_id = '">>, pg_odbc:escape(GroupId), <<"';">>]) of
    {updated,1} ->
     		gen_result(GroupId, Key);       
     _Error  ->
	        do_insert_conf(GroupId, Key, Value)
	 end.

do_insert_conf(GroupId, Key, Value) ->
	case catch pg_odbc:sql_query(<<"ejb_http_server">>,
			[<<"insert into group_extent (group_id, key, value) values ('">>, pg_odbc:escape(GroupId),<<"','">>, 
				pg_odbc:escape(Key), <<"','">>, pg_odbc:escape(Value), <<"');">>]) of
    {updated,1} ->
		gen_result(GroupId, Key); 
	_ ->
		[{obj,[{"groupid", GroupId},{"key",Key}, {"version", <<"-1">>}]}]
	end.
																				 
gen_result(GroupId, Key) ->
    case catch pg_odbc:sql_query(<<"ejb_http_server">>,
		[<<"select value, version from group_extent where key = '">>, pg_odbc:escape(Key), <<"' and group_id = '">>,GroupId, <<"';">>]) of
    {selected,_, Res} ->
		lists:map(fun([Value, Ver]) ->
				{obj,[{"groupid", GroupId},{"key", Key}, {"version", Ver}]}
			  end, Res);
        _Error  ->
				[{obj,[{"groupid", GroupId},{"key", Key}, {"version", <<"-1">>}]}]
    end.



