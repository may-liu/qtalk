%% Feel free to use, reuse and abuse the code in this file.
%%========================================================
%%设置用户机器码接口
%%========================================================

-module(http_set_mac_key).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-include("ejabberd.hrl").
-include("logger.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _} = cowboy_req:method(Req),
	case Method of 
	<<"GET">> ->
    {Method, _} = cowboy_req:method(Req),
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
    	Res =
    		case {Users,Domain} of
    		{undefined,undefined} ->
				http_utils:gen_result(false,1,<<"Not get full info">>);
   			 _ ->
    			set_user_mackey(Users,Domain,Mackey)
			end,
        cowboy_req:reply(200, [
				            {<<"content-type">>, <<"text/plain; charset=utf-8">>}
							        ], Res, Req);
get_echo(_,Req) ->
	cowboy_req:reply(405, Req).

echo(undefined, Req) ->
    cowboy_req:reply(400, [], <<"Missing parameter.">>, Req);
echo(Echo, Req) ->
    cowboy_req:reply(200, [
			        {<<"content-type">>, <<"text/plain; charset=utf-8">>}
	    			    ], Echo, Req).

terminate(_Reason, _Req, _State) ->
	ok.

set_user_mackey(Users,Host,Mackey) ->
    Servers = ejabberd_config:get_myhosts(),
    Server = lists:nth(1,Servers),
    case catch odbc_queries:update_user_mac_key(Server,Users,Host,Mackey) of
    {updated, 1} ->
            http_utils:gen_result(true, 0, <<"success">>);
     A ->
	 		?DEBUG("A ~p ~n",[A]),
		    case catch odbc_queries:insert_user_mac_key(Server,Users,Host,Mackey) of
			{updated,1} ->
            	http_utils:gen_result(true, 0, <<"success">>);
		    B ->
	 		?DEBUG("A1 ~p ~n",[B]),
            	http_utils:gen_result(false, 1, <<"failed">>)
		    end
    end.
