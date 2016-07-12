%% Feel free to use, reuse and abuse the code in this file.
%%========================================================
%%获取群历史信息接口
%%========================================================

-module(http_get_muc_msg).

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
    		{From,_} = cowboy_req:qs_val(<<"muc_name">>, Req),
	   		{Timestamp,_} = cowboy_req:qs_val(<<"timestamp">>, Req),
   			{Num,_} = cowboy_req:qs_val(<<"limitnum">>, Req),
   			{Direction,_} = cowboy_req:qs_val(<<"direction">>, Req),
			{User,_} = cowboy_req:qs_val(<<"u">>, Req),

			case user_in_room(User,From) of
			true ->
				http_utils:gen_result(true,0,get_muc_msg_info(Direction,ejabberd_odbc:escape(From),Timestamp,Num));
			_ ->
				http_utils:gen_result(false,1,<<"User not in room">>)
			end;	
		_ ->
       		http_utils:gen_result(false, 1, <<"Not found Mac_Key">>)
		end,
	cowboy_req:reply(200,[
			{<<"content-type">>, <<"text/plain; charset=utf-8">>}
		], Res, Req);
get_echo(_,Req) ->
	cowboy_req:reply(405, Req).

post_echo(<<"POST">>, true, Req) ->
	{ok, Body, _} = cowboy_req:body(Req),
	Servers = ejabberd_config:get_myhosts(),
	LServer = lists:nth(1,Servers),
	case rfc4627:decode(Body) of
	{ok,{obj,Args} ,[]}  ->
		Res = 
			case http_utils:verify_user_key(LServer,Req) of	
			true ->
				From = proplists:get_value("muc_name",Args),
				Timestamp = proplists:get_value("timestamp",Args),
				Num = proplists:get_value("limitnum",Args),
				Direction = proplists:get_value("direction",Args),
				{User,_} = cowboy_req:qs_val(<<"u">>, Req),

				Ret = 
					case user_in_room(User,From) of
					true ->
						get_muc_msg_info(Direction,ejabberd_odbc:escape(From),Timestamp,Num);
					_ ->
						[]
					end,
				case User of
				undefined ->
					http_utils:gen_result(true,0,Ret);
				_ ->
					Sup_json = get_user_last(LServer,User,From),
					http_utils:gen_result(true,0,Ret ++ Sup_json)
				end;
			_ ->
				http_utils:gen_result(true,0,[])
			end,
		cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain; charset=utf-8">>}], Res, Req);
	_ ->
		Res = http_utils:gen_result(false,1,<<"Body parse error">>),
		cowboy_req:reply(200,[{<<"content-type">>, <<"text/json; charset=utf-8">>}],Res, Req)
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

get_muc_msg_info(<<"1">>,From,Timestamp,Num) ->
	Servers = ejabberd_config:get_myhosts(),
	LServer = lists:nth(1,Servers),
	case catch odbc_queries:get_muc_msg_info(jlib:nameprep(LServer),From,Timestamp,Num) of
	{selected,  [<<"muc_room_name">>,<<"nick">>,<<"packet">>], SRes}
			when is_list(SRes) ->
				lists:map(fun([Lfrom,Lto,Lbody]) ->
					{obj,[{"M",Lfrom},{"N",Lto},{"B",Lbody}]}
			end ,SRes);
	_ ->
			[]
	end;
get_muc_msg_info(_,From,Timestamp,Num) ->
	Servers = ejabberd_config:get_myhosts(),
	LServer = lists:nth(1,Servers),
	case catch odbc_queries:get_muc_msg_info1(jlib:nameprep(LServer),From,Timestamp,Num) of
	{selected,  [<<"muc_room_name">>,<<"nick">>,<<"packet">>], SRes}
		when is_list(SRes) ->
			lists:reverse(lists:map(fun([Lfrom,Lto,Lbody]) ->
			        {obj,[{"M",Lfrom},{"N",Lto},{"B",Lbody}]}
			        end ,SRes));
	 _ ->
	        []
	end.

get_user_last(LServer,User,Room) ->
	case catch  ejabberd_odbc:sql_query(LServer,
			[<<"select date from muc_room_users  where username = '">>,User,<<"' and muc_name = '">>,Room,<<"';">>]) of	
	{selected, [<<"date">>], [[D]]}  ->
		 [{obj,[{"M",<<"">>},{"N",<<"">>},{"B",<<"">>},{"T",http_utils:to_integer(D,0)}]}];
	_ ->
		[]
	end.

user_in_room(_User,_From)  ->
	true.
