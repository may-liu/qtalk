%% Feel free to use, reuse and abuse the code in this file.

-module(http_getmucmsg).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-include("logger.hrl").
-include("http_req.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _} = cowboy_req:method(Req),
	catch ejb_monitor:monitor_count(<<"http_get_muc_msg">>,1),
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
    	
get_echo(<<"GET">>,Host,Req) ->
	Req_compress = Req#http_req{resp_compress = true},
	 Rslt = 
		case http_utils:verify_user_key(Req) of
		true ->
		 do_get_muc_msg(get,<<"">>,Req);
		_ ->
%%       		http_utils:gen_result(false, <<"-1">>, <<"Not found Mac_Key">>)
			[]
		end,
	cowboy_req:reply(200,[
		{<<"content-type">>, <<"text/plain; charset=utf-8">>}
	], Rslt, Req_compress);
get_echo(<<"Get">>,_,Req) ->
	cowboy_req:reply(405, Req).

post_echo(<<"POST">>, true, Req) ->
	Req_compress = Req#http_req{resp_compress = true},
	{ok, Body, _} = cowboy_req:body(Req),
	{Host,_} =  cowboy_req:host(Req),
	case rfc4627:decode(Body) of
	{ok,{obj,Args} ,[]}  ->
		case http_utils:check_version(Req) of
		false ->
			Rslt = do_get_muc_msg(post,Args,Req),
			cowboy_req:reply(200, [	{<<"content-type">>, <<"text/plain; charset=utf-8">>}],
				   http_utils:gen_result(true, <<"0">>, <<"">>,Rslt),Req_compress);
		_ ->
			case http_utils:verify_user_key(Req) of	
			true ->
				Rslt = do_get_muc_msg(post,Args,Req),
				cowboy_req:reply(200, [	{<<"content-type">>, <<"text/plain; charset=utf-8">>}],
					 http_utils:gen_result(true, <<"0">>, <<"">>,Rslt),Req_compress);
			_ ->
				cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain; charset=utf-8">>}],
					 http_utils:gen_result(false, <<"-1">>, <<"Vesion Check Error">>,<<"">>), Req_compress)
			end
		end;
	_ ->
		Rslt = <<"Body parse error">>,
		cowboy_req:reply(200,[{<<"content-type">>, <<"text/json; charset=utf-8">>}],
				http_utils:gen_result(false, <<"-1">>, Rslt,<<"">>), Req_compress)
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

get_muc_msg_info(From,Timestamp,Num) ->
	case catch ejb_odbc_query:get_muc_msg_info(From,Timestamp,Num) of
	{selected,  [<<"muc_room_name">>,<<"nick">>,<<"packet">>], SRes}
			when is_list(SRes) ->
				lists:map(fun([Lfrom,Lto,Lbody]) ->
						{obj,[{"M",Lfrom},{"N",Lto},{"B",Lbody}]}
				end ,SRes);
	_ ->
			[]
	end.
							
get_muc_msg_info1(From,Timestamp,Num) ->
    case catch ejb_odbc_query:get_muc_msg_info1(From,Timestamp,Num) of
	{selected,  [<<"muc_room_name">>,<<"nick">>,<<"packet">>], SRes}
		when is_list(SRes) ->
			lists:map(fun([Lfrom,Lto,Lbody]) ->
			        {obj,[{"M",Lfrom},{"N",Lto},{"B",Lbody}]}
			        end ,SRes);
			 _ ->
			        []
			end.

get_user_last(User,Room) ->
	case catch  pg_odbc:sql_query( <<"ejb_http_server">>,
			[<<"select date from muc_room_users  where username = '">>,User,<<"' and muc_name = '">>,Room,<<"';">>]) of	
	{selected, [<<"date">>], [[D]]}  ->
		to_integer(D);
	_ ->
		0
	end.

to_integer(V) when is_binary(V) ->
    binary_to_integer(V);
to_integer(V) when is_integer(V) ->
    V;
to_integer(_) ->
	0.
do_get_muc_msg(get,_,Req) ->
    {From,_} = cowboy_req:qs_val(<<"muc_name">>, Req),
    {Timestamp,_} = cowboy_req:qs_val(<<"timestamp">>, Req),
    {Num,_} = cowboy_req:qs_val(<<"limitnum">>, Req),
    {Direction,_} = cowboy_req:qs_val(<<"direction">>, Req),
	{User,_} = cowboy_req:qs_val(<<"u">>, Req),

	Muc_msg = case Direction of
		<<"1">> ->
			get_muc_msg_info(pg_odbc:escape(From),Timestamp,Num);
		_ ->
			get_muc_msg_info1(pg_odbc:escape(From),Timestamp,Num)
		end,
	Rslt = 
		case User of
		undefined ->
			lists:reverse(Muc_msg);
		_ ->
			lists:reverse(Muc_msg)
		end,
	rfc4627:encode(Rslt);
do_get_muc_msg(post,Args,Req) ->
	From = pg_odbc:escape(proplists:get_value("muc_name",Args)),
	Timestamp = proplists:get_value("timestamp",Args),
	Num = proplists:get_value("limitnum",Args),
	Direction = proplists:get_value("direction",Args),
	{User,_} = cowboy_req:qs_val(<<"u">>, Req),

    Muc_msg =
   		case Direction of
		<<"1">> ->
			get_muc_msg_info(From,Timestamp,Num);
		_ ->
			get_muc_msg_info1(From,Timestamp,Num)
		end,
	Time = 
		case User of
		undefined ->
			0;
		_ ->
			get_user_last(User,From)
		end,
	{obj, [{"Time", Time}, {"Msg", lists:reverse(Muc_msg)}]}.
