%% Feel free to use, reuse and abuse the code in this file.

-module(http_gethistory).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("logger.hrl").
-include("http_req.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    catch ejb_monitor:monitor_count(<<"http_gethistory">>,1),
    do_handle(Req, State).

do_handle(Req, State) ->
	{Method, _} = cowboy_req:method(Req),
	case Method of 
	<<"GET">> ->
	    {Host,_ } = cowboy_req:host(Req),
		{ok, Req1} = get_echo(Method,Host,Req),
		{ok, Req1, State};
	<<"POST">> ->
		HasBody = cowboy_req:has_body(Req),
		{ok, Req1} = post_echo(Method, HasBody, Req),
		{ok, Req1, State};
	_ ->
		{ok,Req1} = echo(undefined, Req),
		{ok, Req1, State}
	end.
    	
get_echo(<<"GET">>,_,Req) ->
		cowboy_req:reply(200, [
			{<<"content-type">>, <<"text/json; charset=utf-8">>}
		], <<"No GET method">>, Req).

post_echo(<<"POST">>, true, Req) ->
    {ok, Body, _} = cowboy_req:body(Req),
    {Host,_} =  cowboy_req:host(Req),
	Req_compress = Req#http_req{resp_compress = true},
	case rfc4627:decode(Body) of
	{ok,Json,[]} -> 
		Rslt = 
			case http_utils:verify_user_key(Req) of
			true ->
        			http_get_history(Json);
			_ ->
            			http_utils:gen_result(false, <<"-1">>, <<"Not found Mac_Key">>)
			end,
		cowboy_req:reply(200, [	{<<"content-type">>, <<"text/json; charset=utf-8">>}], Rslt, Req_compress);
	_ ->
		 cowboy_req:reply(200, [ {<<"content-type">>, <<"text/json; charset=utf-8">>}], <<"Josn parse error">>, Req)
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

http_get_history(Json)->
	[{obj,Args }] = Json ,
	case  proplists:get_value("get_unread",Args) of
	<<"true">> ->
		Msg_ids = proplists:get_value("msg_ids",Args),
		BMsg_ids = 
			lists:foldl(fun(ID,Acc) ->
				case Acc of
				[] -> 
					lists:concat([Acc,[<<"'">>,ID,<<"'">>]]);
				_ ->
					lists:concat([Acc,[<<",'">>,ID,<<"'">>]])
				end end,[],Msg_ids),
		case catch pg_odbc:sql_query(<<"ejb_http_server">>,
				[<<"select msg_id from msg_history where read_flag = 0 and msg_id in (">>,BMsg_ids,<<");">>]) of
			{selected,[<<"msg_id">>],SRes} when is_list(SRes) ->
				http_utils:gen_result(true, <<"0">>, <<"Sucess">>,lists:concat(SRes));
			_ ->
				http_utils:gen_result(true, <<"1">>, <<"Sucess">>,[])
			end;
	_ ->
		User = pg_odbc:escape(case proplists:get_value("User",Args) of
			   	undefined ->
					<<"null">>;
			    U ->
					U
				end),
		Time = ejb_public:format_time(proplists:get_value("Time",Args)),
		Res1 = 
			case catch ejb_odbc_query:get_msg_info2(User,Time,<<"asc">>) of
			{selected,[<<"m_from">>,<<"m_to">>,<<"m_body">>,<<"read_flag">>],SRes} 
			when is_list(SRes) ->
				lists:map(fun([Lfrom,Lto,Lbody,Rflag]) ->
						{obj,[{"F",Lfrom},{"T",Lto},{"B",Lbody},{"R",Rflag}]}
						 end ,SRes);
			A  ->
				[]
			end,
		Res2 = get_warn_msg(User,Time,<<"asc">>),
		Rslt = Res2 ++ Res1,
		rfc4627:encode(Rslt)
	end.

get_warn_msg(User,Time,Dir) ->
	case catch pg_odbc:sql_query(<<"ejb_http_server">>,
		[<<"select m_from,m_to,m_body,read_flag from warn_msg_history where (m_from = '">>,User, <<"' or m_to = '">>,User,
			<<"') and create_time > '">>,Time,<<"' order by id ">>,Dir,<<";">>]) of
	{selected,[<<"m_from">>,<<"m_to">>,<<"m_body">>,<<"read_flag">>],SRes}	 when is_list(SRes) ->
		lists:map(fun([Lfrom,Lto,Lbody,Rflag]) ->
					  {obj,[{"F",Lfrom},{"T",Lto},{"B",Lbody},{"R",Rflag}]}
					  end, SRes);
		_ ->
			[]
	end.
