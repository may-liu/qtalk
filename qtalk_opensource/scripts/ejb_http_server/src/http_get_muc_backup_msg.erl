%% Feel free to use, reuse and abuse the code in this file.
-module(http_get_muc_backup_msg).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-include("logger.hrl").
-include("http_req.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _} = cowboy_req:method(Req),
    catch ejb_monitor:monitor_count(<<"http_get_muc_backup_msg">>,1),
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
get_echo(<<"GET">>,_,Req) ->
	cowboy_req:reply(200, [
	           {<<"content-type">>, <<"text/plain; charset=utf-8">>}], 
				http_utils:gen_result(false, 1, <<"no get method">>,[]) , Req).

post_echo(<<"POST">>, true, Req) ->
	{ok, Body, _} = cowboy_req:body(Req),
	case rfc4627:decode(Body) of
	{ok,{obj,Args} ,[]}  ->
    	Domain = proplists:get_value("domain",Args),
		Ret =
			case http_utils:verify_user_key(Req) of	
			true ->
				case ejb_public:checek_domain(Domain) of
				true ->
			 		do_get_muc_backup_msg(Args);
				_ ->
					http_utils:gen_result(false, 2, <<"Not allow to get other domian muc_msg history">>,<<"">>)	
			%%		http_get_other_domain_muc_msg(post,Domain,Args,Req)
				end;
			_ ->
				http_utils:gen_result(false, 2, <<"Tkey check error">>,<<"">>)	
			end,
		cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain; charset=utf-8">>}],Ret, Req);
	_ ->
		Rslt = <<"Body parse error">>,
		cowboy_req:reply(200,[{<<"content-type">>, <<"text/json; charset=utf-8">>}],
				http_utils:gen_result(false, 3, Rslt,<<"">>), Req)
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
	case catch ejb_odbc_query:get_muc_msg_info5(From,Timestamp,Num) of
	{selected,  [<<"muc_room_name">>,<<"nick">>,<<"host">>,<<"packet">>], SRes}
			when is_list(SRes) ->
				lists:map(fun([Lfrom,Lto,Lhost,Lbody]) ->
						{obj,[{"M",Lfrom},{"N",Lto},{"H",Lhost},{"B",Lbody}]}
				end ,SRes);
	_ ->
			[]
	end.
							
get_muc_msg_info1(From,Timestamp,Num) ->
    case catch ejb_odbc_query:get_muc_msg_info6(From,Timestamp,Num) of
	{selected,  [<<"muc_room_name">>,<<"nick">>,<<"host">>,<<"packet">>], SRes}
		when is_list(SRes) ->
			lists:map(fun([Lfrom,Lto,Lhost,Lbody]) ->
			        {obj,[{"M",Lfrom},{"N",Lto},{"H",Lhost},{"B",Lbody}]}
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

do_get_muc_backup_msg(Args) ->
	Muc = pg_odbc:escape(proplists:get_value("muc_name",Args)),
	Start_Time = proplists:get_value("start_time",Args),
	End_Time = proplists:get_value("end_time",Args),
%%	Num = proplists:get_value("limit",Args),

    Muc_msg =
		case catch pg_odbc:sql_query(<<"ejb_http_server">>,
				[<<"select host,nick,packet from muc_room_history_backup where muc_room_name = '">>,Muc,
					<<"' and m_timestamp > ">>,http_utils:to_to_binary(Start_Time,<<"0">>),
					<<" and m_timestamp < ">>,http_utils:to_to_binary(End_Time,<<"0">>) ,<<" ;">>]) of
		 {selected,  _ , SRes } when is_list(SRes) ->
		 		lists:flatmap(fun([H,N,P]) ->
						[{obj, [{"Nick", N},{"Host",H}, {"Msg", P}]}] end,SRes);
		 _ ->
		 	[]
		end,
	http_utils:gen_result(true, 0, <<"">>,Muc_msg).


