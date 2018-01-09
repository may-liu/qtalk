%% Feel free to use, reuse and abuse the code in this file.

-module(http_getmsgs).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("logger.hrl").
-include("http_req.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _} = cowboy_req:method(Req),
    catch ejb_monitor:monitor_count(<<"http_get_msgs">>,1),
	case Method of 
	<<"GET">> ->
	{Host,_} =  cowboy_req:host(Req),
	{ok, Req2} = get_echo(Method,Host,Req),
	{ok, Req2, State};
	_ ->
	{ok,Req2} = echo(undefined, Req),
	{ok, Req2, State}
	end.
    	
get_echo(<<"GET">>,Host,Req) ->
	Req_compress = Req#http_req{resp_compress = true},
	{From,_} = cowboy_req:qs_val(<<"from">>, Req),
	{To,_} = cowboy_req:qs_val(<<"to">>, Req),
	{U,_} = cowboy_req:qs_val(<<"u">>, Req),
	{Platform,_} = cowboy_req:qs_val(<<"p">>, Req),
	Rslt = 
		case U == From orelse U == To  of 
		true ->
			case http_utils:verify_user_key(Req) of
			true ->
	    			{Timestamp1,_} = cowboy_req:qs_val(<<"timestamp">>, Req),
	    			{Num,_} = cowboy_req:qs_val(<<"limitnum">>, Req),
	    			{Direction,_} = cowboy_req:qs_val(<<"direction">>, Req),
				Timestamp = ejb_public:format_time(Timestamp1),

				case Direction of
				<<"1">> ->
					case check_time_limit(From,To,Platform) of
					true ->
						case check_warn_jid(From,To) of
						true ->
							{F1,T1} = get_another_jid(From,To),
							get_warn_info(pg_odbc:escape(F1),pg_odbc:escape(T1),Timestamp,Num);
						_ ->
							get_msg_info(pg_odbc:escape(From),pg_odbc:escape(To),Timestamp,Num)
						end;
					_ ->
						[]
					end;
				_ ->
					case check_time_limit(From,To,Platform) of
					true ->
						case check_warn_jid(From,To) of
						true ->
							{F1,T1} = get_another_jid(From,To),
							get_warn_info1(pg_odbc:escape(F1),pg_odbc:escape(T1),Timestamp,Num);
						_ ->
							get_msg_info1(pg_odbc:escape(From),pg_odbc:escape(To),Timestamp,Num)
						end;
					_ ->
						[]
					end
				end;
			_ ->
				[]
			end;
		false ->
       		     	http_utils:gen_result(false, <<"-1">>, <<"User info error">>)
		end,
	cowboy_req:reply(200, [
		{<<"content-type">>, <<"text/plain; charset=utf-8">>}
	], Rslt, Req_compress);
get_echo(<<"Get">>,_,Req) ->
	cowboy_req:reply(405, Req).

echo(undefined, Req) ->
    cowboy_req:reply(400, [], <<"Missing parameter.">>, Req);
echo(Echo, Req) ->
    cowboy_req:reply(200, [
			        {<<"content-type">>, <<"text/plain; charset=utf-8">>}
	    			    ], Echo, Req).

terminate(_Reason, _Req, _State) ->
	ok.

get_msg_info(From,To,Timestamp,Num) ->
	Rslt =	
		case catch ejb_odbc_query:get_msg_info(From,To,Timestamp,Num) of
		{selected,  [<<"m_from">>,<<"m_to">>,<<"m_body">>,<<"read_flag">>], SRes}
			when is_list(SRes) ->
				lists:map(fun([Lfrom,Lto,Lbody,Rflag]) ->
					Flag = case Rflag of
					<<"0">> ->
						0;
					_ ->
						1
					end,
					{obj,[{"F",Lfrom},{"T",Lto},{"B",Lbody},{"R",Flag}]}
				end ,SRes);
			_ ->
				[]
			end,
	rfc4627:encode(Rslt).
							
get_msg_info1(From,To,Timestamp,Num) ->
	Rslt =	
		case catch ejb_odbc_query:get_msg_info1(From,To,Timestamp,Num) of
		{selected,  [<<"m_from">>,<<"m_to">>,<<"m_body">>,<<"read_flag">>], SRes}
			when is_list(SRes) ->
				lists:map(fun([Lfrom,Lto,Lbody,Rflag]) ->
					Flag = case Rflag of
					<<"0">> ->
						0;
					_ ->
						1
					end,
					{obj,[{"F",Lfrom},{"T",Lto},{"B",Lbody},{"R",Flag}]}
				end ,SRes);
			_ ->
				[]
			end,
	rfc4627:encode(lists:reverse(Rslt)).

check_time_limit(From,To,Clinet_type) ->
	case Clinet_type  == <<"qim_windows">>  orelse Clinet_type == <<"qim_linux">> of 
	true ->
	Key  = str:concat(From,To),
	Now = get_time(),
	case ets:lookup(getmsg_limit,Key) of
	[] ->
		ets:insert(getmsg_limit,{Key,Now}),
		true;
	[{_,T}] ->
		Diff = Now - T, 
		case Diff > 250000 of
		true ->
			ets:insert(getmsg_limit,{Key,Now}),
			true;
		_ ->
			false
		end;
	_ ->
		true
	end;
	_ ->
		true
	end.

get_time() ->
 	{MegaSecs, Secs,MicroSec} = os:timestamp(),
	MegaSecs * 1000000000000 + Secs*1000000 + MicroSec.

get_warn_info(From,To,Timestamp,Num) ->
	Rslt =	
		case catch pg_odbc:sql_query(<<"ejb_http_server">>,
			[<<"select m_from,m_to,m_body,read_flag from warn_msg_history where (m_from,from_host) in (('">>,To,<<"','">>,
				From,<<"')) and create_time > '">>,Timestamp,<<"' order by create_time asc limit ">>,Num,<<";">>]) of
			{selected,  [<<"m_from">>,<<"m_to">>,<<"m_body">>,<<"read_flag">>], SRes}
				when is_list(SRes) ->
					lists:map(fun([Lfrom,Lto,Lbody,Rflag]) ->
						Flag = case Rflag of
						<<"0">> ->
							0;
						_ ->
							1
						end,
						{obj,[{"F",Lfrom},{"T",Lto},{"B",Lbody},{"R",Flag}]}
					end ,SRes);
				_ ->
					[]
			end,
		rfc4627:encode(Rslt).
							
get_warn_info1(From,To,Timestamp,Num) ->
	Rslt =	
		case catch pg_odbc:sql_query(<<"ejb_http_server">>,
			[<<"select m_from,m_to,m_body,read_flag from warn_msg_history where (m_from,m_to) in (('">>,To,<<"','">>,
				From,<<"')) and create_time < '">>,Timestamp,<<"' order by id desc limit ">>,Num,<<";">>]) of
		{selected,  [<<"m_from">>,<<"m_to">>,<<"m_body">>,<<"read_flag">>], SRes}
			when is_list(SRes) ->
				lists:map(fun([Lfrom,Lto,Lbody,Rflag]) ->
					Flag = case Rflag of
					<<"0">> ->
						0;
					_ ->
						1
					end,
					{obj,[{"F",Lfrom},{"T",Lto},{"B",Lbody},{"R",Flag}]}
				end ,SRes);
			_ ->
				[]
			end,
	rfc4627:encode(lists:reverse(Rslt)).

check_warn_jid(From,To) ->
	if From =:= <<"ops-robot">> orelse To =:= <<"ops-robot">>  orelse From =:= <<"qunar-message">> orelse To =:= <<"qunar-message">> ->
		true;
	true ->
		false
	end.
		

get_another_jid(From,To) ->
	if From =:= <<"ops-robot">> orelse From =:= <<"qunar-message">> ->
		{To,From};
	true ->
		{From,To}
	end.
