%% Feel free to use, reuse and abuse the code in this file.

-module(http_get_msgs).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-export([get_virtual_user_msgs/3]).

-include("logger.hrl").
-include("http_req.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _} = cowboy_req:method(Req),
    catch ejb_monitor:monitor_count(<<"http_get_msgs_domain">>,1),
	case Method of 
	<<"GET">> ->
	{Host,_} =  cowboy_req:host(Req),
	{ok, Req2} = get_echo(Method,Host,Req),
	{ok, Req2, State};
	<<"POST">> ->
    HasBody = cowboy_req:has_body(Req),
	{ok, Req2} = post_echo(Method,HasBody,Req),
	{ok, Req2, State};
	_ ->
	{ok,Req2} = echo(undefined, Req),
	{ok, Req2, State}
	end.
    	
post_echo(<<"POST">>, true, Req) ->
	{ok, Body, _} = cowboy_req:body(Req),
	case rfc4627:decode(Body) of
	{ok,{obj,Args} ,[]}  ->
		From = proplists:get_value("from",Args),	
		To = proplists:get_value("to",Args),	
	    {U,_} = cowboy_req:qs_val(<<"u">>, Req),
        Rslt = 
    		case U == From orelse U == To  of 
	    	true ->
                get_msgs(Req,Args,From,To);
    		false ->
                case get_virtual_user_msgs(From,To,U) of
                false ->
                    http_utils:gen_result(false, <<"-1">>, <<"User info error">>,[]);
                true ->
                    get_msgs(Req,Args,From,To)
                end
	    	end,
		cowboy_req:reply(200, [
			{<<"content-type">>, <<"text/plain; charset=utf-8">>}
		], Rslt, Req);
	_ ->
		cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain; charset=utf-8">>}],
				http_utils:gen_result(false, <<"-1">>, <<"Json parse error">>,[]), Req)
	end;
post_echo(_,_,Req) ->
	cowboy_req:reply(405, Req).

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

get_virtual_user_msgs(From,To,U) ->
    case catch pg_odbc:sql_query(<<"ejb_http_server">>,
            [<<"select virtual_user from virtual_user_list where on_duty_flag = '1' and real_user = '">>,U,<<"';">>]) of
    {selected, _ , SRes} when is_list(SRes) ->    
        lists:member([From],SRes) orelse lists:member([To],SRes);
    _ ->
        false
    end.


get_msgs(Req,Args,From,To) ->
	From_host = proplists:get_value("from_host",Args),	
	To_host = proplists:get_value("to_host",Args),	
	{Platform,_} = cowboy_req:qs_val(<<"p">>, Req),
    Ret = 
	    case http_utils:verify_user_key(Req) of
		true ->
		    Timestamp1 = proplists:get_value("timestamp",Args),	
			Num1 = proplists:get_value("limitnum",Args),	
			Direction = proplists:get_value("direction",Args),	
			Time = handle_time(Timestamp1,Direction),	
			Timestamp = ejb_public:format_time(Time),
			Num = handle_num(Num1),
			case {From,To} of
                	{_,<<"admin">>} ->
	                    [];
			_ ->
			    case ejb_public:check_time(Timestamp1) of
				true ->
					get_chat_msg(Direction,From,From_host,To,To_host,Platform,Timestamp,Num);
				_ ->
					get_chat_msg_backup(Direction,From,From_host,To,To_host,Platform,Timestamp,Num)
				end
			end;
		_ ->
			[]
    		end,
	http_utils:gen_result(true ,<<"0">>,<<"">>,Ret).

get_chat_msg(<<"1">>,From,From_host,To,To_host,Platform,Timestamp,Num) ->
	case check_time_limit(From,To,Platform) of
	true ->
		case check_warn_jid(From,To) of
		true ->
			{F1,T1} = get_another_jid(From,To),
			get_warn_msg(F1,T1,From_host,To_host,Timestamp,http_utils:to_binary(Num,<<"10">>));
		_ ->	
			get_msg_info(From,To,From_host,To_host,Timestamp,http_utils:to_binary(Num,<<"10">>))
		end;
	_ ->
		[]
	end;
get_chat_msg(_,From,From_host,To,To_host,Platform,Timestamp,Num) ->
	case check_time_limit(From,To,Platform) of
	true ->
		case check_warn_jid(From,To) of
		true ->
			{F1,T1} = get_another_jid(From,To),
			get_warn_msg1(F1,T1,From_host,To_host,Timestamp,http_utils:to_binary(Num,<<"10">>));
		_ ->	
			get_msg_info1(From,To,From_host,To_host,Timestamp,http_utils:to_binary(Num,<<"10">>))
		end;
	_ ->
		[]
	end.

get_msg_info(From,To,From_host,To_host,Timestamp,Num) ->
%%	case catch ejb_odbc_query:get_msg_info3(From,From_host,To,To_host,Timestamp,Num) of
	case catch ejb_odbc_query:get_chat_msg(From,From_host,To,To_host,Timestamp,Num) of
	{selected,  [<<"m_from">>,<<"from_host">>,<<"m_to">>,<<"to_host">>,<<"m_body">>,<<"read_flag">>], SRes}
		when is_list(SRes) ->
			case length(SRes) of
			0 ->
				get_msg_info_backup(From,To,From_host,To_host,Timestamp,Num);
			_ ->	
				lists:map(fun([Lfrom,Lfrom_host,Lto,Lto_host,Lbody,Rflag]) ->
					Flag = case Rflag of
					<<"0">> ->
						0;
					_ ->
						1
					end,
					{obj,[{"F",Lfrom},{"FH",Lfrom_host},{"T",Lto},{"TH",Lto_host},{"B",Lbody},{"R",Flag}]}
				end ,SRes)
			end;
		A ->
			?DEBUG("A ~p ~n",[A]),
			[]
		end.
							
get_msg_info1(From,To,From_host,To_host,Timestamp,Num) ->
	case catch ejb_odbc_query:get_chat_msg1(From,From_host,To,To_host,Timestamp,Num) of
	{selected,  [<<"m_from">>,<<"from_host">>,<<"m_to">>,<<"to_host">>,<<"m_body">>,<<"read_flag">>], SRes}
		when is_list(SRes) ->
			case length(SRes) of
			0 ->
				get_msg_info1_backup(From,To,From_host,To_host,Timestamp,Num); 
			_ ->
				lists:reverse(lists:map(fun([Lfrom,Lfrom_host,Lto,Lto_host,Lbody,Rflag]) ->
					Flag = case Rflag of
					<<"0">> ->
						0;
					_ ->
						1
					end,
					{obj,[{"F",Lfrom},{"FH",Lfrom_host},{"T",Lto},{"TH",Lto_host},{"B",Lbody},{"R",Flag}]}
				end ,SRes))
			end;
		_ ->
			[]
		end.

check_time_limit(From,To,Clinet_type) ->
	true;
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


handle_time(Time) when is_integer(Time)->
	if Time > 1662768873 ->
		mod_time:get_timestamp() - 3600*24;
	true ->
		Time
	end;
handle_time(_) ->
	mod_time:get_timestamp() - 3600*24.
			
handle_num(Num) when is_binary(Num)->
	INum = binary_to_integer(Num),
	if INum < 10 ->
		<<"10">>;
	true ->
		Num
	end;
handle_num(Num) when is_integer(Num) ->
	if Num < 10 ->
		<<"10">>;
	true ->
		Num
	end;
handle_num(Num) ->
	<<"10">>.
	
get_warn_msg(From,To,From_host,To_host,Timestamp,Num) ->
	case catch pg_odbc:sql_query(<<"ejb_http_server">>,
		[<<"select m_from,m_to,m_body,read_flag from warn_msg_history where m_from = '">>,To,<<"' and m_to = '">>,
		From,<<"' and create_time > '">>,Timestamp,<<"' order by id asc limit ">>,Num,<<";">>]) of
	{selected,  [<<"m_from">>,<<"m_to">>,<<"m_body">>,<<"read_flag">>], SRes}	when is_list(SRes) ->
			lists:map(fun([Lfrom,Lto,Lbody,Rflag]) ->
				Flag = case Rflag of
				<<"0">> ->
					0;
				_ ->
					1
				end,
				{obj,[{"F",Lfrom},{"FH",<<"ejabhost1">>},{"T",Lto},{"TH",<<"ejabhost1">>},{"B",Lbody},{"R",Flag}]}
			end ,SRes);
		A ->
			?DEBUG("A ~p ~n",[A]),
			[]
		end.
							
get_warn_msg1(From,To,From_host,To_host,Timestamp,Num) ->
	case catch pg_odbc:sql_query(<<"ejb_http_server">>,
		[<<"select m_from,m_to,m_body,read_flag from warn_msg_history where m_from = '">>,To,<<"' and m_to = '">>
			,From,<<"' and create_time < '">>,Timestamp,<<"' order by id desc limit ">>,Num,<<";">>]) of
	{selected,  [<<"m_from">>,<<"m_to">>,<<"m_body">>,<<"read_flag">>], SRes}
		when is_list(SRes) ->
	 		lists:reverse(lists:map(fun([Lfrom,Lto,Lbody,Rflag]) ->
				Flag = case Rflag of
				<<"0">> ->
					0;
				_ ->
					1
				end,
				{obj,[{"F",Lfrom},{"FH",<<"ejabhost1">>},{"T",Lto},{"TH",<<"ejabhost1">>},{"B",Lbody},{"R",Flag}]}
				end ,SRes));
		A ->
			?DEBUG("A ~p ~n",[A]),
			[]
		end.

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

handle_time(Time,Direc) ->
        case Direc of
        <<"0">> ->
               if Time =:= 0 orelse Time =:= <<"0">> ->
                        mod_time:get_timestamp();
			                true ->
					                        ITime = http_utils:to_integer(Time),
                   if ITime > 15006744073709500 ->
                             mod_time:get_timestamp();
                      true ->
                            Time
                      end
                end;
       _ ->
              Time
       end.

get_msg_info_backup(From,To,From_host,To_host,Timestamp,Num) ->
%	case catch ejb_odbc_query:get_chat_msg_backup(From,From_host,To,To_host,Timestamp,Num) of
	case catch ejb_odbc_query:get_chat_msg2_backup(From,From_host,To,To_host,Timestamp,Num) of
	{selected,  [<<"m_from">>,<<"from_host">>,<<"m_to">>,<<"to_host">>,<<"m_body">>,<<"read_flag">>], SRes}
		when is_list(SRes) ->
			lists:map(fun([Lfrom,Lfrom_host,Lto,Lto_host,Lbody,Rflag]) ->
				Flag = case Rflag of
				<<"0">> ->
					0;
				_ ->
					1
				end,
				{obj,[{"F",Lfrom},{"FH",Lfrom_host},{"T",Lto},{"TH",Lto_host},{"B",Lbody},{"R",Flag}]}
			end ,SRes);
		A ->
			?DEBUG("A ~p ~n",[A]),
			[]
		end.
							
get_msg_info1_backup(From,To,From_host,To_host,Timestamp,Num) ->
%%	case catch ejb_odbc_query:get_chat_msg1_backup(From,From_host,To,To_host,Timestamp,Num) of
	case catch ejb_odbc_query:get_chat_msg3_backup(From,From_host,To,To_host,Timestamp,Num) of
	{selected,  [<<"m_from">>,<<"from_host">>,<<"m_to">>,<<"to_host">>,<<"m_body">>,<<"read_flag">>], SRes}
		when is_list(SRes) ->
            case length(SRes) of
            0 ->
                get_msg_info1_backup_2016(From,To,From_host,To_host,Timestamp,Num);
            _ ->
			    lists:reverse(lists:map(fun([Lfrom,Lfrom_host,Lto,Lto_host,Lbody,Rflag]) ->
				    Flag = case Rflag of
    				<<"0">> ->
	    				0;
		    		_ ->
			    		1
		    		end,
				    {obj,[{"F",Lfrom},{"FH",Lfrom_host},{"T",Lto},{"TH",Lto_host},{"B",Lbody},{"R",Flag}]}
			    end ,SRes))
            end;
		_ ->
			[]
		end.

get_chat_msg_backup(<<"1">>,From,From_host,To,To_host,Platform,Timestamp,Num) ->
	case check_time_limit(From,To,Platform) of
	true ->
		case check_warn_jid(From,To) of
		true ->
			{F1,T1} = get_another_jid(From,To),
			get_warn_msg(F1,T1,From_host,To_host,Timestamp,http_utils:to_binary(Num,<<"10">>));
		_ ->	
			get_msg_info_backup(From,To,From_host,To_host,Timestamp,http_utils:to_binary(Num,<<"10">>))
		end;
	_ ->
		[]
	end;
get_chat_msg_backup(_,From,From_host,To,To_host,Platform,Timestamp,Num) ->
	case check_time_limit(From,To,Platform) of
	true ->
		case check_warn_jid(From,To) of
		true ->
			{F1,T1} = get_another_jid(From,To),
			get_warn_msg1(F1,T1,From_host,To_host,Timestamp,http_utils:to_binary(Num,<<"10">>));
		_ ->	
			get_msg_info1_backup(From,To,From_host,To_host,Timestamp,http_utils:to_binary(Num,<<"10">>))
		end;
	_ ->
		[]
	end.

get_msg_info1_backup_2016(From,To,From_host,To_host,Timestamp,Num) ->
	case catch ejb_odbc_query:get_chat_msg5_backup(From,From_host,To,To_host,Timestamp,Num) of
	{selected,  [<<"m_from">>,<<"from_host">>,<<"m_to">>,<<"to_host">>,<<"m_body">>,<<"read_flag">>], SRes}
		when is_list(SRes) ->
			    lists:reverse(lists:map(fun([Lfrom,Lfrom_host,Lto,Lto_host,Lbody,Rflag]) ->
				    Flag = case Rflag of
    				<<"0">> ->
	    				0;
		    		_ ->
			    		1
		    		end,
				    {obj,[{"F",Lfrom},{"FH",Lfrom_host},{"T",Lto},{"TH",Lto_host},{"B",Lbody},{"R",Flag}]}
			    end ,SRes));
		_ ->
			[]
		end.
