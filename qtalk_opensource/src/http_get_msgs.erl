%% Feel free to use, reuse and abuse the code in this file.
%%========================================================
%%获取历史消息接口
%%========================================================

-module(http_get_msgs).

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
	{ok, Req2} = get_echo(Method,Req),
	{ok, Req2, State};
	_ ->
	{ok,Req2} = echo(undefined, Req),
	{ok, Req2, State}
	end.
    	
get_echo(<<"GET">>,Req) ->
	Servers = ejabberd_config:get_myhosts(),
	Server = lists:nth(1,Servers),
	Res = 
		case http_utils:verify_user_key(Req) of
		true ->
	    	{From,_} = cowboy_req:qs_val(<<"from">>, Req),
	    	{To,_} = cowboy_req:qs_val(<<"to">>, Req),
	   		{U,_} = cowboy_req:qs_val(<<"u">>, Req),
			case U =/= From andalso U =/= To  of 
		 	true->
            	http_utils:gen_result(false, 2, <<"User info not match">>);
			_ ->
		    	{Timestamp,_} = cowboy_req:qs_val(<<"timestamp">>, Req),
		    	{Num,_} = cowboy_req:qs_val(<<"limitnum">>, Req),
		    	{Direction,_} = cowboy_req:qs_val(<<"direction">>, Req),
				http_utils:gen_result(true,0,get_msg_info(Direction,Server,
									   ejabberd_odbc:escape(From),ejabberd_odbc:escape(To),Timestamp,Num))
			end;
		_ ->
       			http_utils:gen_result(false, 1, <<"Not found Mac_Key">>)
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

get_msg_info(<<"1">>,LServer,From,To,Timestamp,Num) ->
	case catch odbc_queries:get_msg_info(jlib:nameprep(LServer),From,To,Timestamp,Num) of
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
		end;
get_msg_info(_,LServer,From,To,Timestamp,Num) ->
    Res =   case catch odbc_queries:get_msg_info1(jlib:nameprep(LServer),From,To,Timestamp,Num) of
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
    lists:reverse(Res).

