%% Feel free to use, reuse and abuse the code in this file.

-module(http_qmonitor).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-include("ejabberd.hrl").
-include("logger.hrl").
-include("http_req.hrl").

-record(monitor_rec,{key,count,time}).
-record(monitor_val,{key,value}).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, Req2} = cowboy_req:method(Req),
	case Method of 
	<<"GET">> ->
	{ok, Req4} = get_echo(Method,<<"pidgin">>,Req2),
	{ok, Req4, State};
	_ ->
	{ok,Req3} = echo(undefined, Req2),
	{ok, Req3, State}
	end.
    	
get_echo(<<"GET">>,Client_type,Req) ->
	if Client_type == <<"pidgin">>; Client_type == <<"qtalk">> ->
		Res = get_monitor_info(),
		cowboy_req:reply(200, [
			{<<"content-type">>, <<"text/plain; charset=utf-8">>}
		], Res, Req);
	 true ->
		Res2 = "Not found Client Type",
		cowboy_req:reply(200, [
			{<<"content-type">>, <<"text/plain; charset=utf-8">>}
		], Res2, Req)
	end;
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


get_monitor_info() ->
	Monitor_sum  = ets:tab2list(monitor_sum),
	Monitor_info = 
		case Monitor_sum of
		[] ->
			<<"user_login_Count=0\nuser_login_Time=0\n">>;
		_ ->
			list_to_binary(lists:flatmap(fun(M) ->
				case erlang:is_record(M,monitor_rec) of 
				true ->
					lists:concat([binary_to_list(M#monitor_rec.key),"_Count=",M#monitor_rec.count,"\n",
							  binary_to_list(M#monitor_rec.key),"_Time=",io_lib:format("~p",[M#monitor_rec.time]),"\n"]);
				false ->
					lists:concat([binary_to_list(M#monitor_val.key),"=",M#monitor_val.value,"\n" ])
				 end end,Monitor_sum))
		end,
	  [{Pid, MsgNum, Attr}] = recon:proc_count(message_queue_len, 1),
	   ?INFO_MSG("the process(~p) message_queue num is ~p, and the attr is ~p~n", [Pid, MsgNum, Attr]),
	   NewMsgNum = integer_to_binary(MsgNum),
		<<"max_msg_queue=", NewMsgNum/binary,"\n", Monitor_info/binary>>.
			
