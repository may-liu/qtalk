%% Feel free to use, reuse and abuse the code in this file.
%%========================================================
%%发送机器人消息接口
%%========================================================

-module(http_send_rbt_msg).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("http_req.hrl").

-record(user_rbts,{name,rbt}).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
   %% handle(Req, State, iplimit_util:check_ip(Req)).
    handle(Req, State, true).

handle(Req, State, false) ->
    Res = http_utils:gen_result(false, <<"3">>, <<"ip is limited">>),
    {ok, NewReq} = cowboy_req:reply(200, [
                                    {<<"content-type">>, <<"text/plain; charset=utf-8">>}
                                   ], Res, Req),
    {ok, NewReq, State};
handle(Req, State, _) ->
	{Method, _} = cowboy_req:method(Req),
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
			{<<"content-type">>, <<"text/json; charset=utf-8">>}
		], <<"No GET method">>, Req).

post_echo(<<"POST">>, true, Req) ->
    {ok, Body, _} = cowboy_req:body(Req),
    Servers = ejabberd_config:get_myhosts(),
    Server = lists:nth(1,Servers),
	case catch rfc4627:decode(Body) of
	{ok, {obj,Args },[]} -> 
		Res = 
			case http_utils:verify_user_key(Server,Req) of
			true ->
        		http_send_message(Server,Args);
			_ ->
           		http_utils:gen_result(false, 1, <<"Not found Mac_Key">>)
			end,
		cowboy_req:reply(200, [	{<<"content-type">>, <<"text/json; charset=utf-8">>}], Res, Req);
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

http_send_message(Server,Args)->
	From = proplists:get_value("From",Args),
	To = proplists:get_value("To",Args),
	Body  = proplists:get_value("Body",Args),
	Msg_type  = proplists:get_value("MsgType",Args),
	case is_suit_from(From) of
	true ->
		case To of
		undefined ->
			http_utils:gen_result(false, 2, <<"Json not find To">>);
		<<"subscription_users">> ->
			send_msg_to_all_subscription(Server,From,Body,Msg_type);
		_ ->
			Packet = ejabberd_public:make_send_packet(<<"subscription">>,Body),
			sendMsg(Server,From,To,Packet,Msg_type)
		end;
	false ->	
		http_utils:gen_result(false, 3, <<"From not suit">>)
	end.

is_suit_from(_From) ->
	true.	

send_msg_to_all_subscription(Server,From,Body,Msg_type) ->
	case catch ets:select(user_rbts,[{#user_rbts{rbt = From,name = '$1', _ = '_'},[], ['$1']}]) of
	Users when is_list(Users) ->
		case Body of 
		undefined ->
			http_utils:gen_result(false, 4, <<"Message Body is Null">>);
		_ ->
			Packet = ejabberd_public:make_send_packet(<<"subscription">>,Body),
			lists:foreach(fun(U) ->
				sendMsg(Server,From,U,Packet,Msg_type) end,Users),
			http_utils:gen_result(true, 0, <<"Send message Ok">>)
		end;
	_ ->
		http_utils:gen_result(false, 5, <<"Send message failed">>)
	end.

sendMsg(Server,From,To,Packet,_Msg_type) ->
	case jlib:make_jid(From,Server,<<"">>) of 
	error ->
		http_utils:gen_result(false, 2, <<"From make jid error">>);
	JFrom ->
		case jlib:make_jid(To,Server,<<"">>) of 
		error ->
			http_utils:gen_result(false, 3, <<"To make jid error">>);
		JTo ->
			?DEBUG("JFrom ~p ,JTo ~p,Packet ~p  ~n",[JFrom,JTo,Packet]),
			ejabberd_router:route(JFrom,JTo,Packet),
			http_utils:gen_result(true, 0, <<"Send message Ok">>)
		end
	end.
