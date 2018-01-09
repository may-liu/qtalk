-module(http_send_warn_msg).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-export([http_send_message/1]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("http_req.hrl").

-record(session, {sid, usr, us, priority, info, show}).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _} = cowboy_req:method(Req),
	case Method of 
	<<"GET">> ->
	    {Host,_} =  cowboy_req:host(Req),
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
	case rfc4627:decode(Body) of
	{ok, {obj,Args},[]} -> 
		Res = http_send_message(Args),
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
	
http_send_message(Json) ->
    Servers = ejabberd_config:get_myhosts(),
    Server = lists:nth(1,Servers),
	http_send_message(Server,Json).

http_send_message(Server,Args)->
	From = proplists:get_value("From",Args),
	To = proplists:get_value("To",Args),
	Body  = proplists:get_value("Body",Args),
	Msg_Type  = proplists:get_value("Msg_Type",Args),
	Host = proplists:get_value("Host",Args,Server),
	Extend_Info  =	proplists:get_value("Extend_Info",Args,<<"">>),
	send_chat_msg(Server,From,To,Host,Body,Msg_Type,Extend_Info).

send_chat_msg(Server,From,undefined,Host,Body,Msg_Type,Extend_Info) ->
	http_utils:gen_result(false,1,<<"no found To jid">>,<<"">>);
send_chat_msg(Server,From,undefined,Host,Body,Msg_Type,Extend_Info) ->
	http_utils:gen_result(false,1,<<"no found send msg Body">>,<<"">>);
send_chat_msg(Server,From,To,Host,Body,Msg_Type,Extend_Info) ->
	case is_suit_from(From) of
	true ->
		case jlib:make_jid(From,Server,<<"">>) of
		error ->
			http_utils:gen_result(false,1,<<"make From jid error">>,<<"">>);
		JFrom ->
			Packet = ejabberd_public:make_message_packet(<<"chat">>,Body,Extend_Info,Msg_Type),
			Send_Res =
				lists:flatmap(fun({obj,[{"User",ToU}]}) ->
					case jlib:make_jid(ToU,Server,<<"">>) of 
					error ->
						[<<",">>,ToU];
					JTo ->
						ejabberd_router:route(JFrom,JTo,Packet),
						case judge_user_status(ToU,Server,Host) of
						true ->
							?DEBUG("!! ~p ~n",[ToU]),
							[];
						_ ->
							?DEBUG("~p ~n",[ToU]),
							[<<",">>,ToU]
						end
					end	end,To),
			case length(Send_Res) of 
			0 ->
				http_utils:gen_result(true,0,<<"send warn msg sucess">>,<<"">>);
			_ ->
				[_|Res] = Send_Res,
				http_utils:gen_result(false,2,<<"send warn msg eror">>,list_to_binary(Res))
			end
		end;
	false ->	
		http_utils:gen_result(false,3,<<"from not suit">>,<<"">>)
	end.

is_suit_from(_From) ->
	true.	

judge_user_status(User,Server,Host) ->
    if Server =:= Host  ->
	    case ejabberd_sm:get_user_session(User,Server) of
	    [] ->
	    	false;
	    Ss -> 
	    	Rs = lists:map(fun(S) ->
	    			{S#session.show,element(3, S#session.usr)} end,Ss),
			
	    	Res = ejabberd_sm:judge_away_flag(Rs,true),
    		not Res
    	end;
    true ->
        true
    end.
	
