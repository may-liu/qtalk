-module(http_wlan_send_msg).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-export([http_send_message/1]).
-include("ejabberd.hrl").
-include("logger.hrl").
-include("http_req.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, Req2} = cowboy_req:method(Req),
	case Method of 
	<<"GET">> ->
	    {Host,Req3} =  cowboy_req:host(Req),
		{ok, Req4} = get_echo(Method,Host,Req3),
		{ok, Req4, State};
	<<"POST">> ->
		HasBody = cowboy_req:has_body(Req2),
		{ok, Req3} = post_echo(Method, HasBody, Req2),
		{ok, Req3, State};
	_ ->
		{ok,Req3} = echo(undefined, Req2),
		{ok, Req3, State}
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
	From = proplists:get_value("from",Args),
	To = proplists:get_value("to",Args),
	Body  = proplists:get_value("body",Args),
	Type  = proplists:get_value("type",Args),
	Msg_Type  = proplists:get_value("msg_type",Args,<<"1">>),
	Host = proplists:get_value("host",Args,Server),
	Domain  = proplists:get_value("domain",Args),
	Extend_Info  =	proplists:get_value("extend_info",Args,<<"">>),
	case Type of 
	<<"groupchat">> ->
		send_muc_msg(Server,From,To,Host,Domain,Body,Extend_Info,Msg_Type);
	_ ->
		send_chat_msg(Server,From,To,Host,Body,Msg_Type,Extend_Info)
	end.

send_chat_msg(Server,From,undefined,Host,Body,Msg_Type,Extend_Info) ->
	http_utils:gen_result(false, 1,<<"To no suit">>,<<"">>);
send_chat_msg(Server,From,To,Host,undefined,Msg_Type,Extend_Info) ->
	http_utils:gen_result(false, 1,<<"Body is null">>,<<"">>);
send_chat_msg(Server,From,To,Host,Body,Msg_Type,Extend_Info) ->
	case is_suit_from(From) of
	true ->
		JFrom = jlib:make_jid(From,Server,<<"">>),
		case JFrom of 
		error ->
			http_utils:gen_result(false, 1,<<"Make from jid error">>,<<"">>);
		_ ->
			SendRes =
				lists:flatmap(fun({obj,[{"user",ToU}]}) ->
					case jlib:make_jid(ToU,Host,<<"">>) of 
					error ->
						[<<",">>,ToU];
					JTo ->
						Packet = ejabberd_public:make_message_packet(<<"chat">>,Body,Extend_Info,Msg_Type),
							ejabberd_router:route(JFrom,JTo,Packet),
						Packet1 = make_carbon_packet(JFrom,Body,Extend_Info,Msg_Type),
							ejabberd_router:route(JTo,JFrom,Packet1),
							[]
						end	end,To),
				case length(SendRes) of 
				0 ->
					http_utils:gen_result(true, 0,<<"Send msg sucess">>,<<"">>);
				_ ->
					[_|Res] = SendRes,
					http_utils:gen_result(false, 1,<<"Send msg error">>,list_to_binary(Res))
				end
		end;
	false ->	
		http_utils:gen_result(false, 1,<<"From not suit">>,<<"">>)
	end.

is_suit_from(_From) ->
	true.	

send_muc_msg(Server,User,Room,_Host,Domain,undefined,Extend_Info,Msg_Type) ->
	http_utils:gen_result(false, 1,<<"Body is null">>,<<"">>);
send_muc_msg(Server,User,Room,_Host,Domain,Body,Extend_Info,Msg_Type) ->
	case is_suit_from(User) of
	true ->
		case Room of
		[{obj,[{"user",Muc}]}] ->
				case  jlib:make_jid(User,Server,<<"">>) of
				error ->
					http_utils:gen_result(false, 1,<<"Make from jid error">>,<<"">>);
				JFrom ->
					case jlib:make_jid(Muc,Domain,<<"">>) of 
					error ->
						http_utils:gen_result(false, 1,<<"Make Muc jid error">>,<<"">>);
					JTo ->
						Packet = ejabberd_public:make_message_packet(<<"groupchat">>,Body,Extend_Info,Msg_Type),
						ejabberd_router:route(JFrom,JTo,Packet),
						http_utils:gen_result(true, 0,<<"Send msg sucess">>,<<"">>)
					end
				end;
		_ ->
			http_utils:gen_result(false, 1,<<"To no suit">>,<<"">>)
		end;
	false ->	
		http_utils:gen_result(false, 1,<<"From no suit">>,<<"">>)
	end.

make_carbon_packet(To,Msg,Extend_Info,Msg_Type) ->
%    Bid = list_to_binary("wlan_" ++ uuid:to_string(uuid:random())),
    Bid = list_to_binary("http_" ++ integer_to_list(random:uniform(65536)) ++ integer_to_list(mod_time:get_exact_timestamp())),
    xml:to_xmlel(
            {xmlel  ,<<"message">>, [{<<"type">>,<<"chat">>},{<<"to">>,jlib:jid_to_string(To)},{<<"carbon_message">>,<<"true">>}],
	                [{xmlel,<<"body">>,[{<<"id">>,Bid},{<<"msgType">>,Msg_Type},{<<"extendInfo">>,Extend_Info}],[{xmlcdata, Msg}]}]}).
