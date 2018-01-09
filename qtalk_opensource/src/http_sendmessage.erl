%% Feel free to use, reuse and abuse the code in this file.

-module(http_sendmessage).

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
    handle(Req, State, true).

handle(Req, State, false) ->
    Req_Res = Req#http_req{resp_compress = true},
    Res = http_utils:gen_result(false, <<"3">>, <<"ip is limited">>),
    {ok, NewReq} = cowboy_req:reply(200, [
                                    {<<"content-type">>, <<"text/plain; charset=utf-8">>}
                                   ], Res, Req_Res),
    {ok, NewReq, State};
handle(Req, State, _) ->
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
	{ok, [{obj,Args}],[]} -> 
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
    ?DEBUG("Args ~p ~n",[Args]),
	From = proplists:get_value("From",Args),
	To = proplists:get_value("To",Args),
	Body  = proplists:get_value("Body",Args),
	Type  = proplists:get_value("Type",Args,<<"chat">>),
	Msg_Type  = proplists:get_value("Msg_Type",Args),
	Host = proplists:get_value("Host",Args,Server),
	Domain  = proplists:get_value("Domain",Args),
	Extend_Info  =	proplists:get_value("Extend_Info",Args,<<"">>),
	Carbon  =	proplists:get_value("Carbon",Args,<<"false">>),
	Res = 
		case Type of 
		<<"groupchat">> ->
			send_muc_msg(Server,From,To,Host,Domain,Body,Extend_Info,Msg_Type);
		_ ->
			send_chat_msg(Server,From,To,Host,Body,Msg_Type,Extend_Info,Type,Carbon)
		end,
    list_to_binary(Res).

send_chat_msg(Server,From,To,Host,Body,Msg_Type,Extend_Info,Type,Carbon) ->
	case is_suit_from(From) of
	true ->
		case To of
		undefined ->
			rfc4627:encode({obj,[{"data",<<"To not suit">>}]});
		[{obj,[{"User",<<"All">>}]}]  -> 
			Users = ets:tab2list(userlist),
			JFrom = jlib:make_jid(From,Server,<<"">>),
			case JFrom of error ->
				rfc4627:encode({obj,[{"data",<<"Send Message failed">>}]});
			_ ->
				lists:foreach(fun({U}) ->
						JTo = jlib:make_jid(U,Host,<<"">>),
						case JTo of
						error ->
							 ok;
						_ ->
							Packet = ejabberd_public:make_message_packet(Type,Body,Extend_Info,Msg_Type),
						 ejabberd_router:route(JFrom,JTo,Packet)
						end
					 end ,Users),
				rfc4627:encode({obj,[{"data",<<"Send Message Ok">>}]})
			end;
		_ ->
			case Body of
			undefined ->
				rfc4627:encode({obj,[{"data",<<"Message Body is Null">>}]});
			_ ->
				JFrom = jlib:make_jid(From,Server,<<"">>),
				case JFrom of 
				error ->
					rfc4627:encode({obj,[{"data",<<"From make jid error">>}]});
				_ ->
					SendRes =
						lists:flatmap(fun({obj,[{"User",ToU}]}) ->
							case jlib:make_jid(ToU,Host,<<"">>) of 
							error ->
								[{"failed",ToU}];
							JTo ->
								Packet = ejabberd_public:make_message_packet(Type,Body,Extend_Info,Msg_Type),
								ejabberd_router:route(JFrom,JTo,Packet),
                                route_carbon_msg(Carbon,Msg_Type,JFrom,JTo,Type,Body,Extend_Info),
								[]
							end	end,To),
					case length(SendRes) of 
					0 ->
						rfc4627:encode({obj,[{"data",<<"Send Message Ok">>}]});
					_ ->
						rfc4627:encode({obj,SendRes})
					end
				end
			end
		end;
	false ->	
   	    Us2 = {obj,[{"data",<<"From not suit">>}]},
   	    rfc4627:encode(Us2)
	end.

route_carbon_msg(<<"true">>,Msg_Type,JFrom,JTo,Type,Body,Extend_Info) ->
    Packet = make_carbon_packet(JFrom,Body,Extend_Info,Msg_Type,Type),
	ejabberd_router:route(JTo,JFrom,Packet);
route_carbon_msg(_,Msg_Type,JFrom,JTo,Type,Body,Extend_Info) ->
	if Msg_Type =:= <<"512">> orelse Msg_Type =:= <<"513">> orelse Msg_Type =:= <<"666">> ->
		Packet = make_carbon_packet(JFrom,Body,Extend_Info,Msg_Type,Type),
		ejabberd_router:route(JTo,JFrom,Packet);
    true ->
        ok
    end.

make_carbon_packet(To,Msg,Extend_Info,Msg_Type,Type) ->
   % pBid =     list_to_binary("http_" ++ uuid:to_string(uuid:random())),
        Bid = list_to_binary("http_" ++ integer_to_list(random:uniform(65536)) ++ integer_to_list(mod_time:get_exact_timestamp())),
	xml:to_xmlel(
			{xmlel	,<<"message">>,	[{<<"type">>,Type},{<<"to">>,jlib:jid_to_string(To)},{<<"carbon_message">>,<<"true">>}],
				[{xmlel,<<"body">>,[{<<"id">>,Bid},{<<"msgType">>,Msg_Type},{<<"extendInfo">>,Extend_Info}],[{xmlcdata, Msg}]}]}).
	

is_suit_from(_From) ->
	true.	

send_muc_msg(Server,User,Room,_Host,Domain,Body,Extend_Info,Msg_Type) ->
	case is_suit_from(User) of
	true ->
		case Room of
		[{obj,[{"User",Muc}]}] ->
			case Body of
			undefined ->
				rfc4627:encode({obj,[{"data",<<"Message Body is Null">>}]});
			_ ->
		%		case ejabberd_public:get_user_room_rescource(str:concat(<<"conference.">>,Server),User,Server,Muc,Domain) of
		%		[] ->
		%			rfc4627:encode({obj,[{"data",<<"User not in Room">>}]});
		%		Rescoures when is_list(Rescoures)->
		%			Rs = hd(Rescoures),
		%			case  jlib:make_jid(User,Server,Rs) of
				case catch ejabberd_public:check_user_reg_muc(Server,Muc,User) of
				true ->
					case  jlib:make_jid(User,Server,<<"">>) of
					error ->
						rfc4627:encode({obj,[{"data",<<"From make jid error">>}]});
					JFrom ->
						case jlib:make_jid(Muc,Domain,<<"">>) of 
						error ->
							 rfc4627:encode({obj,[{"data",<<"Muc make jid error">>}]});
						JTo ->
							Packet = ejabberd_public:make_message_packet(<<"groupchat">>,Body,Extend_Info,Msg_Type),
							ejabberd_router:route(JFrom,JTo,Packet),
							rfc4627:encode({obj,[{"data",<<"Send Message Ok">>}]})
						end
					end;
				_ ->
					rfc4627:encode({obj,[{"data",<<"User not in Room">>}]})
				end
			end;
		_ ->
			rfc4627:encode({obj,[{"data",<<"To not suit">>}]})
		end;
	false ->	
   	    Us2 = {obj,[{"data",<<"From not suit">>}]},
   	    rfc4627:encode(Us2)
	end.
