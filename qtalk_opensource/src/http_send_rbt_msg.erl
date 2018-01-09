%% Feel free to use, reuse and abuse the code in this file.

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
	{ok,{obj,Args},[]} -> 
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
	Msg_type  = proplists:get_value("MsgType",Args),
	Chat_type  = proplists:get_value("Type",Args,<<"chat">>),
	Type = 
		case Msg_type of 
		undefined ->
			<<"1">>;
		_ ->
			Msg_type
		end,
	case is_suit_from(From)  of
	true ->
		case To of
		undefined ->
			http_utils:gen_result(false, <<"-1">>, <<"Json not find To">>);
		<<"subscription_users">> ->
			send_msg_to_all_subscription(Server,From,Body,Type);
		_ ->
            send_rbt_msg(Chat_type,Server,From,To,Body,Type,Args)  
	    end;
	false ->	
		http_utils:gen_result(false, <<"-1">>, <<"From not suit">>)
	end.

make_send_packet(To,Msg,Msg_type) ->
    make_send_packet(To,Msg,Msg_type,<<"subscription">>).

make_send_packet(To,Msg,Msg_type,Type) ->
%%	Integer = random:uniform(65536),
	Bid = list_to_binary("rbts_" ++  [jlib:integer_to_binary(X) || X <- tuple_to_list(os:timestamp())]),
	xml:to_xmlel(
			{xmlel	,<<"message">>,	[{<<"type">>,Type},{<<"to">>,jlib:jid_to_string(To)}],
				[{xmlel,<<"active">>,[{<<"xmlns">>,<<"http://jabber.org/protocol/chatstates">>}],[]},
					{xmlel,<<"body">>,[{<<"id">>,Bid},{<<"msgType">>,Msg_type}],[{xmlcdata, Msg}]}]}).

is_suit_from(_From) ->
	true.	

send_msg_to_all_subscription(Server,From,Body,Msg_type) ->
	case catch ets:select(user_rbts,[{#user_rbts{rbt = From,name = '$1', _ = '_'},[], ['$1']}]) of
	Users when is_list(Users) ->
		lists:foreach(fun(U) ->
			sendMsg(Server,Server,From,U,Body,Msg_type,<<"subscription">>) end,Users),
		http_utils:gen_result(true, <<"0">>, <<"Send message Ok">>);
	_ ->
		http_utils:gen_result(false, <<"-1">>, <<"Send message failed">>)
	end.

sendMsg(Server,Domain,From,To,Body,Msg_type,Type) ->
	case Body of
	undefined ->
			[str:concat(To,<<"Message Body is Null">>)];
	_ ->
			JFrom = jlib:make_jid(From,Server,<<"">>),
			case JFrom of 
			error ->
				[str:concat(From,<<"From make jid error">>)];
			_ ->
				case jlib:make_jid(To,Domain,<<"">>) of 
				error ->
					[str:concat(To,<<"To make jid error">>)];
				JTo ->
					Packet = make_send_packet(JTo,Body,Msg_type,Type),
					ejabberd_router:route(JFrom,JTo,Packet),
					[]
				end
			end
	end.

check_legal_touser(Server,Rbt,JTo) ->
	case catch ets:select(user_rbts,[{#user_rbts{name = JTo,rbt = Rbt, _ = '_'},[], [[]]}]) of
	[] ->
		case catch ejabberd_odbc:sql_query(Server,
			[<<"select user_name from robot_pubsub where rbt_name = '">>,Rbt,<<"' and user_name = '">>,JTo,<<"';">>]) of
		{selected,[<<"user_name">>],[]} ->
			false;
		{selected, [<<"user_name">>], SRes1} when is_list(SRes1) ->
			ets:insert(user_rbts,#user_rbts{name = Rbt,rbt = JTo}),
			true;
		_ ->
			false
		end;
	[[]] ->
		true
	end.

send_rbt_msg(<<"chat">>,Server,From,To,Body,Type,Args) ->
    Tos =  str:tokens(To,<<",">>),
	Rslt = 
	    lists:flatmap(fun(T) ->
	        sendMsg(Server,Server,From,T,Body,Type,<<"subscription">>) end,Tos),
	case Rslt of
	[] ->
		http_utils:gen_result(true,<<"0">>,Rslt);
	_ ->
	    http_utils:gen_result(false,<<"-1">>,Rslt)
    end;
send_rbt_msg(<<"groupchat">>,Server,From,To,Body,Type,Args)  ->
    case proplists:get_value("Domain",Args) of
    undefined ->
        http_utils:gen_result(false,<<"-1">>,[]);
    Domain ->
        %% conference.ejabhost1
        Rslt = sendMsg(Server,<<"conference.ejabhost1">>,From,To,Body,Type,<<"groupchat">>),
        case Rslt of
        [] ->
            http_utils:gen_result(true,<<"0">>,Rslt);
        _ ->
            http_utils:gen_result(false,<<"-1">>,Rslt)
        end
    end;
send_rbt_msg(_,Server,From,To,Body,Type,Args) ->
     http_utils:gen_result(true,<<"0">>,[]).
