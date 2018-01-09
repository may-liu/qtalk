%% Feel free to use, reuse and abuse the code in this file.

-module(http_sendall).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-export([http_send_message/2]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("http_req.hrl").

-record(department_users,{dep1,dep2,dep3,dep4,dep5,user}).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    {Url,_Req_t} = cowboy_req:url(Req),
   handle(Req, State, true).

handle(Req, State, false) ->
    Req_Res = Req#http_req{resp_compress = true},
    Res = http_utils:gen_result(false, <<"3">>, <<"ip is limited">>),
    {ok, NewReq} = cowboy_req:reply(200, [
                                    {<<"content-type">>, <<"text/plain; charset=utf-8">>}
                                   ], Res, Req_Res),
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
	{Online,_} = cowboy_req:qs_val(<<"online">>, Req),
	case rfc4627:decode(Body) of
	{ok,[{obj,Args}],[]} -> 
		Res = http_send_message(Args,Online),
		cowboy_req:reply(200, [{<<"content-type">>, <<"text/json; charset=utf-8">>}], Res, Req);
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

http_send_message(Json,Flag)->
    Servers = ejabberd_config:get_myhosts(),
    Server = lists:nth(1,Servers),
	http_send_message(Server,Json,Flag).
	
http_send_message(Server,Args,Online)->
	From = proplists:get_value("From",Args),
	Body  = proplists:get_value("Body",Args),
	Res = 
		case is_suit_from(From) of
		true ->
			case Body of
			undefined ->
				rfc4627:encode({obj,[{"data",<<"Message Body is Null">>}]});
			_ ->
				JFrom = jlib:make_jid(From,Server,<<"">>),
				case JFrom of 
				error ->
					rfc4627:encode({obj,[{"data",<<"From make jid error">>}]});
				_ ->
					http_send_notice(Online,Server,JFrom,Body) 
				end
			end;
		false ->	
    	    Us2 = {obj,[{"data",<<"From not suit">>}]},
    	    Us1 = rfc4627:encode(Us2)
		end,
      list_to_binary(Res).

is_suit_from(_From) ->
	true.

http_send_notice(<<"false">>,Server,JFrom,Body)  ->
	Packet = ejabberd_public:make_message_packet(<<"headline">>,Body,<<"1">>,<<"1">>),
	Users = 
		 case catch ets:select(department_users,[{#department_users{user = '$1', _ = '_'}, [], ['$1']}]) of
		 [] ->
		 	[];
		 UL when is_list(UL) ->
		 	UL;
		 _ ->
		 	[]
		 end,
	lists:foreach(fun(U) ->
			 case jlib:make_jid(U,Server,<<"">>) of
			 error ->
				 	ok;
			 JTo ->
				 	ejabberd_router:route(JFrom,JTo,Packet)
			end
		end,Users),
	rfc4627:encode({obj,[{"data",<<"Send Message Ok">>}]});
http_send_notice(_ ,Server,JFrom,Body) ->
	Packet = ejabberd_public:make_message_packet(<<"headline">>,Body,<<"1">>,<<"1">>),
	Uers = ejabberd_sm:get_vh_session_list(Server),
	lists:foreach(fun({U,S,R}) ->
		case jlib:make_jid(U,S,R) of
		error ->
			ok;
		JTo ->
			ejabberd_router:route(JFrom,JTo,Packet)
		end
	end ,Uers),
	rfc4627:encode({obj,[{"data",<<"Send Message Ok">>}]}).
