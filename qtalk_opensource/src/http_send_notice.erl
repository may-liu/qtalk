%% Feel free to use, reuse and abuse the code in this file.
%%=========================================================
%%发送notice（headline)类型消息
%%=========================================================

-module(http_send_notice).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("http_req.hrl").
-include("ejabberd_extend.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
%%    handle(Req, State, iplimit_util:check_ip(Req)).
	handle(Req, State,true).

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
    {ok, Body,_} = cowboy_req:body(Req),
	{Online,_} = cowboy_req:qs_val(<<"online">>, Req),
    Servers = ejabberd_config:get_myhosts(),
    Server = lists:nth(1,Servers),
	case catch rfc4627:decode(Body) of
	{ok,{obj,Args},[]} -> 
       	Res = http_send_message(Server,Args,Online),
		cowboy_req:reply(200, [{<<"content-type">>, <<"text/json; charset=utf-8">>}],  
				http_utils:gen_result(true,0,Res), Req);
	_ ->
		 cowboy_req:reply(200, [{<<"content-type">>, <<"text/json; charset=utf-8">>}], 
				 http_utils:gen_result(false, 2, <<"Not found Mac_Key">>), Req)
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

http_send_message(Server,Args,Online)->
	From = proplists:get_value("From",Args),
	Body  = proplists:get_value("Body",Args),
	case is_suit_from(From) of
	true ->
		case Body of
		undefined ->
			{obj,[{"data",<<"Message Body is Null">>}]};
		_ ->
			case jlib:make_jid(From,Server,<<"">>) of
			error ->
				[{obj,[{"data",<<"From make jid error">>}]}];
			JFrom ->
				http_send_notice(Online,Server,JFrom,Body)
			end
		end;
	false ->	
   	    {obj,[{"data",<<"From not suit">>}]}
	end.

is_suit_from(_From) ->
	true.	


http_send_notice(<<"false">>,Server,JFrom,Body)  ->
	Packet = ejabberd_public:make_send_packet(<<"headline">>,Body),
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
	{obj,[{"data",<<"Send Message Ok">>}]};
http_send_notice(_ ,Server,JFrom,Body) ->
    Packet = ejabberd_public:make_send_packet(<<"headline">>,Body),
	Uers = ejabberd_sm:get_vh_session_list(Server),
	lists:foreach(fun({U,S,R}) ->
		case jlib:make_jid(U,S,R) of
		error ->
			ok;
		JTo ->
			ejabberd_router:route(JFrom,JTo,Packet)
		end
		end ,Uers),
	{obj,[{"data",<<"Send Message Ok">>}]}.

