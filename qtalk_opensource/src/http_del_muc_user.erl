%% Feel free to use, reuse and abuse the code in this file.

-module(http_del_muc_user).
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-export([del_muc_users/2]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("http_req.hrl").
-include("jlib.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    {Url,_Req_t} = cowboy_req:url(Req),
	{Method, Req2} = cowboy_req:method(Req),
	case Method of 
	<<"GET">> ->
	    {Host,Req3} =  cowboy_req:host(Req),
		{ok, Req4} = echo(<<"No Get Method!">>,Req),
		{ok, Req4, State};
	<<"POST">> ->
		HasBody = cowboy_req:has_body(Req),
		{ok, Req3} = post_echo(Method, HasBody, Req),
		{ok, Req3, State};
	_ ->
		{ok,Req3} = echo(undefined, Req),
		{ok, Req3, State}
	end.
post_echo(<<"POST">>,true,Req) ->	
	{ok, PBody, _} = cowboy_req:body(Req),
    {Host,_} =  cowboy_req:host(Req),
	Header = cowboy_req:get(headers,Req),
	{Type,_ } = cowboy_req:qs_val(<<"type">>, Req),
	Body = 
		case catch proplists:get_value(<<"content-encoding">>,Header) of 
		<<"gzip">> ->
			zlib:gunzip(PBody);
		_ ->
			PBody
		end,	
    Req1 = Req#http_req{resp_compress = true},
	case rfc4627:decode(Body) of
	{ok,{obj,Args},[]}  ->
		Res = del_muc_users(Type,Args),
		cowboy_req:reply(200, [{<<"content-type">>, <<"text/json; charset=utf-8">>}], Res, Req1);
	_ ->
		cowboy_req:reply(200, [{<<"content-type">>, <<"text/json; charset=utf-8">>}], 
					http_utils:gen_result(false, <<"-1">>,<<"Json format error.">>,<<"">>), Req)
	end;
post_echo(<<"POST">>, false, Req) ->
	cowboy_req:reply(400, [], http_utils:gen_result(false, <<"-1">>,<<"Missing Post body.">>,<<"">>), Req);
post_echo(_, _, Req) ->
	cowboy_req:reply(405, Req).
										

echo(undefined, Req) ->
	cowboy_req:reply(400, [], http_utils:gen_result(false, <<"-1">>,<<"Missing Post body.">>,<<"">>), Req);
echo(Echo, Req) ->
    cowboy_req:reply(200, [
			        {<<"content-type">>, <<"text/plain; charset=utf-8">>}
	    			    ], http_utils:gen_result(true, <<"0">>,Echo,<<"">>), Req).

terminate(_Reason, _Req, _State) ->
	ok.

del_muc_users(Type,Args) ->
	Servers = ejabberd_config:get_myhosts(),
	LServer = lists:nth(1,Servers),
	case Type of 
   	<<"1">>	 ->
		do_del_muc_users(LServer,Args);
	_ ->
		do_del_muc_users(LServer,Args)
	end.

do_del_muc_users(Server,Args) ->
	Muc_id = 		http_muc_session:get_value("muc_id",Args,<<"">>),
	Muc_user =		http_muc_session:get_value("muc_owner",Args,<<"">>),
	Muc_member =	http_muc_session:get_value("muc_member",Args,<<"">>),
	Domain =		http_muc_session:get_value("muc_domain",Args,<<"">>),
        case jlib:make_jid(Muc_id,Domain,<<"">>) of
	   error ->
		    ok;
    	To ->
	%%	lists:foreach(fun(U) ->
		%%	Packet = http_muc_session:kick_muc_user(Server,U),
		%%	?DEBUG("Packet ~p ~n",[Packet]),
		%%	catch ejabberd_router:route(jlib:make_jid(Muc_user,Server,<<"">>),To, Packet),
                Packet = http_muc_session:make_del_register_muc_iq(),
                lists:foreach(fun(U) ->
                    From = jlib:make_jid(U,Server,<<"">>),
                    catch ejabberd_router:route(From,To,Packet) end ,Muc_member)
%%			catch http_muc_session:remove_muc_users(Server,Muc_member,Muc_id,Domain) 
	%%		catch http_muc_session:remove_presence_a(Server,U,Muc_id,Domain) 
	%%	   end,Muc_member)
    	end,
	    http_utils:gen_result(true, <<"0">>,<<"">>,<<"sucess">>).
        

