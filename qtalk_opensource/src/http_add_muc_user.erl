%% Feel free to use, reuse and abuse the code in this file.

-module(http_add_muc_user).
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-export([add_muc_users/1]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("http_req.hrl").
-include("jlib.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _ } = cowboy_req:method(Req),
	case Method of 
	<<"GET">> ->
		{ok, Req1} = echo(<<"No Get Method!">>,Req),
		{ok, Req1, State};
	<<"POST">> ->
		HasBody = cowboy_req:has_body(Req),
		{ok, Req1} = post_echo(Method, HasBody, Req),
		{ok, Req1, State};
	_ ->
		{ok,Req3} = echo(undefined, Req),
		{ok, Req3, State}
	end.
post_echo(<<"POST">>,true,Req) ->	
	{ok, PBody, _} = cowboy_req:body(Req),
	Header = cowboy_req:get(headers,Req),
	Body = 
		case catch proplists:get_value(<<"content-encoding">>,Header) of 
		<<"gzip">> ->
			zlib:gunzip(PBody);
		_ ->
			PBody
		end,	
	case rfc4627:decode(Body) of
	{ok,{obj,Args},[]}  ->
		Res = add_muc_users(Args),
		cowboy_req:reply(200, [{<<"content-type">>, <<"text/json; charset=utf-8">>}], Res, Req);
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
add_muc_users(Args) ->
	Servers = ejabberd_config:get_myhosts(),
	LServer = lists:nth(1,Servers),
	add_muc_users(LServer,Args).

add_muc_users(Server,Args) ->
	Muc_id = 		http_muc_session:get_value("muc_id",Args,<<"">>),
    Muc_owner =     http_muc_session:get_value("muc_owner",Args,<<"">>),
	Muc_member =	http_muc_session:get_value("muc_member",Args,<<"">>),
	Domain =		http_muc_session:get_value("muc_domain",Args,<<"">>),
    case http_muc_session:check_muc_exist(Server,Muc_id) of
    true ->
    	Packet = http_muc_session:make_muc_presence(),	
	    Muc_jid = jlib:make_jid(Muc_id,Domain,<<"">>),
    	Invite_Jid = jlib:make_jid(Muc_owner,Server,<<"">>),
	%lists:foreach(fun(U) ->
	%	Jid = jlib:make_jid(U,Server,<<"">>),
%		case ejabberd_sm:get_user_resources(U, Server) of
%		[] ->
%			http_muc_session:handle_add_muc_users(Server,Muc_id,Domain,Jid);
%		Rs ->
%			lists:foreach(fun(R) ->
 	%	case jlib:make_jid(U,Server,<<"">>) of
 	%	error ->
 	%		ok;
    %	Muc_user ->
        %    N = ejabberd_public:get_user_nick(U),
		%	catch ejabberd_router:route(Muc_user, jlib:make_jid(Muc_id,Domain,N), Packet),
        case Muc_member of 
        <<"">> ->
            ok;
        _ -> 
            IQ_Packet = http_muc_session:make_invite_iq(Muc_member,<<"ejabhost1">>),
             ?DEBUG("From ~p ,To ~p,Packet ~p ~n",[Invite_Jid,Muc_jid,IQ_Packet]),
            catch ejabberd_router:route(Invite_Jid,Muc_jid,IQ_Packet)
        end,
%				catch http_muc_session:update_user_presence_a(Server,U,R,Muc_id,Domain)
	%	end
%%			catch ejabberd_router:route(Invite_Jid,Muc_jid,http_muc_session:make_invite_iq(jlib:jid_to_string(Jid)))	
%		http_muc_session:handle_add_muc_users(Server,Muc_id,Domain,Jid)
	%	end,Muc_member),
	    http_utils:gen_result(true, <<"0">>,<<"">>,<<"sucess">>);
    _ ->
	    http_utils:gen_result(true, <<"1">>,<<"">>,<<"failed">>)
    end.
        

