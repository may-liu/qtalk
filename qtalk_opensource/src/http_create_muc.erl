%% Feel free to use, reuse and abuse the code in this file.
-module(http_create_muc).
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-export([create_muc/1]).


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
		{ok, Req2} = post_echo(Method, HasBody, Req),
		{ok, Req2, State};
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
    Req1 = Req#http_req{resp_compress = true},
	case rfc4627:decode(Body) of
	{ok,{obj,Args},[]}  ->
		Res = create_muc(Args),
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

create_muc(Args) ->
	Servers = ejabberd_config:get_myhosts(),
	LServer = lists:nth(1,Servers),
	create_muc(LServer,Args).

create_muc(Server,Args) ->
	Muc_name =	 	http_muc_session:get_value("muc_name",Args,<<"">>),
	Muc_id = 		http_muc_session:get_value("muc_id",Args,<<"">>),
	Muc_owner = 	http_muc_session:get_value("muc_owner",Args,<<"">>),
	Domain =		http_muc_session:get_value("muc_domain",Args,<<"">>),
	Desc =			http_muc_session:get_value("muc_desc",Args,<<"">>),
    
    case http_muc_session:check_muc_exist(Server,Muc_id) of
    false ->
%	Packet = http_muc_session:make_muc_presence(),	
        Packet = http_muc_session:make_create_muc_iq(),
    	case jlib:make_jid(Muc_id,Domain,<<"">>) of
	    error ->
		    http_utils:gen_result(false, <<"-1">>,<<"">>,<<"error">>);
    	To_owner ->
%		Resources  = 
%			case ejabberd_sm:get_user_resources(Muc_owner, Server) of
%			[] ->
%				[<<"">>];
%			Rs ->
%				Rs
%			end,
 %       R = lists:nth(1,Resources),
	    	Owner = jlib:make_jid(Muc_owner,Server,<<"">>),
		    catch ejabberd_router:route(Owner,To_owner, Packet),
%		catch http_muc_session:update_user_presence_a(Server,Muc_owner,Muc_id,Domain),
%        IQ_Packet = http_muc_session:make_invite_iq(U),
%        catch ejabberd_router:route(Invite_Jid,Muc_jid,IQ_Packet)
    		catch odbc_queries:insert_muc_vcard_info(Server,str:concat(Muc_id,str:concat(<<"@">>,Domain)),Muc_name,Desc,<<"">>,<<"">>,<<"1">>),
%		http_muc_session:update_pg_muc_register(Server,Muc_id,[Muc_owner]),
	    	Persistent_packet = http_muc_session:make_muc_persistent(),
		    catch ejabberd_router:route(jlib:make_jid(Muc_owner,Server,<<"">>),jlib:jid_replace_resource(To_owner,<<"">>), Persistent_packet)	
%		case Resources of
%		[<<"">>] ->
%			catch http_muc_session:delete_unavailable_user(Server,Muc_id,Domain,Muc_owner);
%		_ ->
%			ok
%		end
    	end,
	    http_utils:gen_result(true, <<"0">>,<<"">>,<<"sucess">>);
    _ ->
        http_utils:gen_result(true, <<"3">>,<<"">>,<<"failed">>)
    end.


    
    
    
    
