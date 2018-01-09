%% Feel free to use, reuse and abuse the code in this file.
-module(http_destroy_muc).
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-export([destroy_muc/1]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("http_req.hrl").
-include("jlib.hrl").

-record(muc_online_room, {name_host = {<<"">>, <<"">>} :: {binary(), binary()} | '$1' | {'_', binary()} | '_',
		                            pid = self() :: pid() | '$2' | '_' | '$1'}).

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
		Res = destroy_muc(Args),
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

destroy_muc(Args) ->
	Servers = ejabberd_config:get_myhosts(),
	LServer = lists:nth(1,Servers),
	destroy_muc(LServer,Args).

destroy_muc(LServer,Args) ->
	Room = 		http_muc_session:get_value("muc_id",Args,<<"">>),
	Server_Host =		http_muc_session:get_value("muc_domain",Args,<<"">>),
	Muc_Owner =		http_muc_session:get_value("muc_owner",Args,<<"">>),
    Host = http_muc_session:get_value("host",Args,<<"">>),
    Owner = jlib:jid_to_string({Muc_Owner,Host,<<"">>}),
    
    case mod_muc:check_muc_owner(Host,Room,Owner) of
    true ->
        case mnesia:dirty_read(muc_online_room, {Room,Server_Host}) of
        [] ->
	    	mod_muc:forget_room(LServer,Server_Host ,Room),
	    	catch odbc_queries:restore_muc_user_mark(LServer,Room),
    		catch odbc_queries:del_muc_users(LServer,<<"muc_room_users">>,Room),
	    	catch odbc_queries:del_user_register_mucs(LServer,Room),
	    	catch odbc_queries:del_muc_vcard_info(LServer,Room,<<"Admin Destroy">>);
        [M] ->
            ?INFO_MSG("Destory Room ~s  by management cmd ~n",[Room]),
	    	Pid = M#muc_online_room.pid,
	    	gen_fsm:send_all_state_event(Pid, {destroy, <<"management close">>}),
    		mod_muc:room_destroyed(Server_Host, Room,Pid, LServer),
    		mod_muc:forget_room(LServer,Server_Host ,Room),
    		catch odbc_queries:restore_muc_user_mark(LServer,Room),
	    	catch odbc_queries:del_muc_users(LServer,<<"muc_room_users">>,Room),
	    	catch odbc_queries:del_user_register_mucs(LServer,Room),
	    	catch odbc_queries:del_muc_vcard_info(LServer,Room,<<"Admin Destroy">>)
    	end,
	    http_utils:gen_result(true, <<"0">>,<<"">>,<<"sucess">>);
    _ ->
        http_utils:gen_result(true, <<"1">>,<<"">>,<<"failed">>)
    end.


