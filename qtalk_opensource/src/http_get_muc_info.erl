%% Feel free to use, reuse and abuse the code in this file.
-module(http_get_muc_info).
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-export([get_muc_info/1]).


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
		Res = get_muc_info(Args),
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

get_muc_info(Args) ->
	Servers = ejabberd_config:get_myhosts(),
	LServer = lists:nth(1,Servers),
	get_muc_info(LServer,Args).

get_muc_info(LServer,Args) ->
	Muc_id = 		http_muc_session:get_value("muc_id",Args,<<"">>),
	Server_Host =		http_muc_session:get_value("muc_domain",Args,<<"">>),
	SName = ejabberd_odbc:escape(Muc_id),
	SHost = ejabberd_odbc:escape(Server_Host),
	Muc_Users = 
		case catch odbc_queries:get_muc_users(LServer,<<"muc_room_users">>,SName) of
		{selected, _, Res} when is_list(Res) ->
			lists:map(fun([_M,U,_H]) ->
					U end,Res);
		_ ->
			[]
		end,
	Owners = 
	case catch ejabberd_odbc:sql_query(LServer,[<<"select opts from muc_room where name='">>,SName, <<"' and host='">>, SHost,<<"';">>]) of
	{selected, [<<"opts">>], [[Opts]]} ->
		BOpts = mod_muc:opts_to_binary(ejabberd_odbc:decode_term(Opts)),
		Users = proplists:get_value(affiliations,BOpts),
		lists:flatmap(fun({{U,S,R},{Aff,J}}) ->
			if Aff =:= owner  ->
				[U];
			true ->
				[]
			end end,Users);
	_ ->
		[]
	end,
	http_utils:gen_result(true, <<"0">>,<<"">>, {obj, [{"Muc_Users", Muc_Users}, {"Owners",Owners}]}).
