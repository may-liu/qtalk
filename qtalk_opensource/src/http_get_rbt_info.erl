%% Feel free to use, reuse and abuse the code in this file.
%%========================================================
%%获取机器人信息接口
%%========================================================

-module(http_get_rbt_info).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("ejabberd_extend.hrl").


init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _ } = cowboy_req:method(Req),
	case Method of 
	<<"GET">> ->
		{ok, Req2} = echo(<<"No Get Method!">>,Req),
		{ok, Req2, State};
	<<"POST">> ->
		HasBody = cowboy_req:has_body(Req),
		{ok, Req2} = post_echo(Method, HasBody, Req),
		{ok, Req2, State};
	_ ->
		{ok,Req2} = echo(undefined, Req),
		{ok, Req2, State}
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
	case catch rfc4627:decode(Body) of
	{ok,Json,[]}  ->
		Res = get_rbt_info(Json),
		cowboy_req:reply(200, [{<<"content-type">>, <<"text/json; charset=utf-8">>}], Res, Req);
	_ ->
		cowboy_req:reply(200, [{<<"content-type">>, <<"text/json; charset=utf-8">>}], 
					http_utils:gen_result(false, 1,<<"Json format error.">>), Req)
	end;
post_echo(<<"POST">>, false, Req) ->
	cowboy_req:reply(400, [], <<"Missing Post body.">>, Req);
post_echo(_, _, Req) ->
	cowboy_req:reply(405, Req).
										

echo(undefined, Req) ->
	cowboy_req:reply(400, [], <<"Missing Post body.">>, Req);
echo(Echo, Req) ->
    cowboy_req:reply(200, [
			        {<<"content-type">>, <<"text/plain; charset=utf-8">>}
	    			    ], http_utils:gen_result(true, 0,Echo), Req).

terminate(_Reason, _Req, _State) ->
	ok.

get_rbt_info(Json) ->
	Res = 
		lists:flatmap(fun({obj,Args}) ->
			Rbt_name 	= proplists:get_value("robot_name",Args),
			Rbt_version	= proplists:get_value("version",Args),
			case catch ets:lookup(rbt_info,Rbt_name) of
			[] ->
			%%	[http_utils:gen_result(false, <<"-1">>,<<"No found Regist Robot">>)];
				[];
			[Rbt] ->
				Ev = http_utils:to_integer(Rbt#rbt_info.version),
				Cv = http_utils:to_integer(Rbt_version),
				case Cv < Ev of
				true ->
					case rfc4627:decode(Rbt#rbt_info.body) of
					{ok,Body,[]} ->
						[{obj,[{"rbt_name",Rbt_name},{"rbt_body",Body},{"rbt_ver",Ev}]}];
					_ ->
						?DEBUG("Boyd jsos error ~p ~n",[Rbt#rbt_info.body]),
						[]
					end;
				false ->
					[]
				end
			end end,Json),
	case Res of 
	[] ->
		http_utils:gen_result(true, 0, <<"">>);
	_ ->
		http_utils:gen_result(true, 0, Res)
	end.
		
