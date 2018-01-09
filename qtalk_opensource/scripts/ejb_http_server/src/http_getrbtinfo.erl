%% Feel free to use, reuse and abuse the code in this file.

-module(http_getrbtinfo).
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("ejb_http_server.hrl").
-include("logger.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _ } = cowboy_req:method(Req),
    catch ejb_monitor:monitor_count(<<"http_get_rbt_info">>,1),
	case Method of 
	<<"GET">> ->
		{ok, Req1} = echo(<<"No Get Method!">>,Req),
		{ok, Req1, State};
	<<"POST">> ->
		HasBody = cowboy_req:has_body(Req),
		{ok, Req1} = post_echo(Method, HasBody, Req),
		{ok, Req1, State};
	_ ->
		{ok,Req1} = echo(undefined, Req),
		{ok, Req1, State}
	end.
post_echo(<<"POST">>,true,Req) ->	
	{ok, PBody, _} = cowboy_req:body(Req),
	Header = cowboy_req:get(headers,Req),
	{Type,_ } = cowboy_req:qs_val(<<"type">>, Req),
	Body = 
		case catch proplists:get_value(<<"content-encoding">>,Header) of 
		<<"gzip">> ->
			zlib:gunzip(PBody);
		_ ->
			PBody
		end,	
	LServer = <<"ejb_http_server">>,
	case rfc4627:decode(Body) of
	{ok,Json,[]}  ->
		Rslt =
			case Type of 
			<<"Php">> ->
				get_rbt_info_for_php(LServer,Json);
			_ ->
				get_rbt_info(Json)
			end,
		cowboy_req:reply(200, [{<<"content-type">>, <<"text/json; charset=utf-8">>}], Rslt, Req);
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

get_rbt_info(Json) ->
	Res = 
		lists:flatmap(fun({obj,Args}) ->
			Rbt_name 	= proplists:get_value("robot_name",Args),
			Rbt_version	= proplists:get_value("version",Args),
			case catch ets:lookup(rbt_info,Rbt_name) of
			[] ->
			%%	[http_utils:gen_result(false, <<"-1">>,<<"No found Regist Robot">>)];
			%%	null;
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
						[]
					end;
				false ->
					[]
				end
			end end,Json),
	case Res of 
	[] ->
		http_utils:gen_result(true, <<"1">>, <<"">>,<<"">>);
	_ ->
		http_utils:gen_result(true, <<"0">>, <<"">>,Res)
	end.
		
get_rbt_info_for_php(LServer,{obj,Args}) ->
	Rbt_name 	= proplists:get_value("robot_name",Args),
	Password	= proplists:get_value("password",Args),
    case catch pg_odbc:sql_query(LServer,
		[<<"select password from robot_info where en_name = '">>,Rbt_name,<<"';">>]) of
	{selected, [<<"password">>], [[Password1]]} when Password1 =:= Password orelse Password1 =:= null ->
		Res =
			case catch ets:lookup(rbt_info,Rbt_name) of
			[] ->
				<<"">>;
			[Rbt] when is_record(Rbt,rbt_info) ->
				case rfc4627:decode(Rbt#rbt_info.body) of
				{ok,Body,[]} ->
					{obj,[{"rbt_name",Rbt_name},{"rbt_body",Body}]};
				_ ->
					<<"">>
				end
			end,
		case Res of 
		<<"">> ->
			http_utils:gen_result(true, <<"1">>,<<"">>,<<"">>);
		_ ->
			http_utils:gen_result(true, <<"0">>,<<"">>,Res)
		end;
	_ ->
		http_utils:gen_result(false, <<"-1">>,<<"Password error">>,<<"">>)
	end;
get_rbt_info_for_php(_,_ ) ->
	http_utils:gen_result(false, <<"-1">>,<<"Json format error.">>,<<"">>).

