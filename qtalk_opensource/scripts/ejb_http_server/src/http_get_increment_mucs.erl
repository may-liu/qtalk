%% Feel free to use, reuse and abuse the code in this file.

-module(http_get_increment_mucs).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("logger.hrl").
-include("ejb_http_server.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
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
			{<<"content-type">>, <<"text/plain; charset=utf-8">>}
		], <<"No GET method">>, Req).

post_echo(<<"POST">>, true, Req) ->
	{ok, Body, _} = cowboy_req:body(Req),
	case rfc4627:decode(Body) of
    {ok,{obj,Json},[]}  ->
		Rslt = 
		%	case http_utils:verify_user_key_pv1(Req) of
            case true of
        	true ->
                {User,_} = cowboy_req:qs_val(<<"u">>, Req),
				Ret = get_increment_mucs(User,Json),
				http_utils:gen_result(true, 0, <<"">>,Ret);
			_ ->
				http_utils:gen_result(false, 1, <<"Tkey check error">>,<<"">>)
			end,
		cowboy_req:reply(200, [{<<"content-type">>, <<"text/json; charset=utf-8">>}], Rslt, Req);
	_ ->
		Rslt = <<"Body parse error">>,
		cowboy_req:reply(200,[{<<"content-type">>, <<"text/json; charset=utf-8">>}], http_utils:gen_result(false, 2, <<"">>,Rslt), Req)
	end;
post_echo(<<"POST">>, false, Req) ->
	cowboy_req:reply(400, [], <<"Missing Post body.">>, Req);
post_echo(_, _, Req) ->
	cowboy_req:reply(405, Req).
										

echo(undefined, Req) ->
    cowboy_req:reply(400, [], <<"Missing parameter.">>, Req);
echo(Echo, Req) ->
    cowboy_req:reply(200, [
			        {<<"content-type">>, <<"text/plain; charset=utf-8">>}
	    			    ], Echo, Req).

terminate(_Reason, _Req, _State) ->
	ok.


get_increment_mucs(User,Args) -> 
    case proplists:get_value("u",Args) of
    User -> 
        T = proplists:get_value("t",Args,<<"0">>),
        case catch ejb_odbc_query:get_user_register_mucs_by_version(User,T) of
        {selected, _,Res} when is_list(Res) ->
                lists:flatmap(fun([M,D,V,F]) ->
                    [{obj,[{"M",M},{"D",D},{"T",V},{"F",F}]}] end,Res);
        _ ->
            []
        end;
    _ ->
        []
    end.
            

