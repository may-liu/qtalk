%% Feel free to use, reuse and abuse the code in this file.
-module(http_get_user_status).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("logger.hrl").
-include("ejb_http_server.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _ } = cowboy_req:method(Req),
    catch ejb_monitor:monitor_count(<<"http_get_user_status_domain">>,1),
	case Method of 
	<<"GET">> ->
	    {Host,_} =  cowboy_req:host(Req),
		{ok, Req1} = get_echo(Method,Host,Req),
		{ok, Req1, State};
	<<"POST">> ->
		HasBody = cowboy_req:has_body(Req),
		{ok, Req2} = post_echo(Method, HasBody, Req),
		{ok, Req2, State};
	_ ->
		{ok,Req3} = echo(undefined, Req),
		{ok, Req3, State}
	end.
    	
get_echo(<<"GET">>,_,Req) ->
		cowboy_req:reply(200, [
			{<<"content-type">>, <<"text/plain; charset=utf-8">>}
		], <<"No GET method">>, Req).

post_echo(<<"POST">>, true, Req) ->
	{ok, Body, _} = cowboy_req:body(Req),
	case rfc4627:decode(Body) of
	{ok,Json,[]}  ->
		Rslt =
		%	case http_utils:verify_user_key(Req) of
            case true of
			true ->
				get_user_status(Json);
			false ->
                {User,_} = cowboy_req:qs_val(<<"u">>, Req), 
                case iplimit_util:check_level_ip_limit(User,<<"12">>,Req) of
                true ->
                    get_user_status(Json);
                _ ->
    				http_utils:gen_result(false, 1, <<"Tkey check error">>,<<"">>)
                end
			end,
		cowboy_req:reply(200, [{<<"content-type">>, <<"text/json; charset=utf-8">>}], Rslt, Req);
	 _ ->
		Rslt = <<"Body parse error">>,
		cowboy_req:reply(200,[{<<"content-type">>, <<"text/json; charset=utf-8">>}],Rslt, Req)
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

get_user_status(Json) ->
	Ret = 
		try 
		lists:flatmap(fun({obj,Args}) ->
            Domain  = proplists:get_value("domain",Args),	
			Users = proplists:get_value("users",Args),
			case ejb_public:checek_domain(Domain) of
			true ->
				get_local_user_status(Domain,Users);
			_ ->
				http_get_user_status(Domain,Args)
			end end,Json)
		catch _:_ ->
			[]
		end,
	http_utils:gen_result(true, 0, <<"">>,Ret).

get_local_user_status(Domain,Users) ->
	DM = 
		case Domain of
		undefined ->
			ejb_public:get_host();
		_ ->
			Domain
		end,
	case DM of
	<<"">> ->
		[];
	_ ->
		do_get_local_user_status(DM,Users)
	end.

do_get_local_user_status(Domain,User) ->
	[{obj,[{"domain",Domain},{"ul",
		lists:flatmap(fun(U) ->
			case ejb_update_cache:get_qchat_status(U) of
			<<"offline">> ->
				[];
			S ->
				[{obj,[{"u",U},{"o",S}]}] 
			end end,User)}]}].			

http_get_user_status(Domain,Args) ->
	case catch ejb_public:get_url_by_domain(Domain) of
	[] ->
		[];
	U1 when is_list(U1) ->
		Url = lists:concat([U1,"/domain/get_user_status_domain"]),
		Body = rfc4627:encode([{obj,Args}]),
		Header = [],
		Type = "application/json",
		HTTPOptions = [],
		Options = [],
		case http_client:http_post(Url,Header,Type,Body,HTTPOptions,Options) of
		{ok, {_Status,_Headers, Rslt}} ->
			case catch rfc4627:decode(Rslt) of
			{ok,{obj,Data},[]} ->
				case proplists:get_value("data",Data) of
				undefined ->
					[];
				V ->
					V
				end;
			_ ->
				[]
			end;	
		_ ->
			[]
		end;
	_ ->
		[]
	end.

