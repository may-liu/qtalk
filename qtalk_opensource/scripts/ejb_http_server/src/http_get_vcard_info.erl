%% Feel free to use, reuse and abuse the code in this file.

-module(http_get_vcard_info).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("logger.hrl").
-include("ejb_http_server.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _} = cowboy_req:method(Req),
    catch ejb_monitor:monitor_count(<<"http_get_muc_msg_domain">>,1),
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
	{ok,Json,[]}  ->
		Rslt = 
			%%case http_utils:verify_user_key(Req) of
			case http_utils:verify_user_key_pv1(Req) of
        	true ->
				Ret = get_update_version(Json),
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

get_update_version(Json)->
	lists:flatmap(fun({obj,Args}) ->
		Domain = proplists:get_value("domain",Args),
		Users = proplists:get_value("users",Args),
		case ejb_public:checek_domain(Domain) of
		true ->
			UV = get_user_vcard_info(Domain,Users),
			[{obj, [{"domain",Domain},{"users",UV}]}];
		_ ->
			http_get_vcard_info(Domain,Users)
		end end,Json).

get_users_by_domain(Json) ->
	lists:foldl(fun({obj,Args},Acc) ->
		case  proplists:get_value("domain",Args) of
		undefined ->
			sort_by_domain(undefined,Args,Acc);
		Key ->
			sort_by_domain(Key,Args,Acc)
		end end,[],Json).
				
sort_by_domain(Key,Args,Acc) ->
	case proplists:get_value(Key,Acc) of
	undefined ->
		Acc ++ [{Key,[{obj,Args}]}];
	V ->
		L1 = proplists:delete(Key,Acc),
		L1 ++ [{Key,V ++ [{obj,Args}]}]
	end.

get_user_vcard_info(Domain,Users) ->
	lists:flatmap(fun({obj,Args}) ->
    	User  = proplists:get_value("user",Args),
    	Vs  = proplists:get_value("version",Args),	
		case catch ets:lookup(vcard_version,User) of
		[] ->
			%%编外人员
		%%	[{obj, [{"type",<<"qunar_emp">>},{"U",User},{"N",<<"">>},{"V",<<"1">>},{"Url",<<"">>},{"commenturl",<<"">>}]}];
			[];
		[Vv] when is_record(Vv,vcard_version)  ->
			IVs = http_utils:to_integer(Vs),
			IVv = http_utils:to_integer(Vv#vcard_version.version),
			if IVs < IVv -> 
				case Vv#vcard_version.url of
				null ->
					[{obj, [{"type",<<"qunar_emp">>},
						{"loginName",User},{"email",<<"">>},{"gender",Vv#vcard_version.gender},
						{"nickname",ejb_public:get_value_with_default(Vv#vcard_version.name,User)},
							{"V",Vv#vcard_version.version},
						{"imageurl",<<"file/xxxx/8a54.png">>},
							{"uid",0},{"username",User},{"commenturl",<<"https://xxxxxxxxxxx/dianping/user_comment.php">>}]}];
				_ ->
					[{obj, [{"type",<<"qunar_emp">>},
						{"loginName",User},{"email",<<"">>},{"gender",Vv#vcard_version.gender},
							{"nickname",ejb_public:get_value_with_default(Vv#vcard_version.name,User)},
							{"V",Vv#vcard_version.version},
						{"imageurl",complete_url(Vv#vcard_version.url)},{"uid",0},{"username",User},
						{"commenturl",<<"https://xxxxxxxxx/dianping/user_comment.php">>}]}]
				end;
			true ->
				[]
			end;
		_ ->
			[]
		end end,Users).

complete_url(Url) ->
	case catch str:left(Url,4) of
	<<"http">> ->
		Url;
	_ ->
		Url
	end.

http_get_vcard_info(Domain,Args) ->
	case catch ejb_public:get_url_by_domain(Domain) of
	[] ->
		[];
	U1 when is_list(U1) ->
		Url = lists:concat([U1,"/domain/get_vcard_info_domain"]),
		Body = rfc4627:encode([{obj, [{"domain",Domain},{"users",Args}]}]),
		Header = [],
		Type = "application/json",
		HTTPOptions = [],
		Options = [],
		case http_client:http_post(Url,Header,Type,Body,HTTPOptions,Options) of
		{ok, {_Status,_Headers, Rslt}} ->
			case catch rfc4627:decode(Rslt) of
			{ok,{obj,Data},[]} ->
				case proplists:get_value("data",Data) of
				V when is_list(V)->
					V;
				_ ->
					[]
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
