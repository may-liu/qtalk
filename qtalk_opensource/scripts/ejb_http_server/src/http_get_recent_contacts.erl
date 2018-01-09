%% Feel free to use, reuse and abuse the code in this file.

-module(http_get_recent_contacts).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("logger.hrl").
-include("http_req.hrl").
-include("ejb_http_server.hrl").


init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _} = cowboy_req:method(Req),
    catch ejb_monitor:monitor_count(<<"http_get_recent_contacts">>,1),
	case Method of 
	<<"GET">> ->
	    {Host,_} =  cowboy_req:host(Req),
		{ok, Req1} = get_echo(Method,Host,Req),
		{ok, Req1, State};
	<<"POST">> ->
		HasBody = cowboy_req:has_body(Req),
		{ok, Req1} = post_echo(Method, HasBody, Req),
		{ok, Req1, State};
	_ ->
		{ok,Req1} = echo(undefined, Req),
		{ok, Req1, State}
	end.
    	
get_echo(<<"GET">>,_,Req) ->
		cowboy_req:reply(200, [
			{<<"content-type">>, <<"text/json; charset=utf-8">>}
		], <<"No GET method">>, Req).

post_echo(<<"POST">>, true, Req) ->
    {ok, Body, _} = cowboy_req:body(Req),
	Ret = 
		case rfc4627:decode(Body) of
		{ok,{obj,Args},[]}  ->
			User = proplists:get_value("user",Args),
			Key = proplists:get_value("key",Args),
			Count = proplists:get_value("count",Args),
			Limit = proplists:get_value("limit",Args),
			Domain = proplists:get_value("domain",Args,<<"ejabhost1">>),
			case http_utils:verify_user_mac_token(User,Key,Count) of
			true ->
				 get_recent_contacts(User,Limit,Domain);
			_ ->
				 http_utils:gen_result(false, <<"1">>,<<"No found name by ID">>,<<"">>)
			end;
		_ ->
			http_utils:gen_result(false, <<"2">>,<<"Josn parse error">>,<<"">>)
		end,	
	cowboy_req:reply(200, [ {<<"content-type">>, <<"text/json; charset=utf-8">>}], Ret, Req);
post_echo(<<"POST">>, false, Req) ->
	cowboy_req:reply(400, [], <<"Missing Post body.">>, Req);
post_echo(_, _, Req) ->
	cowboy_req:reply(405, Req).

echo(undefined, Req) ->
    cowboy_req:reply(400, [], <<"Missing parameter.">>, Req);
echo(Echo, Req) ->
    cowboy_req:reply(200, [
			        {<<"content-type">>, <<"text/json; charset=utf-8">>}
	    			    ], Echo, Req).

terminate(_Reason, _Req, _State) ->
	ok.

get_recent_contacts(User,Limit,Domain) ->
	NLimit = http_utils:to_integer(Limit,1),
	Recent_contacts = 
	case catch ejb_odbc_query:get_msg_concats(User,http_utils:to_binary(NLimit*2,<<"2">>),Domain) of
		{selected,_,SRes} when is_list(SRes) ->
			Res = lists:usort(SRes),
			Num = case length(SRes) >= NLimit of
					true ->
						NLimit;
					_ ->
						length(SRes)
					end,
			lists:flatmap(fun(N) ->
				[U] = lists:nth(N,Res),
				case catch ets:lookup(vcard_version,U) of
				[U_vcard] when is_record(U_vcard,vcard_version) ->
					[{obj,[{"id",U},{"name",default_val(U_vcard#vcard_version.name,<<"">>)},{"pic",default_val(U_vcard#vcard_version.url,<<"">>)}]}];
				_ ->
					[{obj,[{"id",U},{"name",<<"">>},{"pic",<<"">>}]}]
				end	end,lists:seq(1,Num));
		_	->
			[]
		end,
	Muc_recent_contacts = 
		case catch ejb_odbc_query:get_muc_concats(User,http_utils:to_binary(NLimit,<<"1">>),Domain) of
		{selected,_,SRes1} when is_list(SRes1) ->
			lists:flatmap(fun([Muc]) ->
					 Muc_name = str:concat(Muc,<<"@conference.ejabhost1">>),
					 case catch ets:lookup(muc_vcard,Muc_name) of
					 [MV] when is_record(MV,muc_vcard)-> 
					 	[{obj,[{"id",Muc},{"name",MV#muc_vcard.show_name}]}];
					 _ ->
					 	[{obj,[{"id",Muc},{"name",<<"">>}]}]
					end end,SRes1);
		_ ->
			[]
		end,
	case Recent_contacts of
	[] ->
		 http_utils:gen_result(true, 0, <<"">>,{obj,[{"Users",[{obj,[{"id",<<"admin">>},{"name",<<"">>},{"pic",<<"">>}]}]},
				 	{"Mucs",Muc_recent_contacts}]});
	_ ->
		 http_utils:gen_result(true, 0, <<"">>,{obj,[{"Users",Recent_contacts},{"Mucs",Muc_recent_contacts}]})
	end.
		
default_val(V,Default) ->
	case V of 
	null ->
		Default;
	_ ->
		V
	end.
