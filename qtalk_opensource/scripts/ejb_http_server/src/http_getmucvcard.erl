%% Feel free to use, reuse and abuse the code in this file.

-module(http_getmucvcard).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("logger.hrl").
-include("ejb_http_server.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _ } = cowboy_req:method(Req),
    catch ejb_monitor:monitor_count(<<"http_get_muc_vcard">>,1),
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
    {Host,_} =  cowboy_req:host(Req),
	case rfc4627:decode(Body) of
	{ok,Json,[]}  ->
    	case http_utils:check_version(Req) of
		false ->
			Rslt = get_muc_vcard_info(Json),
			cowboy_req:reply(200, [{<<"content-type">>, <<"text/json; charset=utf-8">>}], Rslt, Req);
		true ->
			Rslt =
				case http_utils:verify_user_key(Req) of
				true ->
					get_muc_vcard_info(Json);
				false ->
					<<"">>
				end,
			cowboy_req:reply(200, [{<<"content-type">>, <<"text/json; charset=utf-8">>}], Rslt, Req)
		 end;
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

get_muc_vcard_info(Json)->
        MucStatus = lists:flatmap(fun({obj,Args}) ->
			Muc_name  = proplists:get_value("muc_name",Args),
			Vs  = proplists:get_value("version",Args),
			case catch ets:lookup(muc_vcard,Muc_name) of
			[] ->
				case catch ejb_odbc_query:get_muc_vcard_info_by_name(Muc_name) of
				{selected,[<<"show_name">>,<<"muc_desc">>,<<"muc_title">>,<<"muc_pic">>,<<"version">>],[[SN,MD,MT,MP,PV1]]} ->
					IVs = http_utils:to_integer(Vs),
					IVv = http_utils:to_integer(PV1),	
					if IVs < IVv -> 
						MV = #muc_vcard{muc_name = Muc_name,show_name= SN,
							muc_desc = set_value(MD),muc_title = set_value(MT),muc_pic = set_value(MP),	version = PV1},
						 ets:insert(muc_vcard,MV),
						 [{obj, [{"MN",MV#muc_vcard.muc_name},{"SN",MV#muc_vcard.show_name},{"MD",MV#muc_vcard.muc_desc},
								 	{"MT",MV#muc_vcard.muc_title},{"MP",MV#muc_vcard.muc_pic},{"VS",MV#muc_vcard.version}]}];
					true  ->
						[]
						end;
			  _ ->
			  		[]
			 end;
			[MV] when is_record(MV,muc_vcard) ->
				IVs = http_utils:to_integer(Vs),
				IVv = http_utils:to_integer(MV#muc_vcard.version),
				if IVs < IVv -> 
					[{obj, [{"MN",MV#muc_vcard.muc_name},{"SN",MV#muc_vcard.show_name},{"MD",set_value(MV#muc_vcard.muc_desc)},
					{"MT",set_value(MV#muc_vcard.muc_title)},{"MP",set_value(MV#muc_vcard.muc_pic)},
					{"VS",MV#muc_vcard.version}]}];
				true ->
					[]
				end;
			_ ->
				[]
				end
			end,Json),
        list_to_binary(rfc4627:encode({obj,[{"data",MucStatus}]})).

set_value(V1) ->
	case V1 of
	null ->
		<<"">>;
	_ ->
		V1
	end.
											
