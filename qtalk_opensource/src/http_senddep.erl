%% Feel free to use, reuse and abuse the code in this file.

-module(http_senddep).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-export([http_send_message/1]).
-export([get_departmen_emp/2]).
-include("ejabberd.hrl").
-include("logger.hrl").
-include("http_req.hrl").

-record(department_users,{dep1,dep2,dep3,dep4,dep5,user}).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    {Url,_Req_t} = cowboy_req:url(Req),
%%    handle(Req, State, iplimit_util:check_ip(Req)).
	handle(Req, State,true).

handle(Req, State, false) ->
    Req_Res = Req#http_req{resp_compress = true},
    Res = http_utils:gen_result(false, <<"3">>, <<"ip is limited">>),
    {ok, NewReq} = cowboy_req:reply(200, [
                                    {<<"content-type">>, <<"text/plain; charset=utf-8">>}
                                   ], Res, Req_Res),
    {ok, NewReq, State};
handle(Req, State, _) ->
	{Method, Req2} = cowboy_req:method(Req),
	case Method of 
	<<"GET">> ->
	    {Host,Req3} =  cowboy_req:host(Req),
		{ok, Req4} = get_echo(Method,Host,Req3),
		{ok, Req4, State};
	<<"POST">> ->
		HasBody = cowboy_req:has_body(Req2),
		{ok, Req3} = post_echo(Method, HasBody, Req2),
		{ok, Req3, State};
	_ ->
		{ok,Req3} = echo(undefined, Req2),
		{ok, Req3, State}
	end.
    	
get_echo(<<"GET">>,_,Req) ->
		cowboy_req:reply(200, [
			{<<"content-type">>, <<"text/json; charset=utf-8">>}
		], <<"No GET method">>, Req).

post_echo(<<"POST">>, true, Req) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
	?DEBUG("Body ~p ~n",[Body]),
    {Host,Req3} =  cowboy_req:host(Req),
	case rfc4627:decode(Body) of
	{ok,Json,[]} -> 
		Res = http_send_message(Json),
		cowboy_req:reply(200, [{<<"content-type">>, <<"text/json; charset=utf-8">>}], Res, Req);
	_ ->
		 cowboy_req:reply(200, [ {<<"content-type">>, <<"text/json; charset=utf-8">>}], <<"Josn parse error">>, Req)
	end;
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
http_send_message(Json) ->
    Servers = ejabberd_config:get_myhosts(),
    Server = lists:nth(1,Servers),
	http_send_message(Server,Json).

	
http_send_message(Server,Json)->
	[{obj,Args }] = Json ,
	From = proplists:get_value("From",Args),
	Depname = proplists:get_value("Depname",Args),
	Body  = proplists:get_value("Body",Args),
	Res = 
		case is_suit_from(From) of
		true ->
			case Body of
			undefined ->
				rfc4627:encode({obj,[{"data",<<"Message Body is Null">>}]});
			_ ->
				JFrom = jlib:make_jid(From,Server,<<"">>),
				case JFrom of 
				error ->
					rfc4627:encode({obj,[{"data",<<"From make jid error">>}]});
				_ ->
					send_dep_message(JFrom,Server,Depname,Body),
					rfc4627:encode({obj,[{"data",<<"Send Message Ok">>}]})
				end
			end;
		false ->	
    	    Us2 = {obj,[{"data",<<"From not suit">>}]},
    	    Us1 = rfc4627:encode(Us2)
		end,
      list_to_binary(Res).

make_send_packet(To,Msg) ->
	Bid = list_to_binary("http_" ++ integer_to_list(mod_time:get_exact_timestamp())),
	xml:to_xmlel(
		%%	{xmlel	,"message",	[{"type","headline"},{"id","web9999"},{"to",jlib:jid_to_string(To)}],
			{xmlel	,<<"message">>,	[{<<"type">>,<<"chat">>},{<<"to">>,jlib:jid_to_string(To)}],
				[{xmlel,<<"active">>,[{<<"xmlns">>,<<"http://jabber.org/protocol/chatstates">>}],[]},
					{xmlel,<<"body">>,[{<<"id">>,Bid},{<<"msgType">>,"1"}],[{xmlcdata, Msg}]}]}).

is_suit_from(From) ->
	true.	

send_dep_message(From,Server,Depname,Body) ->
	Users = 
		case str:words(Depname, $/) of
		0 ->
			[];
		N ->
			get_departmen_emp(Depname,N)
		end,
	lists:foreach(fun(U) ->
			JTo = jlib:make_jid(U,Server,<<"">>),
			Packet = make_send_packet(JTo,Body),
            ejabberd_router:route(From,JTo,Packet)  end ,Users).
			
	
get_departmen_emp(Department,1) ->
	Dep = str:sub_string(Department,2,size(Department)),
	catch case ets:select(department_users,[{#department_users{dep1 = Dep,user = '$1', _ = '_'}, [], ['$1']}]) of
	[] ->
		[];
	U when is_list(U) ->
		U;
	_ ->
		[]
	end;
get_departmen_emp(Department,2) ->
	Deplist = str:tokens(Department,<<"/">>),
	[Dep1,Dep2] = Deplist,
	catch case ets:select(department_users,[{#department_users{dep1 = Dep1,dep2 = Dep2,user = '$1', _ = '_'}, [], ['$1']}]) of
	[] ->
		[];
	U when is_list(U) ->
		U;
	_ ->
		[]
	end;
get_departmen_emp(Department,3) ->
	Deplist = str:tokens(Department,<<"/">>),
	[Dep1,Dep2,Dep3] = Deplist,
	catch case ets:select(department_users,[{#department_users{dep1 = Dep1,dep2 = Dep2,dep3 = Dep3,user = '$1', _ = '_'}, [], ['$1']}]) of
	[] ->
		[];
	U when is_list(U) ->
		U;
	_ ->
		[]
	end;
get_departmen_emp(Department,4) ->
	Deplist = str:tokens(Department,<<"/">>),
	[Dep1,Dep2,Dep3,Dep4] = Deplist,
	catch case ets:select(department_users,
		[{#department_users{dep1 = Dep1,dep2 = Dep2,dep3 = Dep3,dep4 = Dep4,user = '$1', _ = '_'}, [], ['$1']}]) of
	[] ->
		[];
	U when is_list(U) ->
		U;
	_ ->
		[]
	end;
get_departmen_emp(Department,5) ->
	Deplist = str:tokens(Department,<<"/">>),
	[Dep1,Dep2,Dep3,Dep4,Dep5] = Deplist,
	catch case ets:select(department_users,
			[{#department_users{dep1 = Dep1,dep2 = Dep2,dep3 = Dep3,dep4 = Dep4,dep5 = Dep5,user = '$1', _ = '_'}, [], ['$1']}]) of
	[] ->
		[];
	U when is_list(U) ->
		U;
	_ ->
		[]
	end.
