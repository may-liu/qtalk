%% Feel free to use, reuse and abuse the code in this file.
-module(http_get_muc_history3).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("logger.hrl").
-include("http_req.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    catch ejb_monitor:monitor_count(<<"http_get_muc_history_domain">>,1),
    do_handle(Req, State).

do_handle(Req, State) ->
	{Method, _} = cowboy_req:method(Req),
	case Method of 
	<<"GET">> ->
	    {Host,_} = cowboy_req:host(Req),
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
	 Req_compress = Req#http_req{resp_compress = true},
    {ok, PBody, _} = cowboy_req:body(Req),
    Header = cowboy_req:get(headers,Req),
	{User,_} = cowboy_req:qs_val(<<"u">>, Req),
	Uv = case User of 
		undefined ->
			<<"null">>;
		_ ->
			User
		end,
    Body =
		case catch proplists:get_value(<<"content-encoding">>,Header) of 
		<<"gzip">> ->
			zlib:gunzip(PBody);
		_ ->
		    PBody
		end,    
	case rfc4627:decode(Body) of
	{ok,Json,[]}  ->
		Rslt = 
			case http_utils:verify_user_key(Req) of
			true ->
			Time1 = mod_time:get_exact_timestamp(),
			Res = http_get_muc_history(Json,pg_odbc:escape(Uv)),
			Time2 =  mod_time:get_exact_timestamp(),
           		http_utils:gen_result(true, 0, <<"Sucess">>,Res);
			_ ->
				http_utils:gen_result(false, 1, <<"Tkey check error">>,<<"">>)
			end,
		cowboy_req:reply(200, [	{<<"content-type">>, <<"text/json; charset=utf-8">>}], Rslt, Req_compress);
	_ ->
		cowboy_req:reply(200, [ {<<"content-type">>, <<"text/json; charset=utf-8">>}], <<"Josn parse error">>, Req_compress)
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

http_get_muc_history(Json,User)->
	LMuc_last = get_user_muc_and_last(User),
	lists:flatmap(fun({obj,Args }) ->
		Muc = pg_odbc:escape(
			case proplists:get_value("M",Args) of 
			undefined ->
				<<"null">>;
			M ->
				M
			end),
		Domain  = proplists:get_value("D",Args),
		case ejb_public:checek_domain(Domain) of
		true ->
			case check_muc_in_db(Muc,LMuc_last) of
			false ->
				[];
			LT ->
				Time1 = proplists:get_value("T",Args),
				T =  ejb_public:handle_max_time(Time1,<<"0">>),
				case catch ejb_odbc_query:get_muc_msg_info4(Muc,T) of
				{selected,  [<<"muc_room_name">>,<<"nick">>,<<"host">>,<<"packet">>], SRes}
					when is_list(SRes) ->
						Msgs = 
							lists:map(fun([_Name,Nick,Host,Packet]) ->
					    	 {obj,[{"N",Nick},{"D",Host},{"B",Packet}]} end,SRes),
						make_ret_json(User,Muc,Domain,Msgs,LT);
			       	A ->
       			 		[]
        			end
			end;
		_ ->
                        Time22222 =  mod_time:get_exact_timestamp(),
			Ret_oterh_domian = http_get_other_domain_muc_history(Domain,User,Args),
			Time22223 =  mod_time:get_exact_timestamp(),
			?DEBUG("User ~p ,get_muc_history domian time ~p ~n",[User,Time22223-Time22222]),
			Ret_oterh_domian
		end
		end,Json).

make_ret_json(User,Muc,Domain,Msgs,Time) ->
	case Msgs =:= [] of
	true ->
		[];
	_ ->
		ITime =
  	    	case User of
			undefined ->
				0;
			_ ->
				binary_to_integer(Time)
			end,
		[{obj, [{"Time", ITime}, {"ID",Muc},{"Domain",Domain},{"Msg", Msgs}]}]
	end.

get_user_muc_and_last(User) ->
    case catch  pg_odbc:sql_query(<<"ejb_http_server">>,
		[<<"select muc_name,date from muc_room_users  where username = '">>,User,<<"';">>]) of
	{selected, _, Res}  when is_list(Res) ->
	    	lists:map(fun([M,T]) ->
			{M,T} end,Res);
	 _ ->
	    []
	end.

check_muc_in_db(Muc,LMuc_last) ->
	case proplists:get_value(Muc,LMuc_last) of
	undefined ->
		false;
	T ->
		T
	end.
	
to_integer(V) when is_binary(V) ->
	binary_to_integer(V);
to_integer(V) when is_integer(V) ->
	V;
to_integer(_) ->
	0.

http_get_other_domain_muc_history(Domain,User,Args) ->
	case catch ejb_public:get_url_by_domain(Domain) of
	[] ->
		[];
	U1 when is_list(U1) ->
		Url = lists:concat([U1,"/domain/get_muc_history_domain?u=",binary_to_list(User)]),
        Header = [],
        Type = "application/json",
        HTTPOptions = [],
		Options = [],
		Body = rfc4627:encode([{obj,Args}]),
		case http_client:http_post(Url,Header,Type,Body,HTTPOptions,Options) of
		{ok, {_Status,_Headers, Rslt}} ->
			case catch rfc4627:decode(Rslt) of
			{ok,{obj,Data},[]} ->
				proplists:get_value("data",Data);
			_ ->
				[]
			end;
		_ ->
			[]
		end;
	_ ->
		[]
	end.

