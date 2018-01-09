-module(http_get_muc_history_v2).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("logger.hrl").
-include("http_req.hrl").

-record(domain_info,{max_time = 20000000000 ,users = [],args = []}).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    catch ejb_monitor:monitor_count(<<"domain_http_get_muc_history">>,1),
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
		?DEBUG("get_muc_history time  ~p  ~n",[Json]),
		Rslt = 
			case http_utils:verify_user_key(Req) of
			true ->
				http_utils:gen_result(true, 0,<<"Sucesss">>,http_get_muc_history(Json,pg_odbc:escape(Uv)));
			_ ->
				http_utils:gen_result(false, 1, <<"Interface IP limit">>,<<"">>)
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

get_domain_info([],Res) ->
	Res;
get_domain_info([{obj,Args}|Left],Res) ->
	Muc = pg_odbc:escape(proplists:get_value("M",Args,<<"null">>)),
	Domain  = proplists:get_value("D",Args),

	Ls = case ejb_public:checek_domain(Domain) of
		true ->
			Infos = proplists:get_value(Domain,Res,#domain_info{}),
			T =  to_integer(proplists:get_value("T",Args,<<"0">>)),
			Max_time = 
				case Infos#domain_info.max_time < T of
				true ->
					Infos#domain_info.max_time;
				_ ->
					T
				end,
			Users = 
				case Muc of
				<<"">> ->
					Infos#domain_info.users;	
				_ ->
					[Muc] ++ Infos#domain_info.users 
				end,
			LR = lists:keydelete(Domain, 1, Res),
			[{Domain,#domain_info{users = Users,max_time = Max_time}}] ++ LR;
		_ ->
			Infos = proplists:get_value(Domain,Res,#domain_info{}),
			NArgs = [{obj,Args}] ++ Infos#domain_info.args,
			LR = lists:keydelete(Domain, 1, Res),
			[{Domain,#domain_info{args  = NArgs}}] ++ LR
		end,
	get_domain_info(Left,Ls);
get_domain_info([_|Left],Res) ->
	get_domain_info(Left,Res).
	

http_get_muc_history(Json,User)->
	{obj,Args } = Json,
        Time1 = proplists:get_value("T",Args),
        Domain = proplists:get_value("D",Args),
        case proplists:get_value("U",Args) of 
	User ->
	        T =  ejb_public:handle_max_time(Time1,<<"1">>),
		R = get_user_muc_and_last(User,Domain,Time1),
		Loacl_Res = 
			lists:flatmap(fun([Muc,Time]) ->
				case catch ejb_odbc_query:get_muc_msg_info4(Muc,T) of
					{selected, _, SRes}  when is_list(SRes)  ->
						Msgs = lists:map(fun([Name,Nick,Host,Packet]) ->
								    {obj,[{"M",Name},{"N",Nick},{"D",Host},{"B",Packet}]} end,SRes),
						case Msgs of 
						[] ->
							[];
						_ ->
							[{obj,[{"Domain",<<"ejabhost1">>},{"ID",Muc},{"Msg",Msgs},{"Time",Time}]}]
						%	[{obj,[{"Domain",<<"ejabhost1">>},{"Msgs",Msgs},{"Times",Time}]}]	
						end;
					Error ->
						?DEBUG("ERror ~p ~n",[Error]),
						[]
					end end ,R),
		Domain_Res =
			case catch  pg_odbc:sql_query(<<"ejb_http_server">>,
			[<<"select muc_name,domain from user_register_mucs where username = '">>,User,<<"' and  domain <> '">>,Domain,<<"'">>]) of
			 {selected, _ , Res3}  when is_list(Res3) -> 
				http_get_other_domain_muc_history(<<"ejabhost2">>,User,Json);
%						[]
%						end,Res3);
				
				_ ->
				[]
				end,
		Domain_Res ++	Loacl_Res;
	_ ->
		[]
	end.	

get_user_muc_and_last(User,Domain,Time) ->
	case catch  pg_odbc:sql_query(<<"ejb_http_server">>,
		[<<"select muc_name,date from muc_room_users  where username = '">>,User,<<"' ;">>]) of
	{selected, _ , Res}  when is_list(Res) ->
		Res;
	 _ ->
	    []
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
		Url = lists:concat([U1,"/domain/get_muc_history_v2_domain?u=",binary_to_list(User)]),
        Header = [],
        Type = "application/json",
        HTTPOptions = [],
		Options = [],
	%	Body = rfc4627:encode([{obj,Args}]),
		Body = rfc4627:encode(Args),
		case http_client:http_post(Url,Header,Type,Body,HTTPOptions,Options) of
		{ok, {_Status,_Headers, Rslt}} ->
			case catch rfc4627:decode(Rslt) of
			{ok,{obj,Data},[]} ->
				case  proplists:get_value("data",Data) of
				R  when is_list(R) ->
					R;
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
