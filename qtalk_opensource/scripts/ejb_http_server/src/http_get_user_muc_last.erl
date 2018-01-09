%% Feel free to use, reuse and abuse the code in this file.
-module(http_get_user_muc_last).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("logger.hrl").
-include("http_req.hrl").

-define(HOST, <<"ejb_http_server">>).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    catch ejb_monitor:monitor_count(<<"http_get_user_muc_last">>,1),
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
%	Req_compress = Req#http_req{resp_compress = true},
    {ok, PBody, _} = cowboy_req:body(Req),
    Header = cowboy_req:get(headers,Req),
	{User,_} = cowboy_req:qs_val(<<"u">>, Req),
    Body =
		case catch proplists:get_value(<<"content-encoding">>,Header) of 
		<<"gzip">> ->
			zlib:gunzip(PBody);
		_ ->
		    PBody
		end,    
    get_user_muc_last(Req,User,Body);
post_echo(<<"POST">>, false, Req) ->
	cowboy_req:reply(400, [], <<"Missing Post body.">>, Req);
post_echo(_, _, Req) ->
	cowboy_req:reply(405, Req).
 


get_user_muc_last(Req,User,Body) when is_binary(User) ->
	case rfc4627:decode(Body) of
	{ok,{obj,Json},[]}  ->
		Rslt = 
            case  proplists:get_value("u",Json) of
            User ->
			    case http_utils:verify_user_key(Req) of
		    	true ->
           	    	http_utils:gen_result(true, 0, <<"Sucess">>,do_get_user_muc_last(pg_odbc:escape(User),Json));
		    	_ ->
			    	http_utils:gen_result(false, 1, <<"Tkey check error">>,<<"">>)
			    end;
            _ ->
                http_utils:gen_result(false, 1, <<"User match error">>,<<"">>)
            end,
		cowboy_req:reply(200, [	{<<"content-type">>, <<"text/json; charset=utf-8">>}], Rslt, Req);
	_ ->
		cowboy_req:reply(200, [ {<<"content-type">>, <<"text/json; charset=utf-8">>}], 
        http_utils:gen_result(false, 1, <<"Json parse error">>,<<"">>), Req)
	end;
get_user_muc_last(_,_,Req) ->
    cowboy_req:reply(200, [ {<<"content-type">>, <<"text/json; charset=utf-8">>}], 
        http_utils:gen_result(false, 1, <<"User info error">>,<<"">>),Req).
										

echo(undefined, Req) ->
    cowboy_req:reply(400, [], <<"Missing parameter.">>, Req);
echo(Echo, Req) ->
    cowboy_req:reply(200, [
			        {<<"content-type">>, <<"text/json; charset=utf-8">>}
	    			    ], Echo, Req).

terminate(_Reason, _Req, _State) ->
	ok.

do_get_local_user_muc_last(Domain,Json,User) ->
    case catch  pg_odbc:sql_query(?HOST,[<<"select grouplist.muc_name,
               max(extract(epoch from date_trunc('MS', muc_room_history.create_time))*1000) 
            from (select muc_name from user_register_mucs where username = '">>,User,
           <<"' and domain = '">>,Domain,<<"' and registed_flag = 1 ) as grouplist 
              left join muc_room_history on grouplist.muc_name = muc_room_history.muc_room_name group by grouplist.muc_name;">>]) of 
    {selected, _,Res} when is_list(Res) ->
        lists:flatmap(fun([M,T]) ->
            case T of
            null ->
                 [{obj,[{"M",M},{"T",0},{"D",<<"conference.ejabhost1">>},{"E",1}]}];
             _ ->
                 [{obj,[{"M",M},{"T",binary_to_integer(T)},{"D",<<"conference.ejabhost1">>},{"E",0}]}]
             end end,Res);
        _ ->
            []
        end.

do_get_user_muc_last(User,Args) ->
    case catch  pg_odbc:sql_query(?HOST,[<<"select distinct(domain) from user_register_mucs where username = '">>,
			User,<<"' and registed_flag = 1 ;">>]) of
    {selected, _,Res} when is_list(Res) ->
        lists:flatmap(fun([D]) ->   
	        case catch ejb_public:checek_domain(D) of
	        true ->
        	        do_get_local_user_muc_last(D,Args,User); 
      		_ ->
                	do_http_get_other_domain_muc_history(D,Args,User)
            end  end,Res);
    _ ->
            []
    end.

do_http_get_other_domain_muc_history(Domain,Args,User) ->    
    case catch  pg_odbc:sql_query(?HOST,[<<"select muc_name from user_register_mucs where domain = '">>,Domain,
            <<"' and username = '">>,User,<<"' and registed_flag = 1;">>]) of
    {selected, _,Res} when is_list(Res) ->
        case catch ejb_public:get_url_by_domain(Domain) of
        [] ->
            [];
        U1 when is_list(U1) ->      
		    Url = lists:concat([U1,"/domain/get_user_muc_last_domain?u=",binary_to_list(User)]),
            Header = [],
            Type = "application/json",
            HTTPOptions = [],
		    Options = [],
		    Body = rfc4627:encode([{obj,Args}]),
	    	case http_client:http_post(Url,Header,Type,Body,HTTPOptions,Options) of
		    {ok, {_Status,_Headers, Rslt}} ->
		    	case catch rfc4627:decode(Rslt) of
		    	{ok,{obj,Data},[]} ->
				case proplists:get_value("data",Data) of
				undefined ->
					[];
				R ->
					R
				end;
		    	_ ->
		    		[]
		    	end;
	    	_ ->
		    	[]
		    end
        end;
	_ ->
		[]
	end.
