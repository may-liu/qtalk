%% Feel free to use, reuse and abuse the code in this file.
-module(http_setwhitelist).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("logger.hrl").
-include("http_req.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    handle(Req, State, iplimit_util:check_ips_limit(Req,<<"1">>,<<"ALL">>)).

handle(Req, State, false) ->
    Res = http_utils:gen_result(false, <<"3">>, <<"ip is limited">>),
    {ok, NewReq} = cowboy_req:reply(200, [
                                    {<<"content-type">>, <<"text/plain; charset=utf-8">>}
                                   ], Res, Req),
    {ok, NewReq, State};
handle(Req, State, _) ->
    {Method, _} = cowboy_req:method(Req),
    case Method of 
        <<"GET">> ->
            {ok, Req2} = get_echo(Method,Req),
            {ok, Req2, State};
        <<"POST">> ->
            HasBody = cowboy_req:has_body(Req),
            {ok, Req2} = post_echo(Method, HasBody, Req),
            {ok, Req2, State};
        _ ->
            {ok,Req2} = echo(undefined, Req),
            {ok, Req2, State}
    end.
    	
get_echo(<<"GET">>,Req) ->
    Req_compress = Req#http_req{resp_compress = true},
    {Username,UsernameReq} = cowboy_req:qs_val(<<"username">>, Req),
    {Flag, _FlagReq} = cowboy_req:qs_val(<<"flag">>, UsernameReq),
    Rslt = set_whitelist(Username, Flag),
    cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain; charset=utf-8">>}], Rslt,Req_compress);
get_echo(<<"Get">>,Req) ->
	cowboy_req:reply(405, Req).

post_echo(<<"POST">>, true, Req) ->
    Req_compress = Req#http_req{resp_compress = true},
    Header = cowboy_req:get(headers,Req),
    {ok, Body1, _} = cowboy_req:body(Req),

	Body = 
		case catch proplists:get_value(<<"content-encoding">>,Header) of 
               <<"gzip">> ->
                   zlib:gunzip(Body1);
               _ ->
                   Body1
           end,

    BodyList = parse_post_data_urlencoded(Body),
    Username = proplists:get_value("username", BodyList),
    Flag = proplists:get_value("flag", BodyList),
    Rslt = set_whitelist(Username, Flag),

	cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain; charset=utf-8">>}], Rslt,Req_compress);
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

set_whitelist(Username, Flag) when Username =/= undefined, Flag =/= undefined ->
    case Flag of
        Flag when Flag =:= "single"; Flag =:= <<"single">> ->
            do_set_whitelist(to_binary(Username), <<"1">>);
        Flag when Flag =:= "unsingle"; Flag =:= <<"unsingle">> ->
            do_set_whitelist(to_binary(Username), <<"0">>);
        Flag when Flag =:= "delete"; Flag =:= <<"delete">> ->
            delete_whitelist(to_binary(Username));
        _ ->
            http_utils:gen_result(false, <<"2">>, <<"error parameters">>)
    end;

set_whitelist(_, _) ->
    http_utils:gen_result(false, <<"1">>, <<"Missing parameter">>).

do_set_whitelist(Username, Flag) ->
	case ejb_odbc_query:get_whitelist_by_user(Username) of
	{selected,_,[[]]} ->
   		case ejb_odbc_query:update_whitelist(Username,Flag) of
        {updated, _} ->
            http_utils:gen_result(true, <<"0">>, <<"set whitelist success">>);
        {aborted, Reason} ->
            http_utils:gen_result(false, proplists:get_value(code, Reason), proplists:get_value(message, Reason))
		end;
	_ ->
		case ejb_odbc_query:insert_whitelist(Username,Flag) of
        {updated, _} ->
            http_utils:gen_result(true,  <<"0">>,  <<"set whitelist success">>);
		_ ->
			http_utils:gen_result(false, <<"-1">>, <<"insert whitelist error">>)
		end
    end.

delete_whitelist(Username) ->
	case ejb_odbc_query:delete_whitelist(Username) of
	{updated, _} ->
		http_utils:gen_result(true, <<"0">>, <<"delete whitelist success">>);
	{aborted, Reason} ->
        http_utils:gen_result(false, proplists:get_value(code, Reason), proplists:get_value(message, Reason));
	A ->
		?DEBUG("A ~p ~n",[A]),
        http_utils:gen_result(false, <<"-1">>, <<"Unknown error">>)
	end.
		
parse_post_data_urlencoded(List) when is_list(List) ->
    parse_post_data_urlencoded(list_to_binary(List));
parse_post_data_urlencoded(Bin) when is_binary(Bin) ->
        do_parse_spec(Bin, nokey, [], key).

do_parse_spec(<<$%, Hi:8, Lo:8, Tail/binary>>, Last, Cur, State)
    when Hi /= $u ->
        Hex = hex_to_integer([Hi, Lo]),
            do_parse_spec(Tail, Last, [ Hex | Cur],  State);

do_parse_spec(<<$&, Tail/binary>>, _Last , Cur,  key) ->
        [{lists:reverse(Cur), undefined} |
              do_parse_spec(Tail, nokey, [], key)];  %% cont keymode

do_parse_spec(<<$&, Tail/binary>>, Last, Cur, value) ->
        V = {Last, lists:reverse(Cur)},
            [V | do_parse_spec(Tail, nokey, [], key)];

do_parse_spec(<<$+, Tail/binary>>, Last, Cur,  State) ->
        do_parse_spec(Tail, Last, [$\s|Cur], State);

do_parse_spec(<<$=, Tail/binary>>, _Last, Cur, key) ->
        do_parse_spec(Tail, lists:reverse(Cur), [], value); %% change mode

do_parse_spec(<<$%, $u, A:8, B:8,C:8,D:8, Tail/binary>>,
                             Last, Cur, State) ->
        Hex = hex_to_integer([A,B,C,D]),
            do_parse_spec(Tail, Last, [ Hex | Cur],  State);

do_parse_spec(<<H:8, Tail/binary>>, Last, Cur, State) ->
        do_parse_spec(Tail, Last, [H|Cur], State);
do_parse_spec(<<>>, nokey, Cur, _State) ->
        [{lists:reverse(Cur), undefined}];
do_parse_spec(<<>>, Last, Cur, _State) ->
        [{Last, lists:reverse(Cur)}];
do_parse_spec(undefined,_,_,_) ->
        [];
do_parse_spec(QueryList, Last, Cur, State) when is_list(QueryList) ->
        do_parse_spec(list_to_binary(QueryList), Last, Cur, State).

hex_to_integer(Hex) ->
        erlang:list_to_integer(Hex, 16).

to_binary(List) when is_list(List) ->
    list_to_binary(List);
to_binary(Binary) when is_binary(Binary) ->
    Binary.
