-module(domain_search_vcard).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("ejb_http_server.hrl").
-include("logger.hrl").
-include("http_req.hrl").

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
    	
get_echo(<<"GET">>,Host,Req) ->
	Res = 
		case http_utils:verify_user_key(Req) of
		true ->
			do_process(Req);
		_ ->
          	http_utils:gen_result(false, 1, <<"Not found Mac_Key">>)
		end,
	cowboy_req:reply(200, [	{<<"content-type">>, <<"text/plain; charset=utf-8">>}], Res, Req);
get_echo(<<"Get">>,_,Req) ->
	cowboy_req:reply(405, Req).

echo(undefined, Req) ->
    cowboy_req:reply(400, [], <<"Missing parameter.">>, Req);
echo(Echo, Req) ->
    cowboy_req:reply(200, [
			        {<<"content-type">>, <<"text/plain; charset=utf-8">>}
	    			    ], Echo, Req).

post_echo(<<"POST">>, _, Req) ->
    cowboy_req:reply(400, [], <<"Missing Post body.">>, Req);
post_echo(_, _, Req) ->
    cowboy_req:reply(405, Req).

terminate(_Reason, _Req, _State) ->
	ok.


do_process(Req) ->
	{P,_} = cowboy_req:qs_val(<<"p">>, Req),
	{V,_} = cowboy_req:qs_val(<<"v">>, Req),
	{Str_Keyword,_} = cowboy_req:qs_val(<<"keyword">>, Req),
	Keyword = binary_to_list(Str_Keyword),
    Param = 
		case is_mobile(Keyword) of
    	true -> "mobile=" ++ Keyword;
		false ->
			case is_email(Keyword) of
			true -> "email=" ++ Keyword;
			false -> "username=" ++ Keyword
		   	end
		end,
    QueryData = Param ++ "&p=" ++ binary_to_list(P) ++ "&v=" ++ binary_to_list(V),
	Url = "http://xxxxxxxxxx?" ++ QueryData,
	case http_client:get(Url) of
	{ok, _Header, Body} ->
		gen_result(Body);
	 Error ->
	 	?DEBUG("Error ~p ~n",[Error]),
	%%	 "{\"ret\":false, \"errmsg\":\"get failed\", \"errcode\":\"3\"}"
		[]
	end.

gen_result(Body) ->
  %%  case ejahttp_json:decode_string(Body) of
  	case rfc4627:decode(Body) of
	{ok,{obj,RetList},[]}  ->
		case proplists:get_value("ret", RetList) of
		<<"1">> ->
			case proplists:get_value("data", RetList) of
			{obj, CData} ->
				NickName = proplists:get_value("nickname", CData, ""),
				UserName = proplists:get_value("username", CData, ""),
				Domain = <<"ejabhost2">>,
				gen_result(<<"1">>,<<"sucess">>,[{obj, [{"domain", Domain}, {"nickname", NickName}, {"username", UserName}]}]);
			_ ->
				gen_result(<<"1">>,<<"sucess">>,[])
			end;
		 _ ->
			gen_result(<<"1">>,<<"sucess">>,[])
		 end;
   _ ->
	gen_result(<<"0">>,<<"failed">>,[])
  end.

is_mobile(Keyword) ->
	case length(Keyword) of
    11 ->
		lists:all(fun(C) when C > 47, C < 58 -> true;
		(_) -> false end,  Keyword);
	 _ ->
	    false
	 end.

is_email(Keyword) ->
	case re:split(Keyword, "[@.]") of
    [_, _, _] -> true;
	 _ -> false
	end.

gen_result(Ret_code,Ret,Data) ->
	list_to_binary(rfc4627:encode({obj, [{"ret", Ret_code}, {"message", Ret}, {"data", Data}]})).
