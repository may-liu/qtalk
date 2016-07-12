%% Feel free to use, reuse and abuse the code in this file.
%%========================================================
%%设置群名片接口
%%========================================================
-module(http_set_muc_vcard).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("logger.hrl").
-include("ejabberd_extend.hrl").

init(_Transport, Req, _) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _ } = cowboy_req:method(Req),
	case Method of 
	<<"GET">> ->
	    {Host,_ } =  cowboy_req:host(Req),
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
	case rfc4627:decode(Body) of
	{ok,Json,[]} -> 
        Servers = ejabberd_config:get_myhosts(),
        Server = lists:nth(1,Servers),
		Rslt =
			case http_utils:verify_user_key(Server,Req) of
			true ->
        		set_update_version(Server,Json);
			_ ->
           		http_utils:gen_result(false, 2, <<"Not found Mac_Key">>)
			end,
		cowboy_req:reply(200, [	{<<"content-type">>, <<"text/json; charset=utf-8">>}], Rslt, Req);
	_ ->
		 cowboy_req:reply(200, [ {<<"content-type">>, <<"text/json; charset=utf-8">>}], 
				 	http_utils:gen_result(false, 1,<<"Josn parse error">>), Req)
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

set_update_version(Server,Json)->
        UserStatus = lists:flatmap(
                        fun({obj,Args}) ->
							Name = get_value("muc_name",Args),
							Nick = get_value("nick",Args),
							Desc = get_value("desc",Args),
							Title = get_value("title",Args),
							Pic = get_value("pic",Args),
							Version = get_curren_version(Name), 
							Fields = add_cloumn([Nick,Desc,Title,Pic],[<<"show_name">>,<<"muc_desc">>,<<"muc_title">>,<<"muc_pic">>,<<"version">>]),
							Vals = 	 add_cloumn([Nick,Desc,Title,Pic],[Nick,Desc,Title,Pic,Version]),
							Where =  [<<"muc_name='">>,Name,<<"'">>],
							case catch ejabberd_odbc:sql_transaction(Server,fun() ->
								odbc_queries:update_no_insert(<<"muc_vcard_info">>,Fields,Vals,Where) end) of
							{atomic,ok} ->
								NMV =
								case ets:lookup(muc_vcard,Name) of
								[OV] when is_record(OV,muc_vcard) ->
									#muc_vcard{muc_name = Name,show_name = set_value(Nick,OV#muc_vcard.show_name),
									   			muc_desc = set_value(Desc,OV#muc_vcard.muc_desc),	
									   			muc_title = set_value(Title,OV#muc_vcard.muc_title),	
									   			muc_pic = set_value(Pic,OV#muc_vcard.muc_pic),	
												version = Version};
								_ ->
									
									#muc_vcard{muc_name = Name,show_name = Nick,muc_desc = Desc,muc_title = Title,muc_pic= Pic,version = Version}
								end,
								catch ets:insert(muc_vcard,NMV),
								[{obj,[{"set_muc_vcard",Name},{"version",Version}]}];
							_ ->
								case odbc_queries:insert_muc_vcard_info(Server,Name,Nick,Desc,Title,Pic,Version) of
								{updated, 1} ->
									catch ets:insert(muc_vcard,#muc_vcard{muc_name = Name,show_name = Nick,
														muc_desc = Desc,muc_title = Title,muc_pic= Pic,version = Version}),
									[{obj,[{"set_muc_vcard",Name},{"version",Version}]}];
								_ ->	
									[{obj,[{"set_muc_vcard",Name},{"version",<<"-1">>}]}]
								end
							end
							end,Json),
		http_utils:gen_result(true,0,UserStatus).

get_value(V,Args) ->
	proplists:get_value(V,Args,<<"">>).

set_value(V1,V2) ->
	case V1 of 
	<<"''">> ->
		V2;
	<<"">> ->
		V2;
	_ ->
		V1
	end.

add_cloumn(Value,Column) when is_list(Value) andalso is_list(Column) ->
	lists:flatmap(fun(Num) ->
		case catch lists:nth(Num,Value) of
		<<"">> ->
			[];
		_ ->
			[lists:nth(Num,Column)]
		end end,lists:seq(1,length(Column)));

add_cloumn(_,_) ->
	[].

get_curren_version(Name) ->
	case ets:lookup(muc_vcard,Name) of 
	[MV] when is_record(MV,muc_vcard)  ->
		VI = binary_to_integer(MV#muc_vcard.version),
		VN = VI + 1,
		integer_to_binary(VN);
	_ ->
		case catch odbc_queries:get_muc_vcard_info_by_name(Name) of
       	{selected,[<<"show_name">>,<<"muc_desc">>,<<"muc_title">>,<<"muc_pic">>,<<"version">>],
			[[SN,MD,MT,MP,VS]]} ->
			MV = #muc_vcard{muc_name = Name,show_name = SN,muc_desc = MD,muc_title = MT,muc_pic= MP,version = VS},
			catch ets:insert(muc_vcard,MV),
	       	VI = binary_to_integer(VS),
			VN = VI + 1,
			integer_to_binary(VN);
          	 _ ->
				<<"1">>
			 end
	end.	
