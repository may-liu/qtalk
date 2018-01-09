%% Feel free to use, reuse and abuse the code in this file.

-module(http_setmucvcard).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-export([send_http_node_update_muc_vcard/1]).

-include("logger.hrl").
-include("ejb_http_server.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _ } = cowboy_req:method(Req),
    catch ejb_monitor:monitor_count(<<"http_set_muc_vcard">>,1),
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
    {Host,_} =  cowboy_req:host(Req),
	case rfc4627:decode(Body) of
	{ok,Json,[]} -> 
		case http_utils:check_version(Req) of
		false ->
			Rslt = set_update_version(Json),
			cowboy_req:reply(200, [{<<"content-type">>, <<"text/json; charset=utf-8">>}], Rslt, Req);
		true ->
			Rslt = 
			%%	case http_utils:verify_user_key(Req) of
				case http_utils:verify_user_key_p1(Req) of
				true ->
        			set_update_version(Json);
				_ ->
                    case catch http_utils:verify_muc_acess_limit(Req) of
                    true -> 
                        set_update_version(Json);
                    false ->
                    	http_utils:gen_result(false, <<"-1">>, <<"Not found Mac_Key">>)
                    end
				end,
			cowboy_req:reply(200, [	{<<"content-type">>, <<"text/json; charset=utf-8">>}], Rslt, Req)
		end;
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

set_update_version(Json)->
		Server = 
		  case catch ets:lookup(ejabberd_config,<<"http_server">>) of
		[Http_server] when is_record(Http_server,ejabberd_config) ->
		           Http_server#ejabberd_config.val;
		 _ ->
		           "http://127.0.0.1/10050"
		end,
        UserStatus = lists:flatmap(
                        fun({obj,Args}) ->
							Name = get_value("muc_name",Args),
							Nick = get_value("nick",Args),
							Desc = get_value("desc",Args),
							Title = get_value("title",Args),
							Pic = get_value("pic",Args),
							Version = get_muc_vcard_version(Name), 
							Fields = add_cloumn([Nick,Desc,Title,Pic],[<<"show_name">>,<<"muc_desc">>,<<"muc_title">>,<<"muc_pic">>,<<"version">>]),
							Vals = 	 add_cloumn([Nick,Desc,Title,Pic],[Nick,Desc,Title,Pic,Version]),
							Where =  [<<"muc_name='">>,Name,<<"'">>],
							case catch pg_odbc:sql_transaction(<<"ejb_http_server">>,fun() ->
								ejb_odbc_query:update_no_insert(<<"muc_vcard_info">>,Fields,Vals,Where) end) of
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
								catch send_update_vcard_presence(Server,Name),
								spawn(?MODULE, send_http_node_update_muc_vcard,[NMV]),
								[{obj,[{"Set Muc-Vcard",Name},{"version",Version}]}];
							_ ->
								case ejb_odbc_query:insert_muc_vcard_info(Name,Nick,Desc,Title,Pic,Version) of
								{updated, 1} ->
									catch ets:insert(muc_vcard,#muc_vcard{muc_name = Name,show_name = Nick,
														muc_desc = Desc,muc_title = Title,muc_pic= Pic,version = Version}),
									catch send_update_vcard_presence(Server,Name),
									spawn(?MODULE, send_http_node_update_muc_vcard,[#muc_vcard{muc_name = Name,show_name = Nick,
											muc_desc = Desc,muc_title = Title,muc_pic= Pic,version = Version}]),
									[{obj,[{"Set Muc-Vcard",Name},{"version",Version}]}];
								_ ->	
									[{obj,[{"Set Muc-Vcard",Name},{"version",<<"-1">>}]}]
								end
							end
							end,Json),
        list_to_binary(rfc4627:encode({obj,[{"data",UserStatus}]})).

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

send_update_vcard_presence(Server,Name) ->
	case  catch  ets:lookup(ejabberd_config,<<"node">>) of
	[Ejb_node] when is_record(Ejb_node,ejabberd_config) ->
		 case rpc:call(Ejb_node#ejabberd_config.val,http_muc_vcard_presence,send_update_vcard_presence,[Name]) of
		{badrpc,_Reason} ->
			Url = Server ++ "send_muc_presence?muc_name=" ++ binary_to_list(Name) ,
			catch http_client:get(Url);
		_ ->
			ok
		end;
	_ ->
		Url = Server ++ "send_muc_presence?muc_name=" ++ binary_to_list(Name) ,
		catch http_client:get(Url)
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


get_muc_vcard_version(Name) ->
	case ets:lookup(muc_vcard,Name) of 
	[MV] when is_record(MV,muc_vcard)  ->
		VI = binary_to_integer(MV#muc_vcard.version),
		VN = VI + 1,
		integer_to_binary(VN);
	_ ->
		case catch ejb_odbc_query:get_muc_vcard_info_by_name(Name) of
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

send_http_node_update_muc_vcard(Muc) ->
	Nodes = nodes(connected),
	lists:foreach(fun(N) ->
			case string:str(atom_to_list(N),"ejb_http_server@") of
			0 ->
				ok;
			_ ->
				erlang:send({'ejb_cache',N},{update_muc_vcard,Muc})
			end end,Nodes).
