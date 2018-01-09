-module(subscription).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").

-export([subscription_message/3,create_rbt_ets/0,get_subscription_url/2,update_subscription_info/1]).
-export([update_user_robots/1,get_subscription_cn_name/1]).

-record(rbt_info,{name,cn_name,url,body,version}).
-record(user_rbts,{name,rbt}).
-record(rbts_map,{cn_name,en_name}).


subscription_message(From,To,Packet) ->
	#xmlel{name = Name, attrs = Attrs, children = Els} = Packet,
	%%case catch xml:get_tag_attr_s(<<"id">>,xml:get_subtag(El,<<"body">>)) of
	Msg_Body = xml:get_subtag_cdata(Packet, <<"body">>),
    {Is_Muc,User} = 
        case catch str:str(From#jid.lserver,<<"conference.">>) of
        0 ->
            {<<"0">>,<<"">>};
        N when is_integer(N) ->
            {<<"1">>,From#jid.lresource};
        _ ->
            {<<"0">>,<<"">>}
        end,
	Body = rfc4627:encode({obj,[{"from",From#jid.luser},{"body",Msg_Body},{"is_muc",Is_Muc},{"domain",From#jid.lserver},{"user",User}]}),
    ?DEBUG("Args Json ~p ~n",[Body]),
	Header = [],
    Type = "application/json",
    HTTPOptions = [{timeout,1500}],
    Options = [],
	Host = To#jid.lserver,
	case get_subscription_url(Host,To#jid.luser) of
	error ->
		?INFO_MSG("User ~p ,No found requeset url  ~n",[To#jid.luser]),
		error;
	Url ->
		?DEBUG("Url ~p ~n",[Url]),
		Res = http_client:http_post(Host,binary_to_list(Url), Header, Type, Body, HTTPOptions, Options),
		?DEBUG("Res ~p ~n",[Res]),
		Res
	end.

create_rbt_ets() ->
	catch ets:new(rbt_info, [named_table, ordered_set, public,{keypos, 2},{write_concurrency, true}, {read_concurrency, true}]),
	catch ets:new(user_rbts, [named_table, bag, public,{keypos, 2},{write_concurrency, true}, {read_concurrency, true}]),
	catch ets:new(rbts_map, [named_table, ordered_set, public,{keypos, 2},{write_concurrency, true}, {read_concurrency, true}]).

update_subscription_info(Server) ->
    ets:delete_all_objects(rbt_info),
    ets:delete_all_objects(rbts_map),
    case catch ejabberd_odbc:sql_query(Server,
		[<<"select en_name,cn_name,request_url,rbt_body,rbt_version from robot_info;">>]) of
	{selected,
		   	[<<"en_name">>,<<"cn_name">>,<<"request_url">>,<<"rbt_body">>,<<"rbt_version">>], SRes} 
		when is_list(SRes) ->
			lists:foreach(fun([Name,Cn_name,Url,Body,Vesion]) ->
                catch ets:insert(rbts_map,#rbts_map{cn_name = Cn_name,en_name = Name}),
				catch ets:insert(rbt_info,#rbt_info{name = Name,cn_name = Cn_name,url = Url,body = Body,version = Vesion}) end,SRes);
	_ ->
		ok
	end.

get_subscription_url(Server,Name) ->
	case catch ets:lookup(rbt_info,Name) of 
	[SI] when is_record(SI,rbt_info) ->
		SI#rbt_info.url;
    _ ->
        error
	end.

get_subscription_cn_name(Name) ->
    case catch ets:lookup(rbt_info,Name) of
    [SI] when is_record(SI,rbt_info) ->
        SI#rbt_info.cn_name;
    _ ->
        Name
    end.
    

update_user_robots(Server) ->
	ets:delete_all_objects(user_rbts),
	case catch ejabberd_odbc:sql_query(Server,<<"select user_name,rbt_name from robot_pubsub;">>) of
	{selected, [<<"user_name">>,<<"rbt_name">>], SRes} when is_list(SRes) ->
		lists:foreach(fun([User,Rbt]) ->
			ets:insert(user_rbts,#user_rbts{name = User,rbt = Rbt}) end,SRes);
	_ ->
		ok
	end.
