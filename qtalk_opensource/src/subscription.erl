-module(subscription).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").

-export([subscription_message/3,create_rbt_ets/0,get_subscription_url/2,update_subscription_info/1]).
-export([update_user_robots/1]).

-record(rbt_info,{name,url,body,version}).
-record(user_rbts,{name,rbt}).

create_rbt_ets() ->
	catch ets:new(rbt_info, [named_table, ordered_set, public,{keypos, 2},{write_concurrency, true}, {read_concurrency, true}]),
	catch ets:new(user_rbts, [named_table, bag, public,{keypos, 2},{write_concurrency, true}, {read_concurrency, true}]).

subscription_message(From,To,Packet) ->
%%	#xmlel{name = Name, attrs = Attrs, children = Els} = Packet,
	Msg_Body = xml:get_subtag_cdata(Packet, <<"body">>),
	Body = rfc4627:encode({obj,[{"from",From#jid.luser},{"body",Msg_Body}]}),
	Header = [],
    Type = "application/json",
    HTTPOptions = [{timeout,1500}],
    Options = [],
	Host = To#jid.lserver,
	case get_subscription_url(Host,To#jid.luser) of
	error ->
		?DEBUG("No found requeset url ~n",[]),
		error;
	Url ->
		http_client:http_post(Host,binary_to_list(Url), Header, Type, Body, HTTPOptions, Options)
	end.


update_subscription_info(Server) ->
    ets:delete_all_objects(rbt_info),
    case catch ejabberd_odbc:sql_query(Server,
		[<<"select en_name,request_url,rbt_body,rbt_version from robot_info;">>]) of
	{selected,
		   	[<<"en_name">>,<<"request_url">>,<<"rbt_body">>,<<"rbt_version">>], SRes} 
		when is_list(SRes) ->
			lists:foreach(fun([Name,Url,Body,Vesion]) ->
				ets:insert(rbt_info,#rbt_info{name = Name,url = Url,body = Body,version = Vesion}) end,SRes);
	_ ->
		ok
	end.

get_subscription_url(_Server,Name) ->
	case catch ets:lookup(rbt_info,Name) of 
	[] ->
		error;
	[SI] ->
		SI#rbt_info.url
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


