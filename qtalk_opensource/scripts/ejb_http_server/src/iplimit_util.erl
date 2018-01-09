-module(iplimit_util).

-export([
    create_ets/0,
    update_iplimit/0,
    check_ip/1,
	update_ip_limit/0,
	check_ips_limit/3,
	check_ips_limit/2,
    check_muc_ip_limit/2,
    check_level_ip_limit/3,
    check_ips_limit_with_args/4
    ]).

-include("logger.hrl").
-include("http_req.hrl").
-include("ejb_http_server.hrl").

-record(ip_limit,{priority,ips = []}).


create_ets() ->
%%    catch ets:new(iplimit, [set, named_table, public, {keypos, 2},{write_concurrency, true}, {read_concurrency, true}]).
    catch ets:new(iplimit, [set, named_table, public, {keypos, 1},{write_concurrency, true}, {read_concurrency, true}]),
    catch ets:new(ip_limit, [set, named_table, public, {keypos, 2},{write_concurrency, true}, {read_concurrency, true}]).

%%--------------------------------------------------------------------
%% @date 2015-07-01
%% 通过使用update_iplimit(<<"host">>)来刷新ip限制列表的缓存
%%--------------------------------------------------------------------
update_iplimit() ->
	ets:delete_all_objects(iplimit),
	case catch ejb_odbc_query:get_iplimit() of
	{selected,_ ,SRes}
	when is_list(SRes) ->
		lists:foreach(fun([IP]) ->
			ets:insert(iplimit,{IP}) end,SRes);
	_ ->
		ok
	end.

update_ip_limit() ->
	ets:delete_all_objects(ip_limit),
	case catch pg_odbc:sql_query(<<"ejb_http_server">>,[<<"select priority,ip,name from iplimit ;">>]) of
	{selected,_ ,SRes}
	when is_list(SRes) ->
		lists:foreach(fun([Priority,IP,Name]) ->
			case catch ets:lookup(ip_limit,Priority) of
			[] ->
				catch ets:insert(ip_limit,#ip_limit{priority = Priority,ips = [{IP,Name}]});
			[Ips] when is_record(Ips,ip_limit) ->
				case lists:member({IP,Name},Ips#ip_limit.ips) of
				true ->
					ok;
				_ ->
					catch ets:insert(ip_limit,#ip_limit{priority = Ips#ip_limit.priority,ips = lists:append(Ips#ip_limit.ips,[{IP,Name}])})
				end;
			_ ->
				ok
			end end,SRes);
	_ ->
		ok
	end.
%%--------------------------------------------------------------------
%% @date 2015-06-02
%% 该函数取出HTTP Headers中的X-Real_IP字段，然后查看该IP是否存在于配
%% 置中，存在返回true，否则返回false
%% 我们需要在ejabberd.yml中添加如下配置：
%% limited_ips:
%%   - "192.168.168.1"
%%   - "192.168.168.2"
%% 或者
%% limited_ips: all
%%
%% 如果没有配置，则允许所有用户访问
%% 我们可以通过该函数来判断访问来源的有效性
%%--------------------------------------------------------------------

check_ips_limit_with_args(Req,Priority,From,Args) ->
        case catch  ets:lookup(ip_limit,Priority) of
        [IPs] when is_record(IPs,ip_limit) ->
                {Headers, _} = cowboy_req:headers(Req),
                IP = proplists:get_value(<<"x-real-ip">>, Headers),
                check_ip_limit_with_args(IP,From,IPs#ip_limit.ips,Args);
        _ ->
                false
        end.

check_ip_limit_with_args(_Ip,<<"">>,_,_) ->
        true;
check_ip_limit_with_args(_Ip,_From,[],_) ->
        true;
check_ip_limit_with_args(Ip,From,LimitedIPs,Args) when is_list(LimitedIPs)->
        case lists:member({Ip,From},LimitedIPs) of
        true ->
                true;
        _ ->
                case lists:member({Ip,<<"ALL">>},LimitedIPs) of
                true ->
                        ?DEBUG("http sendmessage All ip ~p, From ~p , Args ~p ~n",[Ip,From,Args]),
                        true;
                _ ->
                        false
                end
        end;
check_ip_limit_with_args(_Ip,_From,_LimitedIPs,_) ->
        true.

check_ips_limit(Req,Priority) ->
	check_ips_limit(Req,Priority,<<"">>).

check_ips_limit(Req,Priority,From) ->
	case catch  ets:lookup(ip_limit,Priority) of
	[IPs] when is_record(IPs,ip_limit) ->
		{Headers, _} = cowboy_req:headers(Req),
		IP = proplists:get_value(<<"x-real-ip">>, Headers),
		?DEBUG("IP ~p ,iplimit ~p ~n",[IP,From]),
		check_ip_limit(IP,From,IPs#ip_limit.ips);
	_ ->
		true
	end.

check_ip_limit(_Ip,<<"">>,_) ->
	true;
check_ip_limit(_Ip,_From,[]) ->
	true;
check_ip_limit(Ip,From,LimitedIPs) when is_list(LimitedIPs)->
	lists:member({Ip,From},LimitedIPs) orelse lists:member({Ip,<<"ALL">>},LimitedIPs);
check_ip_limit(_Ip,_From,_LimitedIPs) ->
	true.
		
check_ip(Req) ->
    LimitedIPs = ets:tab2list(iplimit),
    {Headers, _} = cowboy_req:headers(Req),
    IP = proplists:get_value(<<"x-real-ip">>, Headers),
    ?DEBUG("IP ~p ~n",[IP]),
    check_ip(IP, LimitedIPs).

check_ip(_IP, []) ->
    true;
check_ip(undefined, _) ->
    false;
check_ip(IP, LimitedIPs) ->
    lists:member({IP}, LimitedIPs).

check_muc_ip_limit(Body,Req) ->
	case rfc4627:decode(Body) of
	{ok,[{obj,Args}],[]}  ->
		User = proplists:get_value("muc_owner",Args),
        iplimit_util:check_ips_limit(Req,<<"1">>,User);
    _ ->
        false
    end.


check_level_ip_limit(User,Key,Req) ->
    iplimit_util:check_ips_limit(Req,Key,User).
