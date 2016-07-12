%%============================================
%%ip限制模块
%%============================================
-module(iplimit_util).

-export([
    create_ets/0,
    update_iplimit/1,
    check_ip/1
    ]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("http_req.hrl").

create_ets() ->
    ?DEBUG("create ets iplimit~n", []),
    catch ets:new(iplimit, [set, named_table, public, {keypos, 1},{write_concurrency, true}, {read_concurrency, true}]),
    catch ets:new(iplimit, [set, named_table, public, {keypos, 1},{write_concurrency, true}, {read_concurrency, true}]).

%%--------------------------------------------------------------------
%% @date 2015-07-01
%% 通过使用update_iplimit(<<"host">>)来刷新ip限制列表的缓存
%%--------------------------------------------------------------------
update_iplimit(LServer) ->
    ?DEBUG("ipdate iplimit ~n", []),
	ets:delete_all_objects(iplimit),
	case catch odbc_queries:get_iplimit(jlib:nameprep(LServer)) of
	{selected,[<<"ip">>],SRes}
	when is_list(SRes) ->
		lists:foreach(fun([IP]) ->
			ets:insert(iplimit,{IP}) end,SRes);
	A ->
		?DEBUG("Get Unknown iplimit res ~p ~n",[A])
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
check_ip(Req) ->
    LimitedIPs = ets:tab2list(iplimit),
    {Headers, _} = cowboy_req:headers(Req),
    IP = proplists:get_value(<<"x-real-ip">>, Headers),
    ?DEBUG("the x-real-ip is ~p~n", [IP]),
    check_ip(IP, LimitedIPs).

check_ip(_IP, []) ->
    true;
check_ip(undefined, _) ->
    false;
check_ip(IP, LimitedIPs) ->
    lists:member({IP}, LimitedIPs).

