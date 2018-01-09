-module(http_utils).

-include("logger.hrl").
-include("http_req.hrl").

-export([
         check_ip/1,
		 check_version/1,
         gen_result/3,
         gen_result/4,
		 verify_user_key/1,
		 verify_user_key_pv1/1,
		 do_verify_user_key/2,
		 verify_user_key_p1/1,
		 verify_user_mac_token/3,
         verify_muc_acess_limit/1,
		 to_integer/2,
		 to_binary/2,
		 to_integer/1
        ]).

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
    LimitedIPs = ejabberd_config:get_option(limited_ips, 
                                            fun(X) -> 
                                                    X
                                            end),
    {Headers, _} = cowboy_req:headers(Req),
    IP = proplists:get_value(<<"x-real-ip">>, Headers),
    ?DEBUG("the x-real-ip is ~p~n", [IP]),
    check_ip(IP, LimitedIPs).

check_ip(_IP, undefined) ->
    true;
check_ip(_IP, all) ->
    true;
check_ip(undefined, _) ->
    false;
check_ip(IP, LimitedIPs) ->
    lists:member(IP, LimitedIPs).

check_authorization(From,Req) ->
	{Headers, _} = cowboy_req:headers(Req),
	IP = proplists:get_value(<<"x-real-ip">>, Headers).

gen_result(Ret, Code, Msg) ->
    list_to_binary(rfc4627:encode({obj, [{"ret", Ret}, {"errcode", Code}, {"errmsg", Msg}]})).

gen_result(Ret, Code, Msg, Data) ->
    list_to_binary(rfc4627:encode({obj, [{"ret", Ret}, {"errcode", Code}, {"errmsg", Msg}, {"data",Data}]})).

check_version(Req) ->
	{Version,_} = cowboy_req:qs_val(<<"v">>, Req),
	{Platform,_} = cowboy_req:qs_val(<<"p">>, Req),
	case Version of
	undefined ->
		false;
	_ ->
		do_check_version(Platform,to_integer(Version))
	end.

do_check_version(undefined,_Ver) ->
	false;
do_check_version(<<"qim_windows">>,Ver) ->
	case Ver >= 10120960 of
	true ->
		true;
	_ ->
		false
	end;
do_check_version(<<"qim_linux">>,Ver) ->
	case Ver >= 10120930 of
	true ->
		true;
	_ ->
		false
	end;
do_check_version(<<"mac">>,Ver) ->
	case Ver >= 10200155 of
	true ->
		true;
	_ ->
		false
	end;
do_check_version(_,_Ver) ->
        false.
		
to_integer(V) ->
	to_integer(V,1).	

verify_user_key(Req) ->
	{User,_} = cowboy_req:qs_val(<<"u">>, Req),
	{Key,_} = cowboy_req:qs_val(<<"k">>, Req),
	do_verify_user_key(User,Key).

do_verify_user_key(undefined,_) ->
	false;
do_verify_user_key(_,undefined) ->
	false;
do_verify_user_key(User,Key) when is_binary(User),is_binary(Key)->
	case catch redis_link:hash_get(2,binary_to_list(User),binary_to_list(Key)) of
	{ok,undefined} ->
		false;
	{ok,_ } ->
		true;
	A ->
		?DEBUG("Redis A ~p ~n",[A]),
		false
	end;
do_verify_user_key(_,_) ->
	false.

verify_user_key_pv1(Req) ->
	{User,_} = cowboy_req:qs_val(<<"u">>, Req),
	{Key,_} = cowboy_req:qs_val(<<"k">>, Req),
	case {User,Key} of
	{<<"noc">>,<<"10000">>} ->
		iplimit_util:check_ips_limit(Req,<<"10">>,User) ;
	_ ->
		do_verify_user_key(User,Key)
	end.


check_http_ret_data(Data) ->
	Data.
	
		
to_integer(V,Default) when is_binary(V) ->
	try 
		binary_to_integer(V)
	catch _:_ ->
		Default
	end;	
to_integer(V,Default)  when is_list(V) ->
    try 
		list_to_integer(V)
	catch _:_ ->
	    Default
	end;
to_integer(V,_Default) when is_integer(V) ->
    V;
to_integer(_,Default) ->
	Default.

to_binary(V,Default) when is_integer(V) ->
	try 
		integer_to_binary(V)
	catch _:_ ->
		Default
	end;	
to_binary(V,Default)  when is_list(V) ->
    try 
		list_to_binary(V)
	catch _:_ ->
	    Default
	end;
to_binary(V,_Default) when is_binary(V) ->
    V;
to_binary(_,Default) ->
	Default.

verify_user_key_p1(Req) ->
	{User,_} = cowboy_req:qs_val(<<"u">>, Req),
	{Key,_} = cowboy_req:qs_val(<<"k">>, Req),
	do_verify_user_key_p1(User,Key,Req).

do_verify_user_key_p1(undefined,_,Req) ->
	do_verify_user_key_p2(Req);
do_verify_user_key_p1(_,undefined,Req) ->
	do_verify_user_key_p2(Req);
do_verify_user_key_p1(User,Key,Req) when is_binary(User),is_binary(Key)->
	case catch redis_link:hash_get(2,User,Key) of
	{ok,undefined} ->
		false;
	{ok,_ } ->
		true;
	A ->
		?DEBUG("Redis A ~p ~n",[A]),
		false
	end;
do_verify_user_key_p1(_,_,_) ->
	false.

do_verify_user_key_p2(Req) ->
	{User,_} = cowboy_req:qs_val(<<"user">>, Req),
	{Key,_} = cowboy_req:qs_val(<<"tkey">>, Req),
	case User =:= undefined orelse Key =:= undefined of
	true ->
		false;
	_ ->
		 case catch redis_link:hash_get(2,User,Key) of
		{ok,undefined} ->
			false;
		{ok,_ } ->
			true;
		_ ->
			false
		end
	end.

verify_user_mac_token(undefined,_,_) ->
	false;
verify_user_mac_token(_,undefined,_) ->
	false;
verify_user_mac_token(_,_,undefined) ->
	false;
verify_user_mac_token(User,Token,Count) when is_binary(User),is_binary(Token),is_binary(Count)->
	?DEBUG("Wlan msg User ~p ,Toke ~p,Count ~p ~n",[User,Token,Count]),
	case catch redis_link:redis_cmd(2,["HKEYS",str:concat(User,<<"_tkey">>)]) of
	{ok,undefined} ->
		false;
	{ok,L } ->
		http_check_auth:check_token_md5(Token,User,Count,L);
	A ->
		?DEBUG("Redis A ~p ~n",[A]),
		false
	end;
verify_user_mac_token(_,_,_) ->
	false.

	
verify_muc_acess_limit(Req) ->
	{User,_} = cowboy_req:qs_val(<<"u">>, Req),
	iplimit_util:check_ips_limit(Req,<<"10">>,User).
