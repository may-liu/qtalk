-module(ejb_odbc_query).

-include("logger.hrl").

-define(HOST, <<"ejb_http_server">>).

-export([escape/1]).
-export([get_department_info/0,get_vcard_version/0,get_muc_vcard_version/0,get_rbt_info/0,get_user_rbt/0,get_iplimit/0]).
-export([update_blacklist/2,update_whitelist/2,delete_whitelist/1,get_whitelist_by_user/1,insert_whitelist/2]).
-export([get_msg_info/4,get_msg_info1/4,get_msg_info2/3,get_muc_msg_info/3,get_muc_msg_info1/3,get_muc_msg_info2/2]).
-export([get_muc_msg_info3/3,get_muc_msg_info4/2,get_muc_msg_info5/3,get_muc_msg_info6/3,get_msg_info3/6,get_msg_info4/6]).
-export([update_user_mac_key/3,insert_user_mac_key/3,update_vcard_version/2,get_vcard_version_by_user/1,insert_vcard_version/3]).
-export([get_muc_vcard_info_by_name/1,update_no_insert/4,insert_muc_vcard_info/6,get_user_profile/0]).
-export([update_user_profile/2,get_profile_by_user/1,insert_user_profile/3,get_msg_info5/4]).
-export([get_msg_concats/3,get_muc_concats/3,check_rbts_auth/1]).
-export([get_chat_msg/6,get_chat_msg1/6,get_warn_msg/6,get_warn_msg1/6,get_warn_history/4]).
-export([get_chat_msg_backup/6,get_chat_msg1_backup/6]).
-export([get_msg_info_backup/4,get_msg_info1_backup/4]).
-export([get_muc_msg_info6_backup/3,get_muc_msg_info5_backup/3]).
-export([get_chat_msg2_backup/6,get_chat_msg3_backup/6,get_chat_msg5_backup/6]).
-export([get_user_register_mucs_by_version/2]).
-export([get_department_info_v1/0]).

join([], _Sep) -> [];
join([H | T], Sep) -> [H, [[Sep, X] || X <- T]].

escape($\000) -> <<"\\0">>;
escape($\n) -> <<"\\n">>;
escape($\t) -> <<"\\t">>;
escape($\b) -> <<"\\b">>;
escape($\r) -> <<"\\r">>;
escape($') -> <<"''">>;
escape($") -> <<"\\\"">>;
escape($\\) -> <<"\\\\">>;
escape(C) -> <<C>>.


%%--------------------------------------------------------------------
%% @date 2015-08
%% 获取部门信息
%%--------------------------------------------------------------------
get_department_info() ->
	pg_odbc:sql_query(?HOST,
		[<<"select dep1,dep2,dep3,dep4,dep5,username,name,department,fpinyin,spinyin from users where  hire_flag > 0  
		order by dep1,dep2,dep3,dep4,dep5;">>]).

get_department_info_v1() ->
	pg_odbc:sql_query(?HOST,
		[<<"select ps_deptid,dep1,dep2,dep3,dep4,dep5,username,name,department,fpinyin,spinyin from users where  hire_flag > 0  
        order by ps_deptid,dep1,dep2,dep3,dep4,dep5;">>]).
%%--------------------------------------------------------------------
%% @date 2015-08
%% 获取个人版本号信息
%%--------------------------------------------------------------------
get_vcard_version() ->
	pg_odbc:sql_query(?HOST,
		[<<"select username,version,url,gender from vcard_version;">>]).

%%--------------------------------------------------------------------
%% @date 2015-08
%% 获取个人信息
%%--------------------------------------------------------------------
get_user_profile() ->
	pg_odbc:sql_query(?HOST,
		[<<"select username,profile_version,mood from vcard_version;">>]).
%%--------------------------------------------------------------------
%% @date 2015-08
%% 获取muc版本号信息
%%--------------------------------------------------------------------
get_muc_vcard_version() ->
	pg_odbc:sql_query(?HOST,
		 [<<"select muc_name,show_name,muc_desc,muc_title,muc_pic,version from muc_vcard_info;">>]).

%%--------------------------------------------------------------------
%% @date 2015-08
%% 获取机器人信息
%%--------------------------------------------------------------------
get_rbt_info() ->
	pg_odbc:sql_query(?HOST,
		[<<"select en_name,request_url,rbt_body,rbt_version from robot_info;">>]).

%%--------------------------------------------------------------------
%% @date 2015-08
%% 获取用户所订阅的机器人
%%--------------------------------------------------------------------
get_user_rbt() ->
	pg_odbc:sql_query(?HOST,
		[<<"select user_name,rbt_name from robot_pubsub;">>]).

%%--------------------------------------------------------------------
%% @date 2015-08
%% 获取IP白名单
%%--------------------------------------------------------------------
get_iplimit() ->
	pg_odbc:sql_query(?HOST,
		[<<"select ip from iplimit ;">>]).

%%--------------------------------------------------------------------
%% @date 2015-08
%% 更新黑名单
%%--------------------------------------------------------------------
update_blacklist(Username,Flag) ->
	pg_odbc:sql_query(?HOST,
		[<<"update users set frozen_flag = '">>,Flag,<<"' where username = '">>,Username,<<"';">>]).

%%--------------------------------------------------------------------
%% @date 2015-08
%% 更新白名单
%%--------------------------------------------------------------------
update_whitelist(Username,Flag) ->
	pg_odbc:sql_query(?HOST,
		[<<"update white_list set single_flag = '">>,Flag,<<"' where username = '">>,Username,<<"';">>]).

%%--------------------------------------------------------------------
%% @date 2015-08
%% 删除白名单
%%--------------------------------------------------------------------
delete_whitelist(Username) ->
	pg_odbc:sql_query(?HOST,
		[<<"delete from white_list where username = '">>,Username,<<"';">>]).

%%--------------------------------------------------------------------
%% @date 2015-08
%% 添加白名单
%%--------------------------------------------------------------------
insert_whitelist(Username,Flag) ->
	pg_odbc:sql_query(?HOST,
		[<<"insert into white_list(username,single_flag) values ('">>,Username,<<"','">>,Flag,<<"');">>]).

%%--------------------------------------------------------------------
%% @date 2015-08
%% 检查用户是否存在白名单中
%%--------------------------------------------------------------------
get_whitelist_by_user(Username) ->
	pg_odbc:sql_query(?HOST,
		[<<"select username from white_list where username = '">>,Username,<<"';">>]).

%%--------------------------------------------------------------------
%% @date 2015-08
%% 获取单人聊天信息
%%--------------------------------------------------------------------
get_msg_info(From,To,Timestamp,Num) ->
	pg_odbc:sql_query(?HOST,
		[<<"select m_from,m_to,m_body,read_flag from msg_history where (m_from,m_to) in (('">>,From,
		<<"','">>,To,<<"'),('">>, To,<<"','">>,From,<<"')) and create_time > '">>,Timestamp,<<"' order by create_time asc limit ">>,Num,<<";">>]).

get_msg_info1(From,To,Timestamp,Num) ->
	pg_odbc:sql_query(?HOST,
		[<<"select m_from,m_to,m_body,read_flag from msg_history where (m_from,m_to) in (('">>,From,
		<<"','">>,To,<<"'),('">>, To,<<"','">>,From,<<"')) create_time < '">>,Timestamp,<<"' order by create_time desc limit ">>,Num,<<";">>]).

get_msg_info_backup(From,To,Timestamp,Num) ->
	pg_odbc:sql_query(?HOST,
		[<<"select m_from,m_to,m_body,read_flag from msg_history_backup where (m_from,m_to) in (('">>,From,
		<<"','">>,To,<<"'),('">>, To,<<"','">>,From,<<"')) and create_time > '">>,Timestamp,<<"' order by create_time asc limit ">>,Num,<<";">>]).

get_msg_info1_backup(From,To,Timestamp,Num) ->
	pg_odbc:sql_query(?HOST,
		[<<"select m_from,m_to,m_body,read_flag from msg_history_backup where (m_from,m_to) in (('">>,From,
		<<"','">>,To,<<"'),('">>, To,<<"','">>,From,<<"')) create_time < '">>,Timestamp,<<"' order by create_time desc limit ">>,Num,<<";">>]).

get_msg_info2(User,Timestamp,Direction) ->
	pg_odbc:sql_query(?HOST,
		[<<"select m_from,m_to,m_body,read_flag from msg_history where (m_from = '">>,User, <<"' or m_to = '">>,User,<<"') and create_time > '">>,
			Timestamp,<<"' order by id ">>,Direction,<<";">>]).

get_msg_info3(From,From_host,To,To_host,Timestamp,Num) ->
	pg_odbc:sql_query(?HOST,
		[<<"select m_from,from_host,m_to,to_host,m_body,read_flag from msg_history where (m_from,from_host,m_to,to_host) in (('">>,From,<<"','">>,
		From_host,<<"','">>,To,<<"','">>,To_host,<<"'),('">>, To,<<"','">>,To_host,<<"','">>,From,<<"','">>,From_host,<<"')) and m_timestamp > ">>,
			Timestamp,<<" order by id asc limit ">>,Num,<<";">>]).

get_msg_info4(From,From_host,To,To_host,Timestamp,Num) ->
	pg_odbc:sql_query(?HOST,
		[<<"select m_from,from_host,m_to,to_host,m_body,read_flag from msg_history where (m_from,from_host,m_to,to_host) in (('">>,From,<<"','">>,
			From_host,<<"','">>,To,<<"','">>,To_host,<<"'),('">>, To,<<"','">>,To_host,<<"','">>,From,<<"','">>,From_host,<<"')) 
		and m_timestamp < ">>,Timestamp,<<" order by id desc limit ">>,Num,<<";">>]).

get_msg_info5(User,Host,Timestamp,Direction) ->
	pg_odbc:sql_query(?HOST,
			[<<"select m_from,from_host,m_to,to_host,m_body,read_flag from msg_history where ((m_from = '">>,User,<<"' and from_host = '">>,Host,
				<<"') or (m_to = '">>,User,<<"' and to_host = '">>,Host,<<"')) and create_time > '">>,Timestamp,
					<<"' order by id ">>,Direction,<<";">>]).

%%--------------------------------------------------------------------
%%@date 2016-05
%%改进获取单人消息接口
%%--------------------------------------------------------------------
get_chat_msg(From,From_host,To,To_host,Time,Num) ->
	pg_odbc:sql_query(?HOST,
		[<<"select m_from,from_host,m_to,to_host,m_body,read_flag from msg_history where (m_from,from_host,m_to,to_host) in (('">>,From,<<"','">>,
			From_host,<<"','">>,To,<<"','">>,To_host,<<"'),('">>, To,<<"','">>,To_host,<<"','">>,From,<<"','">>,From_host,
				<<"')) and create_time > '">>,Time,<<"' order by id asc limit ">>,Num,<<";">>]).

get_chat_msg1(From,From_host,To,To_host,Time,Num) ->
	pg_odbc:sql_query(?HOST,
		[<<"select m_from,from_host,m_to,to_host,m_body,read_flag from msg_history where (m_from,from_host,m_to,to_host) in (('">>,From,<<"','">>,
			From_host,<<"','">>,To,<<"','">>,To_host,<<"'),('">>, To,<<"','">>,To_host,<<"','">>,From,<<"','">>,From_host,
			<<"')) and create_time < '">>,Time,<<"' order by id desc limit ">>,Num,<<";">>]).

get_chat_msg_backup(From,From_host,To,To_host,Time,Num) ->
	pg_odbc:sql_query(?HOST,
		[<<"select m_from,from_host,m_to,to_host,m_body,read_flag from msg_history_backup where (m_from,from_host,m_to,to_host) in (('">>,
			From,<<"','">>,	From_host,<<"','">>,To,<<"','">>,To_host,<<"'),('">>, To,<<"','">>,To_host,<<"','">>,From,<<"','">>,From_host,
				<<"')) and create_time > '">>,Time,<<"' order by id asc limit ">>,Num,<<";">>]).

get_chat_msg2_backup(From,From_host,To,To_host,Time,Num) ->
	pg_odbc:sql_query(?HOST,
		[<<"with result as ( select id, m_from,from_host,m_to,to_host,m_body,read_flag from msg_history_backup where (m_from,from_host,m_to,to_host) in (('">>,
			From,<<"','">>,	From_host,<<"','">>,To,<<"','">>,To_host,<<"'),('">>, To,<<"','">>,To_host,<<"','">>,From,<<"','">>,From_host,	<<"')) and create_time > '">>,Time,<<"') select m_from,from_host,m_to,to_host,m_body,read_flag from result order by id asc limit ">>,Num,<<";">>]).



get_chat_msg1_backup(From,From_host,To,To_host,Time,Num) ->
	pg_odbc:sql_query(?HOST,
		[<<"select m_from,from_host,m_to,to_host,m_body,read_flag from msg_history_backup where (m_from,from_host,m_to,to_host) in (('">>,
			From,<<"','">>,From_host,<<"','">>,To,<<"','">>,To_host,<<"'),('">>, To,<<"','">>,To_host,<<"','">>,From,<<"','">>,From_host,
				<<"')) and create_time < '">>,Time,<<"' order by id desc limit ">>,Num,<<";">>]).

get_chat_msg3_backup(From,From_host,To,To_host,Time,Num) ->
	pg_odbc:sql_query(?HOST,
		[<<"with result as ( select id, m_from,from_host,m_to,to_host,m_body,read_flag from msg_history_backup where (m_from,from_host,m_to,to_host) in (('">>,
			From,<<"','">>,	From_host,<<"','">>,To,<<"','">>,To_host,<<"'),('">>, To,<<"','">>,To_host,<<"','">>,From,<<"','">>,From_host,	<<"')) and create_time < '">>,Time,<<"') select m_from,from_host,m_to,to_host,m_body,read_flag from result order by id desc limit ">>,Num,<<";">>]).

get_chat_msg5_backup(From,From_host,To,To_host,Time,Num) ->
	pg_odbc:sql_query(?HOST,
		[<<"with result as ( select id, m_from,from_host,m_to,to_host,m_body,read_flag from msg_history_backup_2016 where (m_from,from_host,m_to,to_host) in (('">>,
			From,<<"','">>,	From_host,<<"','">>,To,<<"','">>,To_host,<<"'),('">>, To,<<"','">>,To_host,<<"','">>,From,<<"','">>,From_host,	<<"')) and create_time < '">>,Time,<<"') select m_from,from_host,m_to,to_host,m_body,read_flag from result order by id desc limit ">>,Num,<<";">>]).


get_warn_msg(From,From_host,To,To_host,Time,Num) ->
	pg_odbc:sql_query(?HOST,
		[<<"select m_from,from_host,m_to,to_host,m_body,read_flag from warn_msg_history where (m_from,from_host,m_to,to_host) in (('">>,
			From,<<"','">>,From_host,<<"','">>,To,<<"','">>,To_host,<<"')) and create_time > '">>,Time,
				<<"' order by id asc limit ">>,Num,<<";">>]).

get_warn_msg1(From,From_host,To,To_host,Time,Num) ->
	pg_odbc:sql_query(?HOST,
		[<<"select m_from,from_host,m_to,to_host,m_body,read_flag from warn_msg_history where (m_from,from_host,m_to,to_host) in (('">>,
			From,<<"','">>,From_host,<<"','">>,To,<<"','">>,To_host,<<"')) and create_time < '">>,Time,
				<<"' order by id desc limit ">>,Num,<<";">>]).

get_warn_history(User,Host,Timestamp,Direction) ->
	pg_odbc:sql_query(?HOST,
			[<<"select m_from,from_host,m_to,to_host,m_body,read_flag from warn_msg_history where ((m_from = '">>,User,<<"' and from_host = '">>,
			Host,<<"') or (m_to = '">>,User,<<"' and to_host = '">>,Host,<<"')) and create_time > '">>,Timestamp,
					<<"' order by id ">>,Direction,<<";">>]).
%%--------------------------------------------------------------------
%% @date 2015-08
%% 获取聊天聊天信息
%%--------------------------------------------------------------------
get_muc_msg_info(From,Timestamp,Num) ->
	Time = ejb_public:format_time(Timestamp),
	pg_odbc:sql_query(?HOST,
		[<<"select muc_room_name,nick,packet from muc_room_history where muc_room_name = '">>,From,
			 <<"' and create_time > '">>,Time , <<"' order by create_time asc limit ">>,Num,<<";">>]).

get_muc_msg_info1(From,Timestamp,Num) -> 
	Time = ejb_public:format_time(Timestamp),
	pg_odbc:sql_query(?HOST,
		[<<"select muc_room_name,nick,packet from muc_room_history where muc_room_name = '">>,From,
			 <<"' and create_time < '">>,Time , <<"' order by create_time desc limit ">>,Num,<<";">>]).

get_muc_msg_info2(From,Timestamp) -> 
	Time = ejb_public:format_time(Timestamp),
	pg_odbc:sql_query(?HOST,
		[<<"select muc_room_name,nick,packet from muc_room_history where muc_room_name = '">>,From,
			 <<"' and create_time > '">>,Time , <<"' order by create_time desc; ">>]).

get_muc_msg_info3(From,Timestamp,Num) -> 
	Time = ejb_public:format_time(Timestamp),
	pg_odbc:sql_query(?HOST,
		[<<"select muc_room_name,nick,host,packet from muc_room_history where muc_room_name = '">>,From,
			 <<"' and create_time < '">>,Time , <<"' order by create_time desc limit ">>,Num,<<";">>]).

get_muc_msg_info4(From,Timestamp) -> 
	Time = ejb_public:format_time(Timestamp),
	pg_odbc:sql_query(?HOST,
		[<<"select muc_room_name,nick,host,packet from muc_room_history where muc_room_name = '">>,From,
			 <<"' and create_time > '">>,Time , <<"' order by create_time asc; ">>]).

get_muc_msg_info5(From,Timestamp,Num) ->
	Time = ejb_public:format_time(Timestamp),
	pg_odbc:sql_query(?HOST,
		[<<"select muc_room_name,nick,host,packet from muc_room_history where muc_room_name = '">>,From,
			 <<"' and create_time > '">>,Time , <<"' order by create_time asc limit ">>,Num,<<";">>]).

get_muc_msg_info6(From,Timestamp,Num) -> 
	Time = ejb_public:format_time(Timestamp),
	pg_odbc:sql_query(?HOST,
		[<<"select muc_room_name,nick,host,packet from muc_room_history where muc_room_name = '">>,From,
			 <<"' and create_time < '">>,Time , <<"' order by create_time desc limit ">>,Num,<<";">>]).

get_muc_msg_info5_backup(From,Timestamp,Num) ->
	Time = ejb_public:format_time(Timestamp),
	pg_odbc:sql_query(?HOST,
		[<<"select muc_room_name,nick,host,packet from muc_room_history_backup where muc_room_name = '">>,From,
			 <<"' and create_time > '">>,Time , <<"' order by create_time asc limit ">>,Num,<<";">>]).

get_muc_msg_info6_backup(From,Timestamp,Num) -> 
	Time = ejb_public:format_time(Timestamp),
	pg_odbc:sql_query(?HOST,
		[<<"select muc_room_name,nick,host,packet from muc_room_history_backup where muc_room_name = '">>,From,
			 <<"' and create_time < '">>,Time , <<"' order by create_time desc limit ">>,Num,<<";">>]).


%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @date 2015-08
%% 更新用户的mackey
%%--------------------------------------------------------------------

update_user_mac_key(User,Host,Mackey) ->
	pg_odbc:sql_query(?HOST,
			[<<"update user_mac_key set mac_key  = '">>,Mackey ,<<"' where user_name = '">>,User,<<"' and host = '">>,Host,<<"';">>]).


%%--------------------------------------------------------------------
%% @date 2015-08
%% 插入用户的mackey
%%--------------------------------------------------------------------
insert_user_mac_key(Username,Host,Mackey) ->
	pg_odbc:sql_query(?HOST,
				[<<"insert into user_mac_key(user_name,host,mac_key) values ('">>,
					Username,<<"','">>,Host,<<"','">>,Mackey,<<"');">>]).

%%--------------------------------------------------------------------
%% @date 2015-08
%% 更新用户的版本号
%%--------------------------------------------------------------------
update_vcard_version(User,Url) ->
	pg_odbc:sql_query(?HOST,
            [<<"update vcard_version set version = version + 1,url = '">>, Url ,<<"' where username = '">>,User,<<"';">>]).

%%--------------------------------------------------------------------
%% @date 2018-01
%% 更新用户个人信息
%%--------------------------------------------------------------------
update_user_profile(User,Mood) ->
	pg_odbc:sql_query(?HOST,
            [<<"update vcard_version set profile_version = profile_version + 1,mood = '">>, Mood ,<<"' where username = '">>,User,<<"';">>]).

%%--------------------------------------------------------------------
%% @date 2015-08
%% 通过用户获取版本号信息
%%--------------------------------------------------------------------
get_vcard_version_by_user(User) ->
	pg_odbc:sql_query(?HOST,
            [<<"select version from vcard_version where username = '">>,User,<<"';">>]).

%%--------------------------------------------------------------------
%% @date 2016-01
%% 通过用户获取用户信息版本号
%%--------------------------------------------------------------------
get_profile_by_user(User) ->
	pg_odbc:sql_query(?HOST,
            [<<"select profile_version from vcard_version where username = '">>,User,<<"';">>]).

%%--------------------------------------------------------------------
%% @date 2015-08
%% 插入用户版本号
%%--------------------------------------------------------------------
insert_vcard_version(Username,Version,Url) ->
	pg_odbc:sql_query(?HOST,
            [<<"insert into vcard_version(username,version,url) values ('">>,Username,<<"',">>,Version,<<",'">>,Url,<<"');">>]).

%%--------------------------------------------------------------------
%% @date 2016-01
%% 插入用户信息
%%--------------------------------------------------------------------
insert_user_profile(Username,Version,Mood) ->
	pg_odbc:sql_query(?HOST,
            [<<"insert into vcard_version(username,profile_version,mood) values ('">>,Username,<<"',">>,Version,<<",'">>,Mood,<<"');">>]).

%%--------------------------------------------------------------------
%% @date 2015-08
%% 通过聊天室名称获取聊天室版本号
%%--------------------------------------------------------------------
get_muc_vcard_info_by_name(Mucname) ->
	pg_odbc:sql_query(?HOST,
			[<<"select show_name,muc_desc,muc_title,muc_pic,version from muc_vcard_info where muc_name = '">>,Mucname,<<"';">>]).

%%--------------------------------------------------------------------
%% @date 2015-08
%% 动态更新无插入
%%--------------------------------------------------------------------
update_no_insert(Table, Fields, Vals, Where) -> 
	UPairs = lists:zipwith(fun (A, B) ->
		<<A/binary, "='", B/binary, "'">> end, Fields, Vals),
	case pg_odbc:sql_query_t([<<"update ">>, Table,<<" set ">>, join(UPairs, <<", ">>), <<" where ">>, Where, <<";">>]) of
	{updated, 1} -> ok;
	Reason ->
		Reason
	end.

%%--------------------------------------------------------------------
%% @date 2015-08
%% 插入用户版本号信息
%%--------------------------------------------------------------------
insert_muc_vcard_info(Mucname,Nick,Desc,Title,Pic,Version) ->
	pg_odbc:sql_query(?HOST,
		[<<"insert into muc_vcard_info(muc_name,show_name,muc_desc,muc_title,muc_pic,version) values ('">>,Mucname,<<"','">>,
			Nick,<<"','">>,Desc,<<"','">>,Title,<<"','">>,Pic,<<"','">>,Version,<<"');">>]).


get_msg_concats(User,Limit,Domain) ->
    pg_odbc:sql_query(?HOST,
			[<<"select u from (select case when m_from = '">>,User,
			<<"' then m_to else m_from end as u,max(create_time) as m_time from msg_history where ((m_from = '">>,User,
			<<"' or m_to='">>,User,<<"') and from_host = '">>,Domain,<<"' and to_host = '">>,Domain,
			<<"') group by m_from,m_to ) tab order by tab.m_time desc limit ">>,Limit,<<";">>]).

get_muc_concats(User,Limit,Domain) ->
    pg_odbc:sql_query(?HOST,
		   [<<"select muc_name from  (select muc_room_name as muc_name,max(create_time) as m_time from muc_room_history where muc_room_name in (select muc_name from muc_room_users where username = '">>,User,<<"') group by muc_room_name) tab order by m_time desc limit ">>,Limit,<<" ;">>]).     


check_rbts_auth(Rbt) ->
    pg_odbc:sql_query(?HOST,
			            [<<"select password from robot_info where en_name = '">>,Rbt,<<"';">>]).

get_user_register_mucs_by_version(User,Version) ->
    pg_odbc:sql_query(?HOST,
            [<<"select muc_name,domain,extract(epoch from date_trunc('MS', created_at))*1000,registed_flag  
                    from user_register_mucs where created_at > ">>,ejb_public:pg2timestamp(Version), 
                      <<" and username = '">>,User,<<"';">>]).
