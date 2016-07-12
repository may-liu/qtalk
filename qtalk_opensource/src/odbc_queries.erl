%%%----------------------------------------------------------------------
%%% File    : odbc_queries.erl
%%% Author  : Mickael Remond <mremond@process-one.net>
%%% Purpose : ODBC queries dependind on back-end
%%% Created : by Mickael Remond <mremond@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2014   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

-module(odbc_queries).

-author("mremond@process-one.net").

-export([get_db_type/0, update_t/4, insert_t/3, sql_transaction/2,update_no_insert/4,insert_t_no_single_quotes/3,
	 get_last/2, set_last_t/4, del_last/2, get_password/2, get_blacklist/1,
	 set_password_t/3, add_user/3, del_user/2,insert_msg/6,insert_day_online/4,
	 del_user_return_password/3, list_users/1, list_users/2,insert_muc_msg/7,insert_muc_users/5,insert_muc_users_sub_push/5,
	 users_number/1, users_number/2, add_spool_sql/3,add_spool_sql_no_notice/3,
	 get_muc_history/3,get_muc_users/3,del_muc_user/4,get_user_mucs/3,del_muc_users/3,
	 clear_muc_users/1,clear_muc_spool/1,clear_spool/1,get_muc_msg_last_timestamp/2,del_muc_vcard_info/2,get_muc_last_name_and_time/1,
	 add_spool/2, get_and_del_spool_msg_t/2, del_spool_msg/2,get_muc_msg_info/4,get_muc_msg_info1/4,get_last_muc_msg_time/3,
	 get_roster/2, get_roster_jid_groups/2,get_roster_info/1,get_department_info/1,
	 insert_muc_last/3,delete_muc_last/2,update_muc_last/4,update_muc_last_create_time/3,
	 get_roster_groups/3, del_user_roster_t/2,get_msg_info/5,get_msg_info1/5,get_msg_info2/4,
	 get_roster_by_jid/3, get_rostergroup_by_jid/3,get_user_suoxie/1,get_white_list_users/1,get_iplimit/1, insert_iplimit/3,delete_iplimit/2,
	 del_roster/3, del_roster_sql/2, update_roster/5,get_user_muc_subscribe/2,del_user_muc_subscribe/3,add_user_muc_subscribe/3,
	 update_roster_sql/4, roster_subscribe/4,insert_subscribe_msg/5,del_muc_spool/2,add_spool_away/5,
	 get_subscription/3, set_private_data/4,get_concats/2,get_muc_concats/2,
	 get_use_registed_muc_num/2,update_whitelist/2,delete_whitelist/2,clear_user_mac_key/1,
	 get_muc_vcard_info/1,set_muc_vcard_info/6,insert_muc_vcard_info/7,get_muc_vcard_version/2,get_muc_vcard_info_by_name/2,
	 set_private_data_sql/3, get_private_data/3, get_private_data/2,
	 insert_user_mac_key/4,update_user_mac_key/4,
	 insert_vcard_version/4,update_vcard_version/3,get_vcard_version/1,get_vcard_version_by_user/2,
	 del_user_private_storage/2, get_default_privacy_list/2,
	 get_default_privacy_list_t/1, get_privacy_list_names/2,
	 get_privacy_list_names_t/1, get_privacy_list_id/3,
	 get_privacy_list_id_t/2, get_privacy_list_data/3,
	 get_privacy_list_data_by_id/2, get_privacy_list_data_t/2,
	 get_privacy_list_data_by_id_t/1,
	 set_default_privacy_list/2,
	 unset_default_privacy_list/2,
	 remove_privacy_list/2,
	 add_privacy_list/2,
	 set_privacy_list/2,
	 del_privacy_lists/3,
	 set_vcard/26,
	 get_vcard/2,
	 escape/1,
	 insert_sql/2,
	 get_mac_users/1,
	 count_records_where/3,
	 get_roster_version/2,
	 set_roster_version/2,
     update_blacklist/2]).

%% We have only two compile time options for db queries:
%-define(generic, true).
%-define(mssql, true).
-ifndef(mssql).

-undef(generic).

-define(generic, true).

-endif.

-include("ejabberd.hrl").
-include("logger.hrl").

%% Almost a copy of string:join/2.
%% We use this version because string:join/2 is relatively
%% new function (introduced in R12B-0).
join([], _Sep) -> [];
join([H | T], Sep) -> [H, [[Sep, X] || X <- T]].

%% -----------------
%% Generic queries
-ifdef(generic).

get_db_type() -> generic.

%% Safe atomic update.
update_t(Table, Fields, Vals, Where) ->
    UPairs = lists:zipwith(fun (A, B) ->
				   <<A/binary, "='", B/binary, "'">>
			   end,
			   Fields, Vals),
    case ejabberd_odbc:sql_query_t([<<"update ">>, Table,
				    <<" set ">>, join(UPairs, <<", ">>),
				    <<" where ">>, Where, <<";">>])
	of
      {updated, 1} -> ok;
      _ ->
	  ejabberd_odbc:sql_query_t([<<"insert into ">>, Table,
				     <<"(">>, join(Fields, <<", ">>),
				     <<") values ('">>, join(Vals, <<"', '">>),
				     <<"');">>])
    end.

insert_t(Table, Fields, Vals) ->
	  ejabberd_odbc:sql_query_t([<<"insert into ">>, Table,
				     <<"(">>, join(Fields, <<", ">>),
				     <<") values ('">>, join(Vals, <<"', '">>),
				     <<"');">>]).

insert_t_no_single_quotes(Table, Fields, Vals) ->
	  ejabberd_odbc:sql_query_t([<<"insert into ">>, Table,
				     <<"(">>, join(Fields, <<", ">>),
				     <<") values (">>, join(Vals, <<" , ">>),
				     <<");">>]).

get_concats(LServer,Username) ->
    ejabberd_odbc:sql_query(LServer,
                [<<"select u from (select case when m_from = '">>,Username,
		                <<"' then m_to else m_from end as u,max(m_timestamp) as m_time from msg_history where m_from = '">>,Username,
						                <<"' or m_to='">>,Username,<<"' group by m_from,m_to ) tab order by tab.m_time desc ;">>]).	

get_muc_concats(LServer,Username) ->
    ejabberd_odbc:sql_query(LServer,
			[<<"select muc_name from  (select muc_room_name as muc_name,max(m_timestamp) as m_time from muc_room_history where muc_room_name in (select muc_name from muc_room_users where username = '">>,Username,<<"') group by muc_room_name) tab order by m_time desc ">>]).

update_no_insert(Table, Fields, Vals, Where) ->
    UPairs = lists:zipwith(fun (A, B) ->
				   <<A/binary, "='", B/binary, "'">>
			   end,
			   Fields, Vals),
    case ejabberd_odbc:sql_query_t([<<"update ">>, Table,
				    <<" set ">>, join(UPairs, <<", ">>),
				    <<" where ">>, Where, <<";">>])
	of
      {updated, 1} -> ok;
      Reason ->
	  		Reason
    end.
update(LServer, Table, Fields, Vals, Where) ->
    UPairs = lists:zipwith(fun (A, B) ->
				   <<A/binary, "='", B/binary, "'">>
			   end,
			   Fields, Vals),
    case ejabberd_odbc:sql_query(LServer,
				 [<<"update ">>, Table, <<" set ">>,
				  join(UPairs, <<", ">>), <<" where ">>, Where,
				  <<";">>])
	of
      {updated, 1} -> ok;
      _ ->
	  ejabberd_odbc:sql_query(LServer,
				  [<<"insert into ">>, Table, <<"(">>,
				   join(Fields, <<", ">>), <<") values ('">>,
				   join(Vals, <<"', '">>), <<"');">>])
    end.

%% F can be either a fun or a list of queries
%% TODO: We should probably move the list of queries transaction
%% wrapper from the ejabberd_odbc module to this one (odbc_queries)
sql_transaction(LServer, F) ->
    ejabberd_odbc:sql_transaction(LServer, F).

get_last(LServer, Username) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select seconds, state from last where "
			       "username='">>,
			     Username, <<"'">>]).

set_last_t(LServer, Username, Seconds, State) ->
    update(LServer, <<"last">>,
	   [<<"username">>, <<"seconds">>, <<"state">>],
	   [Username, Seconds, State],
	   [<<"username='">>, Username, <<"'">>]).

del_last(LServer, Username) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"delete from last where username='">>, Username,
			     <<"'">>]).
get_blacklist(LServer) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select username from users where frozen_flag='1';">>]).

get_mac_users(LServer) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select user_name from user_mac_key where mac_key <> '(null)';">>]).

get_password(LServer, Username) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select password from users where username='">>,
			     Username, <<"';">>]).

set_password_t(LServer, Username, Pass) ->
    ejabberd_odbc:sql_transaction(LServer,
				  fun () ->
					  update_t(<<"users">>,
						   [<<"username">>,
						    <<"password">>],
						   [Username, Pass],
						   [<<"username='">>, Username,
						    <<"'">>])
				  end).

add_user(LServer, Username, Pass) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"insert into users(username, password) "
			       "values ('">>,
			     Username, <<"', '">>, Pass, <<"');">>]).

insert_user_mac_key(LServer,Username,Host,Mackey) ->
	ejabberd_odbc:sql_query(LServer,
				[<<"insert into user_mac_key(user_name,host,mac_key) values ('">>,
					Username,<<"','">>,Host,<<"','">>,Mackey,<<"');">>]).

insert_sql(LServer,Sql) ->
	ejabberd_odbc:sql_query(LServer,[Sql]).

insert_vcard_version(LServer,Username,Version,Url) ->
	ejabberd_odbc:sql_query(LServer,
				[<<"insert into vcard_version(username,version,url) values ('">>,
					Username,<<"',">>,Version,<<",'">>,Url,<<"');">>]).

insert_msg(LServer,From,To,Body,Msg_id,Time) ->
	ejabberd_odbc:sql_query(LServer,
				[<<"insert into msg_history(m_from,m_to,m_body,msg_id,m_timestamp)"
					"values ('">>,
				From,<<"','">>,To,<<"','">>,Body,<<"','">>,Msg_id,<<"','">>,integer_to_list(Time),<<"');">>]).

insert_day_online(LServer,Username,Date,OnlineTime) ->
	ejabberd_odbc:sql_query(LServer,
				[<<"insert into day_online_time(username,date_time,online_time) values ('">>,
				Username,<<"','">>,Date,<<"',">>,integer_to_list(OnlineTime),<<");">>]).

insert_muc_msg(LServer,Muc_name,Nick,Packet,Have_sub,Size,Time) ->
	ejabberd_odbc:sql_query(LServer,
				[<<"insert into muc_room_history(muc_room_name,nick,packet,have_subject,size,m_timestamp) values ('">>,
			Muc_name,<<"','">>,Nick,<<"','">>,Packet,<<"',">>,atom_to_list(Have_sub),<<",'">>,
			integer_to_list(Size),<<"','">>,integer_to_list(Time),<<"');">>]).

insert_muc_users(LServer,Tabname,Muc_name,Username,Host) ->
	ejabberd_odbc:sql_query(LServer,
				[<<"insert into ">>,Tabname,<<" (muc_name,username,host) values ('">>,
			Muc_name,<<"','">>,Username,<<"','">>,Host,<<"');">>]).

insert_muc_users_sub_push(LServer,Tabname,Muc_name,Username,Host) ->
	ejabberd_odbc:sql_query(LServer,
				[<<"insert into ">>,Tabname,<<" (muc_name,username,host,subscribe_flag ) values ('">>,
			Muc_name,<<"','">>,Username,<<"','">>,Host,<<"','1');">>]).

insert_subscribe_msg(LServer,SubUsers,Mucname,Nick,Msg) ->
	ejabberd_odbc:sql_query(LServer,
				[<<"select spilt_users_to_insert_xml('">>,SubUsers,<<"','">>,Mucname,<<"','">>,Nick,<<"','">>,Msg,<<"');">>]).

del_user(LServer, Username) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"delete from users where username='">>, Username,
			     <<"';">>]).

clear_muc_users(LServer) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"delete from muc_room_users a where a.username not in (select username from users);">>]).

clear_spool(LServer) ->
	ejabberd_odbc:sql_query(LServer,
			[<<"delete from spool where username not in (select username from users);">>]).

clear_user_mac_key(LServer) ->
	ejabberd_odbc:sql_query(LServer,
			[<<"delete from user_mac_key where user_name not in (select username from users);">>]).

clear_muc_spool(LServer) ->
	 ejabberd_odbc:sql_query(LServer,
			 [<<"delete from muc_spool where username not in (select username from users);">>]).
	
get_muc_msg_last_timestamp(LServer, Mucname) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select m_timestamp from muc_room_history where muc_room_name = '">>,  Mucname, <<"' order by m_timestamp desc limit 1">>]).

get_muc_last_name_and_time(LServer) ->
	ejabberd_odbc:sql_query(LServer,
				[<<"select muc_name,create_time from muc_last">>]).


insert_muc_last(LServer,Mucname,Time) ->
	ejabberd_odbc:sql_query(LServer,
				[<<"insert into muc_last (muc_name,create_time) values ('">>,
			Mucname,<<"','">>,Time,<<"');">>]).

delete_muc_last(LServer,Mucname) ->
	ejabberd_odbc:sql_query(LServer,
			[<<"delete from muc_last where muc_name = '">>,Mucname,<<"';">>]).

update_user_mac_key(LServer,User,Host,Mackey) ->
	ejabberd_odbc:sql_query(LServer,
			[<<"update user_mac_key set mac_key  = '">>,Mackey ,<<"' where user_name = '">>,User,<<"' and host = '">>,Host,<<"';">>]).

update_vcard_version(LServer,User,Url) ->
	ejabberd_odbc:sql_query(LServer,
			[<<"update vcard_version set version = version + 1,url = '">>, Url ,<<"' where username = '">>,User,<<"';">>]).

get_vcard_version(LServer) ->
	ejabberd_odbc:sql_query(LServer,
			[<<"select username,version,url from vcard_version;">>]).

get_vcard_version_by_user(LServer,User) ->
	ejabberd_odbc:sql_query(LServer,
			[<<"select version from vcard_version where username = '">>,User,<<"';">>]).

get_muc_vcard_info(LServer) ->
	ejabberd_odbc:sql_query(LServer,
			[<<"select muc_name,show_name,muc_desc,muc_title,muc_pic,version from muc_vcard_info;">>]).

get_muc_vcard_info_by_name(LServer,Mucname) ->
	ejabberd_odbc:sql_query(LServer,
			[<<"select show_name,muc_desc,muc_title,muc_pic,version from muc_vcard_info where muc_name = '">>,Mucname,<<"';">>]).

get_user_muc_subscribe(LServer,Mucname) ->
	ejabberd_odbc:sql_query(LServer,
			[<<"select username from muc_room_users where muc_name = '">>,Mucname,<<"' and  subscribe_flag = '1';">>]).

del_user_muc_subscribe(LServer,Mucname,Username) ->
	ejabberd_odbc:sql_query(LServer,
			[<<"update muc_room_users set subscribe_flag = '0' where muc_name = '">>,Mucname,<<"' and username = '">>,Username,<<"';">>]).

add_user_muc_subscribe(LServer,Mucname,Username) ->
	ejabberd_odbc:sql_query(LServer,
			[<<"update muc_room_users set subscribe_flag = '1' where muc_name = '">>,Mucname,<<"' and username = '">>,Username,<<"';">>]).

del_muc_spool(LServer,Username) ->
	ejabberd_odbc:sql_query(LServer,
			[<<"delete from muc_spool where username = '">>,Username,<<"';">>]).

delete_whitelist(LServer,Username) ->
	ejabberd_odbc:sql_query(LServer,
			[<<"delete from white_list where username = '">>,Username,<<"';">>]).

get_muc_vcard_version(LServer,Mucname) ->
	ejabberd_odbc:sql_query(LServer,
			[<<"select show_name,muc_desc,muc_title,muc_pic,version from muc_vcard_info where muc_name = '">>,Mucname,<<"';">>]).

del_muc_vcard_info(LServer,Mucname) ->
	ejabberd_odbc:sql_query(LServer,
			[<<"delete from muc_vcard_info where muc_name = '">>,Mucname,<<"' or muc_name = '">>,
								Mucname,<<"@conference.">>,LServer,<<"';">>]).

set_muc_vcard_info(LServer,Mucname,Nick,Desc,Title,Pic) ->
	ejabberd_odbc:sql_query(LServer,
			[<<"update muc_vcard_info set show_name = '">>,
				Nick,<<"', muc_desc = '">>,Desc,<<"', muc_title = '">>,Title,
						<<"', muc_pic = '">>,Pic ,<<"' where muc_name = '">>,Mucname,<<"';">>]).

insert_muc_vcard_info(LServer,Mucname,Nick,Desc,Title,Pic,Version) ->
	ejabberd_odbc:sql_query(LServer,
			[<<"insert into muc_vcard_info(muc_name,show_name,muc_desc,muc_title,muc_pic,version) values ('">>,Mucname,<<"','">>,
				Nick,<<"','">>,Desc,<<"','">>,Title,<<"','">>,Pic,<<"','">>,Version,<<"');">>]).

update_muc_last(LServer,Mucname,Ctime,Mtime) ->
	ejabberd_odbc:sql_query(LServer,
			[<<"update muc_last set create_time = '">>,Ctime,<<"' , set last_msg_time = '">>,Mtime,<<"'  where muc_name = '">>,Mucname,<<"';">>]).

update_muc_last_create_time(LServer,Mucname,Ctime) ->
	ejabberd_odbc:sql_query(LServer,
			[<<"update muc_last set create_time = '">>,Ctime,<<"' where muc_name = '">>,Mucname,<<"';">>]).

del_user_return_password(_LServer, Username, Pass) ->
    P =
	ejabberd_odbc:sql_query_t([<<"select password from users where username='">>,
				   Username, <<"';">>]),
    ejabberd_odbc:sql_query_t([<<"delete from users where username='">>,
			       Username, <<"' and password='">>, Pass,
			       <<"';">>]),
    P.

get_white_list_users(LServer) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select username, single_flag from white_list">>]).
list_users(LServer) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select username from users">>]).

list_users(LServer, [{from, Start}, {to, End}])
    when is_integer(Start) and is_integer(End) ->
    list_users(LServer,
	       [{limit, End - Start + 1}, {offset, Start - 1}]);
list_users(LServer,
	   [{prefix, Prefix}, {from, Start}, {to, End}])
    when is_binary(Prefix) and is_integer(Start) and
	   is_integer(End) ->
    list_users(LServer,
	       [{prefix, Prefix}, {limit, End - Start + 1},
		{offset, Start - 1}]);
list_users(LServer, [{limit, Limit}, {offset, Offset}])
    when is_integer(Limit) and is_integer(Offset) ->
    ejabberd_odbc:sql_query(LServer,
			    [list_to_binary(
                               io_lib:format(
                                 "select username from users " ++
                                     "order by username " ++
                                     "limit ~w offset ~w",
                                 [Limit, Offset]))]);
list_users(LServer,
	   [{prefix, Prefix}, {limit, Limit}, {offset, Offset}])
    when is_binary(Prefix) and is_integer(Limit) and
	   is_integer(Offset) ->
    ejabberd_odbc:sql_query(LServer,
			    [list_to_binary(
                               io_lib:format(
                                 "select username from users " ++
                                     "where username like '~s%' " ++
                                     "order by username " ++
                                     "limit ~w offset ~w ",
                                 [Prefix, Limit, Offset]))]).

users_number(LServer) ->
    Type = ejabberd_config:get_option({odbc_type, LServer},
                                      fun(pgsql) -> pgsql;
                                         (mysql) -> mysql;
                                         (odbc) -> odbc
                                      end, odbc),
    case Type of
      pgsql ->
	  case
	    ejabberd_config:get_option(
              {pgsql_users_number_estimate, LServer},
              fun(V) when is_boolean(V) -> V end,
              false)
	      of
	    true ->
		ejabberd_odbc:sql_query(LServer,
					[<<"select reltuples from pg_class where "
                                           "oid = 'users'::regclass::oid">>]);
	    _ ->
		ejabberd_odbc:sql_query(LServer,
					[<<"select count(*) from users">>])
	  end;
      _ ->
	  ejabberd_odbc:sql_query(LServer,
				  [<<"select count(*) from users">>])
    end.

users_number(LServer, [{prefix, Prefix}])
    when is_binary(Prefix) ->
    ejabberd_odbc:sql_query(LServer,
			    [list_to_binary(
                               io_lib:fwrite(
                                 "select count(*) from users " ++
                                     %% Warning: Escape prefix at higher level to prevent SQL
                                     %%          injection.
                                     "where username like '~s%'",
                                 [Prefix]))]);
users_number(LServer, []) ->
    users_number(LServer).


add_spool_sql(FromUsername, Username, XML) ->
    [<<"insert into spool(from_username, username, xml) values ('">>,
     FromUsername, <<"', '">>, Username, <<"', '">>, XML, <<"');">>].

add_spool_sql_no_notice(FromUsername, Username, XML) ->
    [<<"insert into spool(from_username, username, xml ,notice_flag) values ('">>,
     FromUsername, <<"', '">>, Username, <<"', '">>, XML, <<"','1');">>].

add_spool_away(LServer,FromUsername,Username,XML,Notice_flag) ->
	ejabberd_odbc:sql_query(LServer,
		[<<"insert into spool(from_username, username, xml ,away_flag,notice_flag)  values ('">>,
			FromUsername, <<"', '">>, Username, <<"', '">>, XML, <<"','1','">>,Notice_flag,<<"');">>]).

add_spool(LServer, Queries) ->
    ejabberd_odbc:sql_transaction(LServer, Queries).

get_and_del_spool_msg_t(LServer, Username) ->
    F = fun () ->
		Result =
		    ejabberd_odbc:sql_query_t([<<"select username, xml from spool where "
						 "username='">>,
					       Username,
					       <<"' and away_flag = '0' order by seq;">>]),
		ejabberd_odbc:sql_query_t([<<"delete from spool where username='">>,
					   Username, <<"';">>]),
		Result
	end,
    ejabberd_odbc:sql_transaction(LServer, F).

del_spool_msg(LServer, Username) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"delete from spool where username='">>, Username,
			     <<"';">>]).

get_roster(LServer, Username) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select username, jid, nick, subscription, "
			       "ask, askmessage, server, subscribe, "
			       "type from rosterusers where username='">>,
			     Username, <<"'">>]).

get_roster_info(LServer) ->
    ejabberd_odbc:sql_query(LServer,
                             [<<"select * from users a">>]).
get_department_info(LServer) ->
    ejabberd_odbc:sql_query(LServer,
         [<<"select dep1,dep2,dep3,dep4,dep5,username,name,department,fpinyin,spinyin from users order by dep1,dep2,dep3,dep4,dep5;">>]).

get_user_suoxie(LServer) ->
	ejabberd_odbc:sql_query(LServer,
		[<<"select username,fpinyin,spinyin from users ;">>]).

get_use_registed_muc_num(LServer,User) ->
	ejabberd_odbc:sql_query(LServer,
		[<<"select count(1),muc_name from muc_room_users where muc_name in 
			(select muc_name from muc_room_users where username = '">>,	User,<<"') group by muc_name">>]).

get_iplimit(LServer) ->
	ejabberd_odbc:sql_query(LServer,
		[<<"select ip from iplimit ;">>]).

insert_iplimit(LServer,Ip,Des) ->
	ejabberd_odbc:sql_query(LServer,
		[<<"insert into iplimit(ip,descriptions) values ('">>,Ip,<<"','">>,Des,<<"');">>]).

delete_iplimit(LServer,Ip) ->
	ejabberd_odbc:sql_query(LServer,
		[<<"delete from iplimit where ip = '">>,Ip,<<"';">>]).


get_muc_history(LServer,Muc_name,History_size) ->
	ejabberd_odbc:sql_query(LServer,
		[<<"select muc_room_name,nick,packet,have_subject,size,m_timestamp from muc_room_history where muc_room_name = '">>,Muc_name,<<"' ">>,
				<<"order by m_timestamp desc limit ">>,integer_to_list(History_size),<<";">>]).

get_muc_users(LServer,Tabname,Muc_name) ->
	ejabberd_odbc:sql_query(LServer,
		[<<"select muc_name,username,host from ">>,Tabname,<<" where muc_name = '">>,Muc_name,<<"';">>]).

del_muc_user(LServer,Tabname,Muc_name,Username) ->
	ejabberd_odbc:sql_query(LServer,
		[<<"delete from ">>,Tabname,<<" where muc_name = '">>,Muc_name,<<"' and username = '">>,Username,<<"';">>]).

del_muc_users(LServer,Tabname,Muc_name) ->
	ejabberd_odbc:sql_query(LServer,
		[<<"delete from ">>,Tabname,<<" where muc_name = '">>,Muc_name,<<"';">>]).

get_user_mucs(LServer,Tabname,User) ->
	ejabberd_odbc:sql_query(LServer,
		[<<"select muc_name from ">>,Tabname,<<" where username = '">>,User,<<"';">>]).

get_msg_info(LServer,From,To,Timestamp,Num) ->
	ejabberd_odbc:sql_query(LServer,
		[<<"select m_from,m_to,m_body,read_flag from msg_history where (m_from,m_to) in (('">>,From,
			<<"','">>,To,<<"'),('">>, To,<<"','">>,From,<<"')) and m_timestamp > ">>,Timestamp,<<" order by m_timestamp asc limit ">>,Num,<<";">>]).

get_msg_info1(LServer,From,To,Timestamp,Num) ->
	ejabberd_odbc:sql_query(LServer,
		[<<"select m_from,m_to,m_body,read_flag from msg_history where (m_from,m_to) in (('">>,From,
			<<"','">>,To,<<"'),('">>, To,<<"','">>,From,<<"')) and m_timestamp < ">>,Timestamp,<<" order by m_timestamp desc limit ">>,Num,<<";">>]).

get_msg_info2(LServer,User,Timestamp,Direction) ->
	ejabberd_odbc:sql_query(LServer,
			[<<"select m_from,m_to,m_body,read_flag from msg_history where (m_from = '">>,User, <<"' or m_to = '">>,User,<<"') and m_timestamp > ">>,
				Timestamp,<<" order by m_timestamp ">>,Direction,<<";">>]).

get_muc_msg_info(LServer,From,Timestamp,Num) ->
	ejabberd_odbc:sql_query(LServer,
		[<<"select muc_room_name,nick,packet from muc_room_history where muc_room_name = '">>,From,
			 <<"' and m_timestamp > ">>,Timestamp , <<" order by m_timestamp asc limit ">>,Num,<<";">>]).

get_muc_msg_info1(LServer,From,Timestamp,Num) -> 
	ejabberd_odbc:sql_query(LServer,
		[<<"select muc_room_name,nick,packet from muc_room_history where muc_room_name = '">>,From,
			 <<"' and m_timestamp < ">>,Timestamp , <<" order by m_timestamp desc limit ">>,Num,<<";">>]).

get_last_muc_msg_time(LServer,From,Num) ->
	ejabberd_odbc:sql_query(LServer,
		[<<"select m_timestamp from muc_room_history where muc_room_name = '">>,From,
			 <<"' order by m_timestamp desc limit ">>,Num,<<";">>]).

get_roster_jid_groups(LServer, Username) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select jid, grp from rostergroups where "
			       "username='">>,
			     Username, <<"'">>]).

get_roster_groups(_LServer, Username, SJID) ->
    ejabberd_odbc:sql_query_t([<<"select grp from rostergroups where username='">>,
			       Username, <<"' and jid='">>, SJID, <<"';">>]).

del_user_roster_t(LServer, Username) ->
    ejabberd_odbc:sql_transaction(LServer,
				  fun () ->
					  ejabberd_odbc:sql_query_t([<<"delete from rosterusers       where "
								       "username='">>,
								     Username,
								     <<"';">>]),
					  ejabberd_odbc:sql_query_t([<<"delete from rostergroups       where "
								       "username='">>,
								     Username,
								     <<"';">>])
				  end).

get_roster_by_jid(_LServer, Username, SJID) ->
    ejabberd_odbc:sql_query_t([<<"select username, jid, nick, subscription, "
				 "ask, askmessage, server, subscribe, "
				 "type from rosterusers where username='">>,
			       Username, <<"' and jid='">>, SJID, <<"';">>]).

get_rostergroup_by_jid(LServer, Username, SJID) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select grp from rostergroups where username='">>,
			     Username, <<"' and jid='">>, SJID, <<"'">>]).

del_roster(_LServer, Username, SJID) ->
    ejabberd_odbc:sql_query_t([<<"delete from rosterusers       where "
				 "username='">>,
			       Username, <<"'         and jid='">>, SJID,
			       <<"';">>]),
    ejabberd_odbc:sql_query_t([<<"delete from rostergroups       where "
				 "username='">>,
			       Username, <<"'         and jid='">>, SJID,
			       <<"';">>]).

del_roster_sql(Username, SJID) ->
    [[<<"delete from rosterusers       where "
	"username='">>,
      Username, <<"'         and jid='">>, SJID, <<"';">>],
     [<<"delete from rostergroups       where "
	"username='">>,
      Username, <<"'         and jid='">>, SJID, <<"';">>]].

update_roster(_LServer, Username, SJID, ItemVals,
	      ItemGroups) ->
    update_t(<<"rosterusers">>,
	     [<<"username">>, <<"jid">>, <<"nick">>,
	      <<"subscription">>, <<"ask">>, <<"askmessage">>,
	      <<"server">>, <<"subscribe">>, <<"type">>],
	     ItemVals,
	     [<<"username='">>, Username, <<"' and jid='">>, SJID,
	      <<"'">>]),
    ejabberd_odbc:sql_query_t([<<"delete from rostergroups       where "
				 "username='">>,
			       Username, <<"'         and jid='">>, SJID,
			       <<"';">>]),
    lists:foreach(fun (ItemGroup) ->
			  ejabberd_odbc:sql_query_t([<<"insert into rostergroups(           "
						       "   username, jid, grp)  values ('">>,
						     join(ItemGroup,
							  <<"', '">>),
						     <<"');">>])
		  end,
		  ItemGroups).

update_roster_sql(Username, SJID, ItemVals,
		  ItemGroups) ->
    [[<<"delete from rosterusers       where "
	"username='">>,
      Username, <<"'         and jid='">>, SJID, <<"';">>],
     [<<"insert into rosterusers(            "
	"  username, jid, nick,              "
	" subscription, ask, askmessage,     "
	"          server, subscribe, type)  "
	"values ('">>,
      join(ItemVals, <<"', '">>), <<"');">>],
     [<<"delete from rostergroups       where "
	"username='">>,
      Username, <<"'         and jid='">>, SJID, <<"';">>]]
      ++
      [[<<"insert into rostergroups(           "
	  "   username, jid, grp)  values ('">>,
	join(ItemGroup, <<"', '">>), <<"');">>]
       || ItemGroup <- ItemGroups].

roster_subscribe(_LServer, Username, SJID, ItemVals) ->
    update_t(<<"rosterusers">>,
	     [<<"username">>, <<"jid">>, <<"nick">>,
	      <<"subscription">>, <<"ask">>, <<"askmessage">>,
	      <<"server">>, <<"subscribe">>, <<"type">>],
	     ItemVals,
	     [<<"username='">>, Username, <<"' and jid='">>, SJID,
	      <<"'">>]).

get_subscription(LServer, Username, SJID) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select subscription from rosterusers "
			       "where username='">>,
			     Username, <<"' and jid='">>, SJID, <<"'">>]).

set_private_data(_LServer, Username, LXMLNS, SData) ->
    update_t(<<"private_storage">>,
	     [<<"username">>, <<"namespace">>, <<"data">>],
	     [Username, LXMLNS, SData],
	     [<<"username='">>, Username, <<"' and namespace='">>,
	      LXMLNS, <<"'">>]).

set_private_data_sql(Username, LXMLNS, SData) ->
    [[<<"delete from private_storage where username='">>,
      Username, <<"' and namespace='">>, LXMLNS, <<"';">>],
     [<<"insert into private_storage(username, "
	"namespace, data) values ('">>,
      Username, <<"', '">>, LXMLNS, <<"', '">>, SData,
      <<"');">>]].

get_private_data(LServer, Username, LXMLNS) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select data from private_storage where "
			       "username='">>,
			     Username, <<"' and namespace='">>, LXMLNS,
			     <<"';">>]).

get_private_data(LServer, Username) ->
    ejabberd_odbc:sql_query(LServer,
                            [<<"select namespace, data from private_storage "
                               "where username='">>, Username, <<"';">>]).

del_user_private_storage(LServer, Username) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"delete from private_storage where username='">>,
			     Username, <<"';">>]).

set_vcard(LServer, LUsername, SBDay, SCTRY, SEMail, SFN,
	  SFamily, SGiven, SLBDay, SLCTRY, SLEMail, SLFN,
	  SLFamily, SLGiven, SLLocality, SLMiddle, SLNickname,
	  SLOrgName, SLOrgUnit, SLocality, SMiddle, SNickname,
	  SOrgName, SOrgUnit, SVCARD, Username) ->
    ejabberd_odbc:sql_transaction(LServer,
				  fun () ->
					  update_t(<<"vcard">>,
						   [<<"username">>,
						    <<"vcard">>],
						   [LUsername, SVCARD],
						   [<<"username='">>, LUsername,
						    <<"'">>]),
					  update_t(<<"vcard_search">>,
						   [<<"username">>,
						    <<"lusername">>, <<"fn">>,
						    <<"lfn">>, <<"family">>,
						    <<"lfamily">>, <<"given">>,
						    <<"lgiven">>, <<"middle">>,
						    <<"lmiddle">>,
						    <<"nickname">>,
						    <<"lnickname">>, <<"bday">>,
						    <<"lbday">>, <<"ctry">>,
						    <<"lctry">>, <<"locality">>,
						    <<"llocality">>,
						    <<"email">>, <<"lemail">>,
						    <<"orgname">>,
						    <<"lorgname">>,
						    <<"orgunit">>,
						    <<"lorgunit">>],
						   [Username, LUsername, SFN,
						    SLFN, SFamily, SLFamily,
						    SGiven, SLGiven, SMiddle,
						    SLMiddle, SNickname,
						    SLNickname, SBDay, SLBDay,
						    SCTRY, SLCTRY, SLocality,
						    SLLocality, SEMail, SLEMail,
						    SOrgName, SLOrgName,
						    SOrgUnit, SLOrgUnit],
						   [<<"lusername='">>,
						    LUsername, <<"'">>])
				  end).

get_vcard(LServer, Username) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select vcard from vcard where username='">>,
			     Username, <<"';">>]).

get_default_privacy_list(LServer, Username) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select name from privacy_default_list "
			       "where username='">>,
			     Username, <<"';">>]).

get_default_privacy_list_t(Username) ->
    ejabberd_odbc:sql_query_t([<<"select name from privacy_default_list "
				 "where username='">>,
			       Username, <<"';">>]).

get_privacy_list_names(LServer, Username) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select name from privacy_list where "
			       "username='">>,
			     Username, <<"';">>]).

get_privacy_list_names_t(Username) ->
    ejabberd_odbc:sql_query_t([<<"select name from privacy_list where "
				 "username='">>,
			       Username, <<"';">>]).

get_privacy_list_id(LServer, Username, SName) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select id from privacy_list where username='">>,
			     Username, <<"' and name='">>, SName, <<"';">>]).

get_privacy_list_id_t(Username, SName) ->
    ejabberd_odbc:sql_query_t([<<"select id from privacy_list where username='">>,
			       Username, <<"' and name='">>, SName, <<"';">>]).

get_privacy_list_data(LServer, Username, SName) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select t, value, action, ord, match_all, "
			       "match_iq, match_message, match_presence_in, "
			       "match_presence_out from privacy_list_data "
			       "where id = (select id from privacy_list "
			       "where             username='">>,
			     Username, <<"' and name='">>, SName,
			     <<"') order by ord;">>]).

get_privacy_list_data_t(Username, SName) ->
    ejabberd_odbc:sql_query_t([<<"select t, value, action, ord, match_all, "
                                 "match_iq, match_message, match_presence_in, "
                                 "match_presence_out from privacy_list_data "
                                 "where id = (select id from privacy_list "
                                 "where             username='">>,
                               Username, <<"' and name='">>, SName,
                               <<"') order by ord;">>]).

get_privacy_list_data_by_id(LServer, ID) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select t, value, action, ord, match_all, "
			       "match_iq, match_message, match_presence_in, "
			       "match_presence_out from privacy_list_data "
			       "where id='">>,
			     ID, <<"' order by ord;">>]).

get_privacy_list_data_by_id_t(ID) ->
    ejabberd_odbc:sql_query_t([<<"select t, value, action, ord, match_all, "
				 "match_iq, match_message, match_presence_in, "
				 "match_presence_out from privacy_list_data "
				 "where id='">>,
			       ID, <<"' order by ord;">>]).

set_default_privacy_list(Username, SName) ->
    update_t(<<"privacy_default_list">>,
	     [<<"username">>, <<"name">>], [Username, SName],
	     [<<"username='">>, Username, <<"'">>]).

unset_default_privacy_list(LServer, Username) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"delete from privacy_default_list    "
			       "   where username='">>,
			     Username, <<"';">>]).

remove_privacy_list(Username, SName) ->
    ejabberd_odbc:sql_query_t([<<"delete from privacy_list where username='">>,
			       Username, <<"' and name='">>, SName, <<"';">>]).

add_privacy_list(Username, SName) ->
    ejabberd_odbc:sql_query_t([<<"insert into privacy_list(username, name) "
				 "values ('">>,
			       Username, <<"', '">>, SName, <<"');">>]).

set_privacy_list(ID, RItems) ->
    ejabberd_odbc:sql_query_t([<<"delete from privacy_list_data where "
				 "id='">>,
			       ID, <<"';">>]),
    lists:foreach(fun (Items) ->
			  ejabberd_odbc:sql_query_t([<<"insert into privacy_list_data(id, t, "
						       "value, action, ord, match_all, match_iq, "
						       "match_message, match_presence_in, match_prese"
						       "nce_out ) values ('">>,
						     ID, <<"', '">>,
						     join(Items, <<"', '">>),
						     <<"');">>])
		  end,
		  RItems).

del_privacy_lists(LServer, Server, Username) ->
%% Characters to escape
%% Count number of records in a table given a where clause
    ejabberd_odbc:sql_query(LServer,
			    [<<"delete from privacy_list where username='">>,
			     Username, <<"';">>]),
    ejabberd_odbc:sql_query(LServer,
			    [<<"delete from privacy_list_data where "
			       "value='">>,
			     <<Username/binary, "@", Server/binary>>,
			     <<"';">>]),
    ejabberd_odbc:sql_query(LServer,
			    [<<"delete from privacy_default_list where "
			       "username='">>,
			     Username, <<"';">>]).

escape($\000) -> <<"\\0">>;
escape($\n) -> <<"\\n">>;
escape($\t) -> <<"\\t">>;
escape($\b) -> <<"\\b">>;
escape($\r) -> <<"\\r">>;
escape($') -> <<"''">>;
escape($") -> <<"\\\"">>;
escape($\\) -> <<"\\\\">>;
escape(C) -> <<C>>.

count_records_where(LServer, Table, WhereClause) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select count(*) from ">>, Table, <<" ">>,
			     WhereClause, <<";">>]).

get_roster_version(LServer, LUser) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select version from roster_version where "
			       "username = '">>,
			     LUser, <<"'">>]).

set_roster_version(LUser, Version) ->
    update_t(<<"roster_version">>,
	     [<<"username">>, <<"version">>], [LUser, Version],
	     [<<"username = '">>, LUser, <<"'">>]).

update_blacklist(Username, Flag) ->
    update_t(<<"users">>,
             [<<"frozen_flag">>], [Flag],
             [<<"username='">>, Username, <<"'">>]).

update_whitelist(Username,Flag) ->
    update_t(<<"white_list">>,
             [<<"username">>,<<"single_flag">>], [Username,Flag],
             [<<"username='">>, Username, <<"'">>]).
	

-endif.

%% -----------------
%% MSSQL queries
-ifdef(mssql).

%% Queries can be either a fun or a list of queries
get_db_type() -> mssql.

sql_transaction(LServer, Queries)
    when is_list(Queries) ->
    F = fun () ->
		lists:foreach(fun (Query) ->
				      ejabberd_odbc:sql_query(LServer, Query)
			      end,
			      Queries)
	end,
    {atomic, catch F()};
sql_transaction(_LServer, FQueries) ->
    {atomic, catch FQueries()}.

get_last(LServer, Username) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"EXECUTE dbo.get_last '">>, Username, <<"'">>]).

set_last_t(LServer, Username, Seconds, State) ->
    Result = ejabberd_odbc:sql_query(LServer,
				     [<<"EXECUTE dbo.set_last '">>, Username,
				      <<"', '">>, Seconds, <<"', '">>, State,
				      <<"'">>]),
    {atomic, Result}.

del_last(LServer, Username) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"EXECUTE dbo.del_last '">>, Username, <<"'">>]).

get_password(LServer, Username) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"EXECUTE dbo.get_password '">>, Username,
			     <<"'">>]).

set_password_t(LServer, Username, Pass) ->
    Result = ejabberd_odbc:sql_query(LServer,
				     [<<"EXECUTE dbo.set_password '">>,
				      Username, <<"', '">>, Pass, <<"'">>]),
    {atomic, Result}.

add_user(LServer, Username, Pass) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"EXECUTE dbo.add_user '">>, Username, <<"', '">>,
			     Pass, <<"'">>]).

del_user(LServer, Username) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"EXECUTE dbo.del_user '">>, Username, <<"'">>]).

del_user_return_password(LServer, Username, Pass) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"EXECUTE dbo.del_user_return_password '">>,
			     Username, <<"'">>]),
    Pass.

list_users(LServer) ->
    ejabberd_odbc:sql_query(LServer,
			    <<"EXECUTE dbo.list_users">>).

list_users(LServer, _) -> list_users(LServer).

users_number(LServer) ->
    ejabberd_odbc:sql_query(LServer,
			    <<"select count(*) from users with (nolock)">>).

users_number(LServer, _) -> users_number(LServer).

add_spool_sql(Username, XML) ->
    [<<"EXECUTE dbo.add_spool '">>, Username, <<"' , '">>,
     XML, <<"'">>].

add_spool(LServer, Queries) ->
    lists:foreach(fun (Query) ->
			  ejabberd_odbc:sql_query(LServer, Query)
		  end,
		  Queries).

get_and_del_spool_msg_t(LServer, Username) ->
    [Result] = case ejabberd_odbc:sql_query(LServer,
					    [<<"EXECUTE dbo.get_and_del_spool_msg '">>,
					     Username, <<"'">>])
		   of
		 Rs when is_list(Rs) ->
		     lists:filter(fun ({selected, _Header, _Row}) -> true;
				      ({updated, _N}) -> false
				  end,
				  Rs);
		 Rs -> [Rs]
	       end,
    {atomic, Result}.

del_spool_msg(LServer, Username) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"EXECUTE dbo.del_spool_msg '">>, Username,
			     <<"'">>]).

get_roster(LServer, Username) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"EXECUTE dbo.get_roster '">>, Username,
			     <<"'">>]).

get_roster_jid_groups(LServer, Username) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"EXECUTE dbo.get_roster_jid_groups '">>,
			     Username, <<"'">>]).

get_roster_groups(LServer, Username, SJID) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"EXECUTE dbo.get_roster_groups '">>, Username,
			     <<"' , '">>, SJID, <<"'">>]).

del_user_roster_t(LServer, Username) ->
    Result = ejabberd_odbc:sql_query(LServer,
				     [<<"EXECUTE dbo.del_user_roster '">>,
				      Username, <<"'">>]),
    {atomic, Result}.

get_roster_by_jid(LServer, Username, SJID) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"EXECUTE dbo.get_roster_by_jid '">>, Username,
			     <<"' , '">>, SJID, <<"'">>]).

get_rostergroup_by_jid(LServer, Username, SJID) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"EXECUTE dbo.get_rostergroup_by_jid '">>,
			     Username, <<"' , '">>, SJID, <<"'">>]).

del_roster(LServer, Username, SJID) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"EXECUTE dbo.del_roster '">>, Username,
			     <<"', '">>, SJID, <<"'">>]).

del_roster_sql(Username, SJID) ->
    [<<"EXECUTE dbo.del_roster '">>, Username, <<"', '">>,
     SJID, <<"'">>].

update_roster(LServer, Username, SJID, ItemVals,
	      ItemGroups) ->
    Query1 = [<<"EXECUTE dbo.del_roster '">>, Username,
	      <<"', '">>, SJID, <<"' ">>],
    ejabberd_odbc:sql_query(LServer, lists:flatten(Query1)),
    Query2 = [<<"EXECUTE dbo.add_roster_user ">>, ItemVals],
    ejabberd_odbc:sql_query(LServer, lists:flatten(Query2)),
    Query3 = [<<"EXECUTE dbo.del_roster_groups '">>,
	      Username, <<"', '">>, SJID, <<"' ">>],
    ejabberd_odbc:sql_query(LServer, lists:flatten(Query3)),
    lists:foreach(fun (ItemGroup) ->
			  Query = [<<"EXECUTE dbo.add_roster_group ">>,
				   ItemGroup],
			  ejabberd_odbc:sql_query(LServer, lists:flatten(Query))
		  end,
		  ItemGroups).

update_roster_sql(Username, SJID, ItemVals,
		  ItemGroups) ->
    [<<"BEGIN TRANSACTION ">>,
     <<"EXECUTE dbo.del_roster_groups '">>, Username,
     <<"','">>, SJID, <<"' ">>,
     <<"EXECUTE dbo.add_roster_user ">>, ItemVals, <<" ">>]
      ++
      [lists:flatten(<<"EXECUTE dbo.add_roster_group ">>,
		     ItemGroup, <<" ">>)
       || ItemGroup <- ItemGroups]
	++ [<<"COMMIT">>].

roster_subscribe(LServer, _Username, _SJID, ItemVals) ->
    catch ejabberd_odbc:sql_query(LServer,
				  [<<"EXECUTE dbo.add_roster_user ">>,
				   ItemVals]).

get_subscription(LServer, Username, SJID) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"EXECUTE dbo.get_subscription '">>, Username,
			     <<"' , '">>, SJID, <<"'">>]).

set_private_data(LServer, Username, LXMLNS, SData) ->
    ejabberd_odbc:sql_query(LServer,
			    set_private_data_sql(Username, LXMLNS, SData)).

set_private_data_sql(Username, LXMLNS, SData) ->
    [<<"EXECUTE dbo.set_private_data '">>, Username,
     <<"' , '">>, LXMLNS, <<"' , '">>, SData, <<"'">>].

get_private_data(LServer, Username, LXMLNS) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"EXECUTE dbo.get_private_data '">>, Username,
			     <<"' , '">>, LXMLNS, <<"'">>]).

del_user_private_storage(LServer, Username) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"EXECUTE dbo.del_user_storage '">>, Username,
			     <<"'">>]).

set_vcard(LServer, LUsername, SBDay, SCTRY, SEMail, SFN,
	  SFamily, SGiven, SLBDay, SLCTRY, SLEMail, SLFN,
	  SLFamily, SLGiven, SLLocality, SLMiddle, SLNickname,
	  SLOrgName, SLOrgUnit, SLocality, SMiddle, SNickname,
	  SOrgName, SOrgUnit, SVCARD, Username) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"EXECUTE dbo.set_vcard '">>, SVCARD, <<"' , '">>,
			     Username, <<"' , '">>, LUsername, <<"' , '">>, SFN,
			     <<"' , '">>, SLFN, <<"' , '">>, SFamily,
			     <<"' , '">>, SLFamily, <<"' , '">>, SGiven,
			     <<"' , '">>, SLGiven, <<"' , '">>, SMiddle,
			     <<"' , '">>, SLMiddle, <<"' , '">>, SNickname,
			     <<"' , '">>, SLNickname, <<"' , '">>, SBDay,
			     <<"' , '">>, SLBDay, <<"' , '">>, SCTRY,
			     <<"' , '">>, SLCTRY, <<"' , '">>, SLocality,
			     <<"' , '">>, SLLocality, <<"' , '">>, SEMail,
			     <<"' , '">>, SLEMail, <<"' , '">>, SOrgName,
			     <<"' , '">>, SLOrgName, <<"' , '">>, SOrgUnit,
			     <<"' , '">>, SLOrgUnit, <<"'">>]).

get_vcard(LServer, Username) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"EXECUTE dbo.get_vcard '">>, Username, <<"'">>]).

get_default_privacy_list(LServer, Username) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"EXECUTE dbo.get_default_privacy_list '">>,
			     Username, <<"'">>]).

get_default_privacy_list_t(Username) ->
    ejabberd_odbc:sql_query_t([<<"EXECUTE dbo.get_default_privacy_list '">>,
			       Username, <<"'">>]).

get_privacy_list_names(LServer, Username) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"EXECUTE dbo.get_privacy_list_names '">>,
			     Username, <<"'">>]).

get_privacy_list_names_t(Username) ->
    ejabberd_odbc:sql_query_t([<<"EXECUTE dbo.get_privacy_list_names '">>,
			       Username, <<"'">>]).

get_privacy_list_id(LServer, Username, SName) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"EXECUTE dbo.get_privacy_list_id '">>, Username,
			     <<"' , '">>, SName, <<"'">>]).

get_privacy_list_id_t(Username, SName) ->
    ejabberd_odbc:sql_query_t([<<"EXECUTE dbo.get_privacy_list_id '">>,
			       Username, <<"' , '">>, SName, <<"'">>]).

get_privacy_list_data(LServer, Username, SName) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"EXECUTE dbo.get_privacy_list_data '">>,
			     Username, <<"' , '">>, SName, <<"'">>]).

get_privacy_list_data_by_id(LServer, ID) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"EXECUTE dbo.get_privacy_list_data_by_id '">>,
			     ID, <<"'">>]).

get_privacy_list_data_by_id_t(ID) ->
    ejabberd_odbc:sql_query_t([<<"EXECUTE dbo.get_privacy_list_data_by_id '">>,
			       ID, <<"'">>]).

set_default_privacy_list(Username, SName) ->
    ejabberd_odbc:sql_query_t([<<"EXECUTE dbo.set_default_privacy_list '">>,
			       Username, <<"' , '">>, SName, <<"'">>]).

unset_default_privacy_list(LServer, Username) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"EXECUTE dbo.unset_default_privacy_list '">>,
			     Username, <<"'">>]).

remove_privacy_list(Username, SName) ->
    ejabberd_odbc:sql_query_t([<<"EXECUTE dbo.remove_privacy_list '">>,
			       Username, <<"' , '">>, SName, <<"'">>]).

add_privacy_list(Username, SName) ->
    ejabberd_odbc:sql_query_t([<<"EXECUTE dbo.add_privacy_list '">>,
			       Username, <<"' , '">>, SName, <<"'">>]).

set_privacy_list(ID, RItems) ->
    ejabberd_odbc:sql_query_t([<<"EXECUTE dbo.del_privacy_list_by_id '">>,
			       ID, <<"'">>]),
    lists:foreach(fun (Items) ->
			  ejabberd_odbc:sql_query_t([<<"EXECUTE dbo.set_privacy_list '">>,
						     ID, <<"', '">>,
						     join(Items, <<"', '">>),
						     <<"'">>])
		  end,
		  RItems).

del_privacy_lists(LServer, Server, Username) ->
%% Characters to escape
%% Count number of records in a table given a where clause
    ejabberd_odbc:sql_query(LServer,
			    [<<"EXECUTE dbo.del_privacy_lists @Server='">>,
			     Server, <<"' @username='">>, Username, <<"'">>]).

escape($\000) -> <<"\\0">>;
escape($\t) -> <<"\\t">>;
escape($\b) -> <<"\\b">>;
escape($\r) -> <<"\\r">>;
escape($') -> <<"''">>;
escape($") -> <<"\\\"">>;
escape(C) -> C.

count_records_where(LServer, Table, WhereClause) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select count(*) from ">>, Table,
			     <<" with (nolock) ">>, WhereClause]).

get_roster_version(LServer, LUser) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"EXECUTE dbo.get_roster_version '">>, LUser,
			     <<"'">>]).

set_roster_version(Username, Version) ->
    LServer = (?MYNAME),
    ejabberd_odbc:sql_query(LServer,
			    [<<"EXECUTE dbo.set_roster_version '">>, Username,
			     <<"', '">>, Version, <<"'">>]).

-endif.
