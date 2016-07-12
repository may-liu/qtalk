-module(table_handle).

-export([create_ets_table/0,create_mnesia_table/0]).

-record(muc_vcard,{muc_name,show_name,muc_desc,muc_title,muc_pic,version}).
-record(vcard_version,{user,version,url}).
-record(mac_sub_users,{user,key}).

create_ets_table() ->
    catch ets:new(dep_num, [named_table, ordered_set, public,{keypos, 2},{write_concurrency, true}, {read_concurrency, true}]),
    catch ets:new(user_department, [named_table, ordered_set, public]),
    catch ets:new(name_nick, [named_table, ordered_set, public,{write_concurrency, true}, {read_concurrency, true}]),
	catch ets:new(department_users, [bag, named_table, public, {keypos, 2},{write_concurrency, true}, {read_concurrency, true}]),
	catch ets:new(user_suoxie,[set,named_table,public,{keypos, 1},{write_concurrency, true}, {read_concurrency, true}]),
	catch ets:new(client_info,[set,named_table,public,{keypos, 2},{write_concurrency, true}, {read_concurrency, true}]),
	catch ets:new(vcard_version,[set,named_table,public,{keypos,2},{write_concurrency, true}, {read_concurrency, true}]),
	catch ets:new(muc_vcard,[set,named_table,public,{keypos,2},{write_concurrency, true}, {read_concurrency, true}]),
	catch ets:new(user_num,[set,named_table,public,{keypos,2},{write_concurrency, true}, {read_concurrency, true}]),
    catch ets:new(online_users, [set, named_table, public, {keypos, 1},{write_concurrency, true}, {read_concurrency, true}]),
    catch ets:new(away_users, [set, named_table, public, {keypos, 1},{write_concurrency, true}, {read_concurrency, true}]),
	catch ets:new(sn_user, [set, named_table, public, {keypos, 1},{write_concurrency, true}, {read_concurrency, true}]),
    catch ets:new(blacklist, [set, named_table, public, {keypos, 1},{write_concurrency, true}, {read_concurrency, true}]),
    catch ets:new(user_list, [set, named_table, public, {keypos, 1},{write_concurrency, true}, {read_concurrency, true}]),
    catch ets:new(whitelist, [set, named_table, public, {keypos, 1},{write_concurrency, true}, {read_concurrency, true}]),
    catch ets:new(nick_name, [set, named_table, public, {keypos, 1},{write_concurrency, true}, {read_concurrency, true}]),
	catch ets:new(mac_push_notice,[set,named_table,public,{keypos,2},{write_concurrency, true}, {read_concurrency, true}]),
	catch ets:new(subscription,[set,named_table,public,{keypos,2},{write_concurrency, true}, {read_concurrency, true}]),
	catch ets:new(online_status, 	[named_table, ordered_set, public,{keypos, 2},{write_concurrency, true}, {read_concurrency, true}]),
    catch ets:new(cache_info, [named_table, ordered_set, public,{keypos, 2},{write_concurrency, true}, {read_concurrency, true}]),
	subscription:create_rbt_ets().

create_mnesia_table() ->
	catch update_tables(),
    catch mnesia:create_table(muc_vcard,[{type,set},{ram_copies, [node()]},{attributes,record_info(fields, muc_vcard)}]),
	mnesia:add_table_index(muc_vcard, muc_name),
	mnesia:add_table_copy(muc_vcard, node(), ram_copies),
    catch mnesia:create_table(vcard_version,[{type,set},{ram_copies, [node()]},{attributes,record_info(fields, vcard_version)}]),
	mnesia:add_table_index(vcard_version, user),
	mnesia:add_table_copy(vcard_version, node(), ram_copies),
    catch mnesia:create_table(mac_sub_users,[{type,set},{ram_copies, [node()]},{attributes,record_info(fields, mac_sub_users)}]),
	mnesia:add_table_index(mac_sub_users, user),
	mnesia:add_table_copy(mac_sub_users, node(), ram_copies),
    mnesia:subscribe(system).

update_tables() ->
	case lists:member(muc_vcard, mnesia:system_info(tables)) of
	true -> mnesia:delete_table(muc_vcard);
	false -> ok
	end,
	case lists:member(vcard_version, mnesia:system_info(tables)) of
	true -> mnesia:delete_table(vcard_version);
	false -> ok
	end,
	case lists:member(mac_sub_users, mnesia:system_info(tables)) of
	true -> mnesia:delete_table(mac_sub_users);
	false -> ok
	end.

