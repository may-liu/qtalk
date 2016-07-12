
-record(muc_room, {name_host = {<<"">>, <<"">>} :: {binary(), binary()} | {'_', binary()}, opts = [] :: list() | '_'}).

-record(muc_online_room,
		   {name_host = {<<"">>, <<"">>} :: {binary(), binary()} | '$1' |{'_', binary()} | '_', pid = self() :: pid() | '$2' | '_' | '$1'}).

-record(muc_registered,
	        {us_host = {{<<"">>, <<"">>}, <<"">>} :: {{binary(), binary()}, binary()} | '$1',nick = <<"">> :: binary()}).

-record(user_num,	{server,usernum,onlinenum}).

-record(vcard_version,	{user,version,name,url}).

-record(mac_sub_users,	{user,key}).

-record(muc_vcard,	{muc_name,show_name,muc_desc,muc_title,muc_pic,version}).

-record(department_users,	{dep1,dep2,dep3,dep4,dep5,user}).

-record(session, {sid, usr, us, priority, info,show}).

-record(cache_info,         {name ,cache}).

-record(online_status,		{user ,status}).

-record(rbt_info,	{name,url,body,version}).

-record(tree_dept,{name,user_list = [],sub_dept = []}).
