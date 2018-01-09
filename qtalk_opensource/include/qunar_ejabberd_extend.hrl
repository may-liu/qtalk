-record(mac_sub_users,{user,key}).
-record(department_users,{dep1,dep2,dep3,dep4,dep5,user}).
-record(mac_push_notice,{user}).
-record(muc_online_room,
           {name_host = {<<"">>, <<"">>} :: {binary(), binary()} | '$1' |{'_', binary()} | '_', pid = self() :: pid() | '$2' | '_' | '$1'}).

-record(vcard_version,      {user,version,url}).
