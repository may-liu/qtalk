-record(user_status,		{user ,status}).
-record(online_status,		{user ,status}).
-record(cache_info, 		{name ,cache}).
-record(vcard_version,		{user,version,name,url,gender = <<"0">>}).
-record(user_profile,		{user,version,mood}).
-record(muc_vcard,			{muc_name,show_name,muc_desc,muc_title,muc_pic,version}).
-record(rbt_info,			{name,url,body,version}).
-record(user_rbts,			{user,rbt}).
-record(iplimit,			{ip,user}).
-record(ejabberd_config,	{key,val}).
-record(domain_to_url,		{domain,url}).
-record(user_version,{user,name,version,hire_flag,fp,sp,type,dept}).


