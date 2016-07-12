-module(ejabberd_public).

-include("ejabberd_extend.hrl").

-export([get_user_nick/1,make_sent_packet/2,is_conference_server/1,set_redis_user_key/6]).
-export([clear_redis_user_key/3,make_send_packet/2,make_send_packet/3,user_status/1]).

get_user_nick(User) ->
    case catch ets:lookup(name_nick,User) of
	[{_,Name,_}] -> 
		Name;
	_ ->
	    User
	end.

make_sent_packet(JID,ID) ->
   xml:to_xmlel(
		{xmlel ,<<"message">>, [{<<"type">>,<<"mstat">>},{<<"to">>,jlib:jid_to_string(JID)}],
			[{xmlel,<<"body">>,[{<<"id">>,ID},{<<"stat">>,<<"sent">>}],[]}]}).

is_conference_server(Server) ->
    str:str(Server,<<"conference">>) =/= 0.

set_redis_user_key(Server,User,Resource,Key,Mac_key,Time) ->
	catch redis_link:hash_set(Server,1,User,Resource,Key),
	catch redis_link:expire_time(Server,1,User,Time),
	catch redis_link:hash_set(Server,2,User,Key,Mac_key),
	catch redis_link:expire_time(Server,2,User,Time).

clear_redis_user_key(Server,User,Resource) ->
    case catch redis_link:hash_get(Server,1,User,Resource) of
    {ok,undefined} ->
                    ok;
    {ok,Key} -> 
    	            redis_link:hash_del(Server,1,User,Resource),
					redis_link:hash_del(Server,2,User,Key);
     _ ->
			        ok
	 end.

make_send_packet(Type,Msg) ->
	make_send_packet(Type,Msg,<<"1">>).

make_send_packet(Type,Msg,Msg_Type) ->
	Integer = random:uniform(99999),
	Bid = list_to_binary("http_" ++ integer_to_list(Integer)),
	xml:to_xmlel(
			{xmlel	,<<"message">>,	[{<<"type">>,Type},{<<"id">>,Bid}],
				[{xmlel,<<"active">>,[{<<"xmlns">>,<<"http://jabber.org/protocol/chatstates">>}],[]},
					{xmlel,<<"body">>,[{<<"id">>,Bid},{<<"msgType">>,Msg_Type}],[{xmlcdata, Msg}]}]}).

user_status(U) ->
    case ets:lookup(online_status,U) of
    [Us] when is_record(Us,online_status) ->
        Us#online_status.status;
    _ ->
        0
    end.
