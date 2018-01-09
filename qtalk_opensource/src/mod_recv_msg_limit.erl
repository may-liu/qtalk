-module(mod_recv_msg_limit).

-behaviour(gen_server).
-behaviour(gen_mod).

-export([start/2,stop/1,start_link/2,init/1]).

-export([handle_call/3, handle_cast/2,
 	    handle_info/2, terminate/2, code_change/3]).
-export([get_hash_user_recv_limit/2]).
-export([handle_recv_limit/3,get_user_recv_limit/2,set_user_recv_limit/3,check_send_auth/4,check_authorization/4]).
-export([update_all_recv_msg_limit/1]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").

-define(SERVER, ?MODULE).

-record(recv_msg_limit,{user,mode,version}).
-record(state,{server}).

start(Host,Opts) ->
	Proc = get_proc_name(Host),

	ChildSpec = {Proc,{?MODULE, start_link, [Host,Opts]}, temporary,1000,worker,[?MODULE]},
	{ok,_Pid} = supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
	Proc = get_proc_name(Host),
    ejabberd_hooks:del(privacy_send_authorization, Host, ?MODULE,  check_authorization, 25),
	catch ets:delete(recv_msg_limit),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).

start_link(Host,Opts) ->
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,?NS_RECV_LIMIT, ?MODULE, handle_recv_limit, no_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,?NS_RECV_LIMIT, ?MODULE, handle_recv_limit, no_queue),

    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,?NS_RECV_LIMIT, ?MODULE, handle_recv_limit, no_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,?NS_RECV_LIMIT, ?MODULE, handle_recv_limit, no_queue),

    ejabberd_hooks:add(privacy_send_authorization, Host, ?MODULE,  check_authorization, 25),
	gen_server:start_link({local, get_proc_name(Host)}, ?MODULE, [Host,Opts], []).

init([Server,_Opts]) ->
	{ok, #state{server = Server},15*1000}.


handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout,State) ->
	{noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_Reason, _State) ->
    ok.

get_proc_name(Host) ->
    gen_mod:get_module_proc(Host, ?MODULE).
	
handle_recv_limit(From, To, #iq{type = Type, sub_el = SubEl} = IQ) ->
	case {Type, SubEl} of
	{get, #xmlel{name  = <<"recv_msg_limit">>}} -> 
		 IQ#iq{type = result, sub_el = [iq_get_recv_msg_limit(To#jid.lserver,SubEl)]};
	{set, #xmlel{name  = <<"recv_msg_limit">>}} -> 
		 IQ#iq{type = result, sub_el = [iq_set_recv_msg_limit(To#jid.lserver,From#jid.luser,SubEl)]}
	end.

iq_get_recv_msg_limit(Server,SubEl) ->
	case xml:get_tag_attr_s(<<"jid">>,SubEl) of
	<<"">> ->
		#xmlel{name = <<"get_recv_msg_limit">>,
	    	    attrs = [{<<"xmlns">>,?NS_RECV_LIMIT},{<<"jid">>,<<"">>},
       	     {<<"mode">>,<<"2">>}],
     	 	children = []};
	User ->
		case get_hash_user_recv_limit(Server,User) of
		Recv_limit when is_record(Recv_limit,recv_msg_limit) ->
			#xmlel{name = <<"get_recv_msg_limit">>,
	    		    attrs = [{<<"xmlns">>,?NS_RECV_LIMIT},{<<"jid">>,User},
       	    	 {<<"mode">>,Recv_limit#recv_msg_limit.mode}],
     	 		children = []};
		_ ->
   			#xmlel{name = <<"get_recv_msg_limit">>,
       			    attrs = [{<<"xmlns">>,?NS_VER_FRI_MODE},{<<"jid">>,User},
       		     {<<"mode">>,<<"2">>}],
   				children = []}
		end
	end.

check_authorization(_,From,To,Packet) ->
	#xmlel{name = Name} = Packet,
    case Name of 
	<<"message">> ->
	    Type = xml:get_tag_attr_s(<<"type">>, Packet),
		case Type == <<"chat">> orelse Type == <<"normal">> of
		true ->
			check_send_auth(To#jid.lserver,To#jid.luser,From#jid.luser,From#jid.lserver);
		_ ->
			true
		end;
	_ ->
		true
	end.

check_send_auth(Server,From,To,Domain) ->
	case catch get_hash_user_recv_limit(Server,From) of
	Recv_msg_limit when is_record(Recv_msg_limit,recv_msg_limit) ->
		case Recv_msg_limit#recv_msg_limit.mode of
		<<"1">> ->
			case catch mod_hash_user:check_user_friend_relationship(Server,From,To,Domain) of
			true ->
				true;
			false ->
				false
			end;
		<<"0">> ->
			false;
		_ ->
			true
		end;
	_ ->
		true
	end.

iq_set_recv_msg_limit(Server,User,SubEl) ->
	Opts = xml:get_tag_attr_s(<<"mode">>,SubEl),
	case set_hash_user_recv_limit(Server,User,Opts) of
	%%	Recv_limit when is_record(Recv_limit,recv_msg_limit) ->
	true ->
		#xmlel{name = <<"set_recv_msg_limit">>,
	   		    attrs = [{<<"xmlns">>,?NS_RECV_LIMIT},{<<"jid">>,User},
        	 {<<"mode">>,Opts}],
     		children = []};
	_ ->
   		#xmlel{name = <<"set_recv_msg_limit">>,
    	    attrs = [{<<"xmlns">>,?NS_VER_FRI_MODE},{<<"jid">>,User},
    	     {<<"mode">>,<<"2">>}],
   			children = []}
	end.

get_hash_user_recv_limit(Server,User) ->
	case catch mod_hash_user:get_user_hash_pid(User) of
	[{Pid,_ }] ->
		case gen_server:call(Pid,get_random_user_hash_pid)  of
		Rpid when is_pid(Rpid) ->
			gen_server:call(Rpid,{get_user_recv_limit,User});
		_ ->
			get_user_recv_limit(Server,User)
		end;
	_ ->
		get_user_recv_limit(Server,User)
	end.
		
get_user_recv_limit(Server,User) ->
	case catch ejabberd_odbc:sql_query(Server,
		[<<"select rec_msg_opt,version from recv_msg_option where username = '">>,User,<<"';">>]) of
	{selected, _ , [[R,V]]} ->
			Recv_msg_limit  = #recv_msg_limit{user = User,mode = R,version = V},
			catch ets:insert(recv_msg_limit,Recv_msg_limit),
			Recv_msg_limit;
	_ ->
		[]
	end.

set_hash_user_recv_limit(Server,User,Mode) ->
	case catch mod_hash_user:get_user_hash_pid(User) of
	[{Pid,_ }] ->
		case gen_server:call(Pid,get_random_user_hash_pid)  of
		Rpid when is_pid(Rpid) ->
			gen_server:call(Rpid,{set_user_recv_limit,User,Mode});
		_ ->
			set_user_recv_limit(Server,User,Mode)
		end;
	_ ->
		set_user_recv_limit(Server,User,Mode)
	end.




set_user_recv_limit(Server,User,Mode) ->	
	case Mode of 
	<<"0">> ->
		?INFO_MSG("User ~p set Mode 0 ,refused All message",[User]);
	_ ->
		ok
	end,
	case catch ejabberd_odbc:sql_query(Server,
		[<<"update recv_msg_option set rec_msg_opt = ">>,Mode,<<"  where username = '">>,User,<<"';">>]) of
	{updated,1} ->
		ets:insert(recv_msg_limit,#recv_msg_limit{user = User,mode = Mode,version = <<"1">>}),
		true;
	_ ->
		case catch ejabberd_odbc:sql_query(Server,
			[<<"insert into recv_msg_option(username,rec_msg_opt,version) values ('">>,User,<<"',">>,Mode,<<",1)">>]) of
		{updated,1} ->
			ets:insert(recv_msg_limit,#recv_msg_limit{user = User,mode = Mode,version = <<"1">>}),
			true;
		_ ->
			false
		end
	end.

update_all_recv_msg_limit(Server) ->
	catch ets:delete_all_objects(recv_msg_limit),
	case catch ejabberd_odbc:sql_query(Server,
		[<<"select user,rec_msg_opt,version from recv_msg_option;">>]) of
	{selected, _ , Res} when is_list(Res) ->
	    Pid = whereis(mod_hash_user),
		lists:foreach(fun([U,R,V]) ->
			case mod_hash_user:get_user_hash_pid(U) of
			[{Pid,_ }] ->
				Recv_msg_limit  = #recv_msg_limit{user = U,mode = R,version = V},
				catch ets:insert(recv_msg_limit,Recv_msg_limit);
			_ ->
				ok
			end end,Res);
	_ ->
		ok
	end.
