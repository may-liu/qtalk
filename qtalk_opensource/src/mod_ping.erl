%%%----------------------------------------------------------------------
%%% File    : mod_ping.erl
%%% Author  : Brian Cully <bjc@kublai.com>
%%% Purpose : Support XEP-0199 XMPP Ping and periodic keepalives
%%% Created : 11 Jul 2009 by Brian Cully <bjc@kublai.com>
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

-module(mod_ping).

-author('bjc@kublai.com').

-behavior(gen_mod).

-behavior(gen_server).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

-define(SUPERVISOR, ejabberd_sup).

-define(DEFAULT_SEND_PINGS, false).

-define(DEFAULT_PING_INTERVAL, 60).

-define(DICT, dict).

%% API
-export([start_link/2, start_ping/2, stop_ping/2]).

%% gen_mod callbacks
-export([start/2, stop/1]).

%% gen_server callbacks
-export([init/1, terminate/2, handle_call/3,
	 handle_cast/2, handle_info/2, code_change/3]).

%% Hook callbacks
-export([iq_ping/3, user_online/3, user_offline/3,get_key/3,set_blocked_user/3,
	 cancel_blocked_user/3,get_recent_contact/3,user_send/3,get_muc_contact/3]).

-export([mac_push_notice/3,get_mac_push_notice/1,set_mac_push_notice/2,cancel_mac_push_notice/2]).

-record(state,
	{host = <<"">>,
         send_pings = ?DEFAULT_SEND_PINGS :: boolean(),
	 ping_interval = ?DEFAULT_PING_INTERVAL :: non_neg_integer(),
	 timeout_action = none :: none | kill,
         timers = (?DICT):new() :: dict()}).
-record(mac_push_notice,{user}).

%%====================================================================
%% API
%%====================================================================
start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:start_link({local, Proc}, ?MODULE,
			  [Host, Opts], []).

start_ping(Host, JID) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:cast(Proc, {start_ping, JID}).

stop_ping(Host, JID) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:cast(Proc, {stop_ping, JID}).

%%====================================================================
%% gen_mod callbacks
%%====================================================================
start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    PingSpec = {Proc, {?MODULE, start_link, [Host, Opts]},
		transient, 2000, worker, [?MODULE]},
    supervisor:start_child(?SUPERVISOR, PingSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:call(Proc, stop),
    supervisor:delete_child(?SUPERVISOR, Proc).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Host, Opts]) ->
    SendPings = gen_mod:get_opt(send_pings, Opts,
                                fun(B) when is_boolean(B) -> B end,
				?DEFAULT_SEND_PINGS),
    PingInterval = gen_mod:get_opt(ping_interval, Opts,
                                   fun(I) when is_integer(I), I>0 -> I end,
				   ?DEFAULT_PING_INTERVAL),
    TimeoutAction = gen_mod:get_opt(timeout_action, Opts,
                                    fun(none) -> none;
                                       (kill) -> kill
                                    end, none),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             no_queue),
    mod_disco:register_feature(Host, ?NS_PING),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
				  ?NS_PING, ?MODULE, iq_ping, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_PING, ?MODULE, iq_ping, IQDisc),

    case SendPings of
      true ->
	  ejabberd_hooks:add(sm_register_connection_hook, Host,
			     ?MODULE, user_online, 100),
	  ejabberd_hooks:add(sm_remove_connection_hook, Host,
			     ?MODULE, user_offline, 100),
	  ejabberd_hooks:add(user_send_packet, Host, ?MODULE,
			     user_send, 100);
      _ -> ok
    end,
    {ok,
     #state{host = Host, send_pings = SendPings,
	    ping_interval = PingInterval,
	    timeout_action = TimeoutAction,
	    timers = (?DICT):new()}}.

terminate(_Reason, #state{host = Host}) ->
    ejabberd_hooks:delete(sm_remove_connection_hook, Host,
			  ?MODULE, user_offline, 100),
    ejabberd_hooks:delete(sm_register_connection_hook, Host,
			  ?MODULE, user_online, 100),
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE,
			  user_send, 100),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
				     ?NS_PING),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,
				     ?NS_PING),
    mod_disco:unregister_feature(Host, ?NS_PING).

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Req, _From, State) ->
    {reply, {error, badarg}, State}.

handle_cast({start_ping, JID}, State) ->
    Timers = add_timer(JID, State#state.ping_interval,
		       State#state.timers),
    {noreply, State#state{timers = Timers}};
handle_cast({stop_ping, JID}, State) ->
    Timers = del_timer(JID, State#state.timers),
    {noreply, State#state{timers = Timers}};
handle_cast({iq_pong, JID, timeout}, State) ->
    Timers = del_timer(JID, State#state.timers),
    ejabberd_hooks:run(user_ping_timeout, State#state.host,
		       [JID]),
    case State#state.timeout_action of
      kill ->
	  #jid{user = User, server = Server,
	       resource = Resource} =
	      JID,
	  case ejabberd_sm:get_session_pid(User, Server, Resource)
	      of
	    Pid when is_pid(Pid) -> ejabberd_c2s:stop(Pid);
	    _ -> ok
	  end;
      _ -> ok
    end,
    {noreply, State#state{timers = Timers}};
handle_cast(_Msg, State) -> {noreply, State}.

handle_info({timeout, _TRef, {ping, JID}}, State) ->
    IQ = #iq{type = get,
	     sub_el =
		 [#xmlel{name = <<"ping">>,
			 attrs = [{<<"xmlns">>, ?NS_PING}], children = []}]},
    Pid = self(),
    F = fun (Response) ->
		gen_server:cast(Pid, {iq_pong, JID, Response})
	end,
    From = jlib:make_jid(<<"">>, State#state.host, <<"">>),
    ejabberd_local:route_iq(From, JID, IQ, F),
    Timers = add_timer(JID, State#state.ping_interval,
		       State#state.timers),
    {noreply, State#state{timers = Timers}};
handle_info(_Info, State) -> {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%====================================================================
%% Hook callbacks
%%====================================================================
iq_ping(_From, _To,
	#iq{type = Type, sub_el = SubEl} = IQ) ->
    case {Type, SubEl} of
      {get, #xmlel{name = <<"ping">>}} ->
	  IQ#iq{type = result, sub_el = []};
      _ ->
	  IQ#iq{type = error,
		sub_el = [SubEl, ?ERR_FEATURE_NOT_IMPLEMENTED]}
    end.

get_key(From, _To,
	#iq{type = Type, sub_el = SubEl} = IQ) ->
    case {Type, SubEl} of
      {get, #xmlel{name = <<"key">>}} ->
	  IQ#iq{type = result, sub_el = [make_iq_key_reply(From)]};
      _ ->
	  IQ#iq{type = error,
		sub_el = [SubEl, ?ERR_FEATURE_NOT_IMPLEMENTED]}
    end.
mac_push_notice(From, _To,
	#iq{type = Type, sub_el = SubEl} = IQ) 	->
	case {Type, SubEl} of
	{get, #xmlel{name = <<"mac_push_notice">>}} ->
		IQ#iq{type = result, sub_el = [get_mac_push_notice(From)]};
	{set, #xmlel{name = <<"mac_push_notice">>}} ->
		IQ#iq{type = result, sub_el = [set_mac_push_notice(From,SubEl)]};
	{set, #xmlel{name = <<"cancel_mac_push_notice">>}} ->
		IQ#iq{type = result, sub_el = [cancel_mac_push_notice(From,SubEl)]};
	_ ->
		IQ#iq{type = error,sub_el = [SubEl, ?ERR_FEATURE_NOT_IMPLEMENTED]}
	end.

set_blocked_user(From, _To,
	#iq{type = Type, sub_el = SubEl} = IQ) ->
	case {Type,SubEl} of
	  {set,#xmlel{name = <<"block_user">>}} ->
	  	 IQ#iq{type = result, sub_el = [set_block_user(From,SubEl)]};
      _ ->
	  IQ#iq{type = error,
		sub_el = [SubEl, ?ERR_FEATURE_NOT_IMPLEMENTED]}
    end.

cancel_blocked_user(From, _To,
	#iq{type = Type, sub_el = SubEl} = IQ) ->
	case {Type,SubEl} of
	  {set,#xmlel{name = <<"block_user">>}} ->
	  	 IQ#iq{type = result, sub_el = [cancel_block_user(From,SubEl)]};
      _ ->
	  IQ#iq{type = error,
		sub_el = [SubEl, ?ERR_FEATURE_NOT_IMPLEMENTED]}
    end.

get_recent_contact(From, _To,
	#iq{type = Type, sub_el = SubEl} = IQ) ->
	case {Type,SubEl} of
	  {get,#xmlel{name = <<"recent_contact_user">>}} ->
	  	 IQ#iq{type = result, sub_el = [get_recent_contact_user(From)]};
      _ ->
	  IQ#iq{type = error,
		sub_el = [SubEl, ?ERR_FEATURE_NOT_IMPLEMENTED]}
    end.
		
get_muc_contact(From, _To,
	#iq{type = Type, sub_el = SubEl} = IQ) ->
	case {Type,SubEl} of
	  {get,#xmlel{name = <<"recent_contact_muc">>}} ->
	  	 IQ#iq{type = result, sub_el = [get_muc_recent_contact(From)]};
      _ ->
	  IQ#iq{type = error,
		sub_el = [SubEl, ?ERR_FEATURE_NOT_IMPLEMENTED]}
    end.

user_online(_SID, JID, _Info) ->
    start_ping(JID#jid.lserver, JID).

user_offline(_SID, JID, _Info) ->
    stop_ping(JID#jid.lserver, JID).

user_send(JID, _From, _Packet) ->
    start_ping(JID#jid.lserver, JID).

%%====================================================================
%% Internal functions
%%====================================================================
add_timer(JID, Interval, Timers) ->
    LJID = jlib:jid_tolower(JID),
    NewTimers = case (?DICT):find(LJID, Timers) of
		  {ok, OldTRef} ->
		      cancel_timer(OldTRef), (?DICT):erase(LJID, Timers);
		  _ -> Timers
		end,
    TRef = erlang:start_timer(Interval * 1000, self(),
			      {ping, JID}),
    (?DICT):store(LJID, TRef, NewTimers).

del_timer(JID, Timers) ->
    LJID = jlib:jid_tolower(JID),
    case (?DICT):find(LJID, Timers) of
      {ok, TRef} ->
	  cancel_timer(TRef), (?DICT):erase(LJID, Timers);
      _ -> Timers
    end.

cancel_timer(TRef) ->
    case erlang:cancel_timer(TRef) of
      false ->
	  receive {timeout, TRef, _} -> ok after 0 -> ok end;
      _ -> ok
    end.

make_iq_key_reply(From) ->
	Resource = From#jid.resource,
	User = From#jid.user,
	LServer = jlib:nameprep(From#jid.server),
	V =
		case redis_link:hash_get(LServer,1,binary_to_list(User),binary_to_list(Resource)) of
		{ok,undefined} ->
			<<"">>;
		{ok,Key} ->
			Key;
		_ ->
			<<"">>
		end,
	#xmlel{name = <<"key">>,
			attrs = [{<<"xmlns">>,?NS_KEY},{<<"value">>,V}],children = []}.

set_block_user(From,El) ->
	User = From#jid.user,
	LServer = jlib:nameprep(From#jid.server),
	V =
		case xml:get_tag_attr_s(<<"jid">>,El) of
	 	<<"">> -> 
			<<"Null">>;
		J ->
			case catch ejabberd_odbc:sql_query(LServer,
					[<<"insert into user_block_list(username,blockuser) values ('">>,User,<<"','">>,J,<<"');">>]) of
			{updated,1} ->
				<<"sucess">>;
			{error,Reason}  ->
				case  proplists:get_value(code,Reason) of
				<<"23505">> ->
					<<"sucess">>;
				_ ->
					<<"failed">>
				end;
			_ ->
				<<"failed">>
			end
		end,
				
    #xmlel{name = <<"block_user">>,
			attrs = [{<<"xmlns">>,?NS_SET_BLOCKED},
						{<<"result">>,V}],
			children = []}.

cancel_block_user(From,El) ->
	User = From#jid.user,
	LServer = jlib:nameprep(From#jid.server),
	V =
		case xml:get_tag_attr_s(<<"jid">>,El) of
	 	<<"">> -> 
			<<"null">>;
		J ->
			case catch ejabberd_odbc:sql_query(LServer,
					[<<"delete from user_block_list where username = '">>,User,<<"' and blockuser = '">>,J,<<"';">>]) of
			{updated,1} ->
				<<"sucess">>;
			_ ->
				<<"failed">>
			end
		end,
    #xmlel{name = <<"block_user">>,
			attrs = [{<<"xmlns">>,?NS_CANCEL_BLOCKED},
						{<<"result">>,V}],
			children = []}.

get_recent_contact_user(From) ->
	User = From#jid.user,
	LServer = jlib:nameprep(From#jid.server),
	Concat_users = 
		case catch odbc_queries:get_concats(LServer,User) of
		{selected, [<<"u">>], SRes} when is_list(SRes) ->
			lists:usort(lists:concat(SRes));
		_ ->
			[]
		end,
	Block_users = 
		case catch ejabberd_odbc:sql_query(LServer,
			[<<"select blockuser from user_block_list where username = '">>,User,<<"';">>]) of
		{selected, [<<"blockuser">>], SRes1} when is_list(SRes1) ->
			lists:concat(SRes1);
		A  ->
			?DEBUG("A = ~p ~n",[A]),
			[]
		end,
	Final_users = 
		lists:foldl(fun(U,Acc) ->
			case lists:member(U,Acc) of
			true ->
				lists:delete(U,Acc);
			_ ->
				Acc
			end end,Concat_users,Block_users),
				
	#xmlel{name = <<"recent_contact_user">>,
			attrs = [{<<"xmlns">>,?NS_RECENT_CONTACT},
						{<<"conctat_user">>,list_to_binary(spell_user(Final_users))},{<<"block_user">>,list_to_binary(spell_user(Block_users))}],
			children = []}.

get_muc_recent_contact(From) ->
	User = From#jid.user,
	LServer = jlib:nameprep(From#jid.server),
	Rooms = 
		case catch odbc_queries:get_muc_concats(LServer,User) of
		{selected, [<<"muc_name">>], SRes} when is_list(SRes) ->
			lists:usort(lists:concat(SRes));
		_ ->
			[]
		end,
	#xmlel{name = <<"recent_contact_muc">>,
			attrs = [{<<"xmlns">>,?NS_MUC_CONTACT},
						{<<"conctat_rooms">>,list_to_binary(spell_user(Rooms))}],
			children = []}.

	
spell_user(User) ->
	lists:foldl(fun(U,Acc) ->
		case Acc of 
		[] ->
			[U];
		_ ->
			lists:concat([Acc,[<<",">>,U]])
		 end end,[],User).

get_mac_push_notice(From) ->
	User = From#jid.user,
	LServer = jlib:nameprep(From#jid.server),
	UserList = 
		case catch ets:select(mac_push_notice, [{#mac_push_notice{user = {'$1','$2'}, _ = '_'},[{'==', '$1', User}], ['$2']}]) of
		[] ->
			[];
		UL when is_list(UL) ->
			UL;
		_ ->
			[]
		end,
    #xmlel{name = <<"mac_push_notice">>,
			attrs = [{<<"xmlns">>,?NS_MAC_PUSH_NOTICE},
		    {<<"shield_user">>,list_to_binary(spell_user(UserList))}],
		    children = []}.

set_mac_push_notice(From,El) ->
	User = From#jid.user,
	LServer = jlib:nameprep(From#jid.server),
	V =
		case xml:get_tag_attr_s(<<"jid">>,El) of
	 	<<"">> -> 
			<<"failed">>;
		J ->
			case ets:lookup(mac_push_notice,{User,J}) of
			[] ->
				catch ets:insert(mac_push_notice,#mac_push_notice{user = {User,J}}),
				case catch ejabberd_odbc:sql_query(LServer,
					[<<"insert into mac_push_notice(user_name,shield_user) values ('">>,User,<<"','">>,J,<<"');">>]) of
				{updated, 1} -> <<"sucess">>;
				_ -> <<"failed">>
				end;	
			_ ->
				<<"sucess">>
			end
		end,
    #xmlel{name = <<"mac_push_notice">>,
			attrs = [{<<"xmlns">>,?NS_MAC_PUSH_NOTICE},
						{<<"result">>,V}],
			children = []}.

cancel_mac_push_notice(From,El) ->
	User = From#jid.user,
	LServer = jlib:nameprep(From#jid.server),
	V =
		case xml:get_tag_attr_s(<<"jid">>,El) of
	 	<<"">> -> 
			<<"failed">>;
		J ->
			case catch ejabberd_odbc:sql_query(LServer,
				[<<"delete from mac_push_notice where user_name = '">>,User,<<"' and shield_user = '">>,J,<<"';">>]) of
			{updated, 1} ->
				case ets:lookup(mac_push_notice,{User,J}) of
				[] ->
					<<"sucess">>;	
				_->
					catch ets:delete(mac_push_notice,{User,J}),
					<<"sucess">>
				end;
			_ -> 
				<<"sucess">>
			end	
		end,
    #xmlel{name = <<"calcel_mac_push_notice">>,
			attrs = [{<<"xmlns">>,?NS_MAC_PUSH_NOTICE},
						{<<"result">>,V}]}.
