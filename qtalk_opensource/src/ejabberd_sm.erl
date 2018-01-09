%%----------------------------------------------------------------------
%%% File    : ejabberd_sm.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Session manager
%%% Created : 24 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
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

-module(ejabberd_sm).

-author('alexey@process-one.net').

-behaviour(gen_server).

%% API
-export([start_link/0,
	 route/3,
	 open_session/5,
	 open_session/6,
	 close_session/4,
	 check_in_subscription/6,
	 bounce_offline_message/3,
	 disconnect_removed_user/2,
	 get_user_resources/2,
	 get_user_present_resources/2,
	 set_presence/7,
	 unset_presence/6,
	 close_session_unset_presence/5,
	 dirty_get_sessions_list/0,
	 dirty_get_my_sessions_list/0,
	 get_vh_session_list/1,
	 get_vh_session_number/1,
	 register_iq_handler/4,
	 register_iq_handler/5,
	 unregister_iq_handler/2,
	 force_update_presence/1,
	 connected_users/0,
	 connected_users_number/0,
	 user_resources/2,
	 kick_user/2,
	 get_session_pid/3,
	 get_user_info/3,
	 get_user_ip/3,
	 get_max_user_sessions/2,
	 get_all_pids/0,
	 is_existing_resource/3,
	 insert_chat_msg/9,
	 record_show/4,
	 get_user_away_rescources/2,
	 get_user_session/2,
	 judge_away_flag/2,
	 add_datetime_to_packet/3,
	 add_msectime_to_packet/4,
	 timestamp_to_xml/1
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3]).

-export([get_user_present_resources_and_pid/2]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

-include("ejabberd_commands.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include("mod_privacy.hrl").

-record(session, {sid, usr, us, priority, info, show}).
-record(session_counter, {vhost, count}).
-record(state, {}).
-record(muc_online_room,
	       {name_host = {<<"">>, <<"">>} :: {binary(), binary()} | '$1' |
	                    {'_', binary()} | '_',
	 			         pid = self() :: pid() | '$2' | '_' | '$1'}).

%% default value for the maximum number of user connections
-define(MAX_USER_SESSIONS, infinity).
-define(DIRECTION, <<"recv">>).
-define(CN, <<"qchat">>).
-define(USRTYPE, <<"common">>).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
-type sid() :: {erlang:timestamp(), pid()}.
-type ip() :: {inet:ip_address(), inet:port_number()} | undefined.
-type info() :: [{conn, atom()} | {ip, ip()} | {node, atom()}
                 | {oor, boolean()} | {auth_module, atom()}].
-type prio() :: undefined | integer().

-export_type([sid/0]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [],
			  []).

-spec route(jid(), jid(), xmlel() | broadcast()) -> ok.

route(From, To, Packet) ->
    case catch do_route(From, To, Packet) of
      {'EXIT', Reason} ->
	  ?ERROR_MSG("~p~nwhen processing: ~p",
		     [Reason, {From, To, Packet}]);
      _ -> ok
    end.

-spec open_session(sid(), binary(), binary(), binary(), prio(), info()) -> ok.

open_session(SID, User, Server, Resource, Priority, Info) ->
    set_session(SID, User, Server, Resource, Priority, Info),
    mnesia:dirty_update_counter(session_counter,
				jlib:nameprep(Server), 1),
    check_for_sessions_to_replace(User, Server, Resource),
    JID = jlib:make_jid(User, Server, Resource),
    ejabberd_hooks:run(sm_register_connection_hook,
		       JID#jid.lserver, [SID, JID, Info]).

-spec open_session(sid(), binary(), binary(), binary(), info()) -> ok.

open_session(SID, User, Server, Resource, Info) ->
	
    open_session(SID, User, Server, Resource, undefined, Info).

-spec close_session(sid(), binary(), binary(), binary()) -> ok.

close_session(SID, User, Server, Resource) ->
    Info = case mnesia:dirty_read({session, SID}) of
	[] -> [];
	[#session{info=I}] -> I
    end,
    F = fun() ->
		mnesia:delete({session, SID}),
		mnesia:dirty_update_counter(session_counter,
					    jlib:nameprep(Server), -1)
	end,
    mnesia:sync_dirty(F),
    JID = jlib:make_jid(User, Server, Resource),
	
%%	catch mod_monitor:count_user_login_out(Server,User,0),
%	case catch redis_link:hash_get(Server,1,binary_to_list(User),binary_to_list(Resource)) of 
%	{ok,undefined} ->
%		ok;
%	{ok,Key} ->
%%		catch redis_link:hash_del(Server,1,binary_to_list(User),binary_to_list(Resource)),
%		catch redis_link:hash_del(Server,2,binary_to_list(User),binary_to_list(Key));
%	_ ->
%		ok
%	end,

    ejabberd_hooks:run(sm_remove_connection_hook,
		       JID#jid.lserver, [SID, JID, Info]).

check_in_subscription(Acc, User, Server, _JID, _Type, _Reason) ->
    case ejabberd_auth:is_user_exists(User, Server) of
      true -> Acc;
      false -> {stop, false}
    end.

-spec bounce_offline_message(jid(), jid(), xmlel()) -> stop.

bounce_offline_message(From, To, Packet) ->
    Err = jlib:make_error_reply(Packet,
				?ERR_SERVICE_UNAVAILABLE),
    ejabberd_router:route(To, From, Err),
    stop.

-spec disconnect_removed_user(binary(), binary()) -> ok.

disconnect_removed_user(User, Server) ->
    ejabberd_sm:route(jlib:make_jid(<<"">>, <<"">>, <<"">>),
		      jlib:make_jid(User, Server, <<"">>),
                      {broadcast, {exit, <<"User removed">>}}).

get_user_resources(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    case catch mnesia:dirty_index_read(session, US, #session.us) of
	{'EXIT', _Reason} ->
	    [];
	Ss ->
	    [element(3, S#session.usr) || S <- clean_session_list(Ss)]
    end.

-spec get_user_present_resources(binary(), binary()) -> [tuple()].

get_user_present_resources(LUser, LServer) ->
    US = {LUser, LServer},
    case catch mnesia:dirty_index_read(session, US,
				       #session.us)
	of
      {'EXIT', _Reason} -> [];
      Ss ->
	  [{S#session.priority, element(3, S#session.usr)}
	   || S <- clean_session_list(Ss),
	      is_integer(S#session.priority)]
    end.

get_user_present_resources_and_pid(LUser, LServer) ->
    US = {LUser, LServer},
    case catch mnesia:dirty_index_read(session, US,
				       #session.us)
	of
      {'EXIT', _Reason} -> [];
      Ss ->
	  [{S#session.priority, element(3, S#session.usr),element(2, S#session.sid)}
	   || S <- clean_session_list(Ss),
	      is_integer(S#session.priority)]
    end.

get_user_session(LUser, LServer) ->
    US = {LUser, LServer},
    case catch mnesia:dirty_index_read(session, US,
				       #session.us)
	of
      {'EXIT', _Reason} -> [];
      Ss ->
	  [S  || S <- clean_session_list(Ss), is_integer(S#session.priority)]
    end.
-spec get_user_ip(binary(), binary(), binary()) -> ip().

get_user_ip(User, Server, Resource) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    LResource = jlib:resourceprep(Resource),
    USR = {LUser, LServer, LResource},
    case mnesia:dirty_index_read(session, USR, #session.usr) of
	[] ->
	    undefined;
	Ss ->
	    Session = lists:max(Ss),
	    proplists:get_value(ip, Session#session.info)
    end.

-spec get_user_info(binary(), binary(), binary()) -> info() | offline.

get_user_info(User, Server, Resource) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    LResource = jlib:resourceprep(Resource),
    USR = {LUser, LServer, LResource},
    case mnesia:dirty_index_read(session, USR, #session.usr) of
	[] ->
	    offline;
	Ss ->
	    Session = lists:max(Ss),
	    Node = node(element(2, Session#session.sid)),
	    Conn = proplists:get_value(conn, Session#session.info),
	    IP = proplists:get_value(ip, Session#session.info),
	    [{node, Node}, {conn, Conn}, {ip, IP}]
    end.

-spec set_presence(sid(), binary(), binary(), binary(),
                   prio(), xmlel(), info()) -> ok.

set_presence(SID, User, Server, Resource, Priority,
	     Presence, Info) ->
    set_session(SID, User, Server, Resource, Priority,
		Info),
    ejabberd_hooks:run(set_presence_hook,
		       jlib:nameprep(Server),
		       [User, Server, Resource, Presence]).

-spec unset_presence(sid(), binary(), binary(),
                     binary(), binary(), info()) -> ok.

unset_presence(SID, User, Server, Resource, Status,
	       Info) ->
    set_session(SID, User, Server, Resource, undefined,
		Info),
    ejabberd_hooks:run(unset_presence_hook,
		       jlib:nameprep(Server),
		       [User, Server, Resource, Status]).

-spec close_session_unset_presence(sid(), binary(), binary(),
                                   binary(), binary()) -> ok.

close_session_unset_presence(SID, User, Server,
			     Resource, Status) ->
    close_session(SID, User, Server, Resource),
    ejabberd_hooks:run(unset_presence_hook,
		       jlib:nameprep(Server),
		       [User, Server, Resource, Status]).

-spec get_session_pid(binary(), binary(), binary()) -> none | pid().

get_session_pid(User, Server, Resource) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    LResource = jlib:resourceprep(Resource),
    USR = {LUser, LServer, LResource},
    case catch mnesia:dirty_index_read(session, USR, #session.usr) of
	[#session{sid = {_, Pid}}] -> Pid;
	_ -> none
    end.

-spec dirty_get_sessions_list() -> [ljid()].

dirty_get_sessions_list() ->
    mnesia:dirty_select(
      session,
      [{#session{usr = '$1', _ = '_'},
	[],
	['$1']}]).

dirty_get_my_sessions_list() ->
    mnesia:dirty_select(
      session,
      [{#session{sid = {'_', '$1'}, _ = '_'},
	[{'==', {node, '$1'}, node()}],
	['$_']}]).

-spec get_vh_session_list(binary()) -> [ljid()].

get_vh_session_list(Server) ->
    LServer = jlib:nameprep(Server),
    mnesia:dirty_select(session,
			[{#session{usr = '$1', _ = '_'},
			  [{'==', {element, 2, '$1'}, LServer}], ['$1']}]).

-spec get_all_pids() -> [pid()].

get_all_pids() ->
    mnesia:dirty_select(
      session,
      ets:fun2ms(
        fun(#session{sid = {_, Pid}}) ->
		Pid
        end)).

get_vh_session_number(Server) ->
    LServer = jlib:nameprep(Server),
    Query = mnesia:dirty_select(
		session_counter,
		[{#session_counter{vhost = LServer, count = '$1'},
		  [],
		  ['$1']}]),
    case Query of
	[Count] ->
	    Count;
	_ -> 0
    end.

register_iq_handler(Host, XMLNS, Module, Fun) ->
    ejabberd_sm !
      {register_iq_handler, Host, XMLNS, Module, Fun}.

-spec register_iq_handler(binary(), binary(), atom(), atom(), list()) -> any().

register_iq_handler(Host, XMLNS, Module, Fun, Opts) ->
    ejabberd_sm !
      {register_iq_handler, Host, XMLNS, Module, Fun, Opts}.

-spec unregister_iq_handler(binary(), binary()) -> any().

unregister_iq_handler(Host, XMLNS) ->
    ejabberd_sm ! {unregister_iq_handler, Host, XMLNS}.


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    update_tables(),
    mnesia:create_table(session,
			[{ram_copies, [node()]},
			 {attributes, record_info(fields, session)}]),
    mnesia:create_table(session_counter,
			[{ram_copies, [node()]},
			 {attributes, record_info(fields, session_counter)}]),
    mnesia:add_table_index(session, usr),
    mnesia:add_table_index(session, us),
    mnesia:add_table_index(session, show),
    mnesia:add_table_copy(session, node(), ram_copies),
    mnesia:add_table_copy(session_counter, node(), ram_copies),
    mnesia:subscribe(system),
    ets:new(sm_iqtable, [named_table]),
    lists:foreach(
      fun(Host) ->
	      ejabberd_hooks:add(roster_in_subscription, Host,
				 ejabberd_sm, check_in_subscription, 20),
	      ejabberd_hooks:add(offline_message_hook, Host,
				 ejabberd_sm, bounce_offline_message, 100),
	      ejabberd_hooks:add(remove_user, Host,
				 ejabberd_sm, disconnect_removed_user, 100)
      end, ?MYHOSTS),
    ejabberd_commands:register_commands(commands()),

    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok, {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) -> {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({route, From, To, Packet}, State) ->
    case catch do_route(From, To, Packet) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("~p~nwhen processing: ~p",
		       [Reason, {From, To, Packet}]);
	_ ->
	    ok
    end,
    {noreply, State};
handle_info({mnesia_system_event, {mnesia_down, Node}}, State) ->
    recount_session_table(Node),
    {noreply, State};
handle_info({register_iq_handler, Host, XMLNS, Module, Function}, State) ->
    ets:insert(sm_iqtable, {{XMLNS, Host}, Module, Function}),
    {noreply, State};
handle_info({register_iq_handler, Host, XMLNS, Module,
	     Function, Opts},
	    State) ->
    ets:insert(sm_iqtable,
	       {{XMLNS, Host}, Module, Function, Opts}),
    {noreply, State};
handle_info({unregister_iq_handler, Host, XMLNS},
	    State) ->
    case ets:lookup(sm_iqtable, {XMLNS, Host}) of
      [{_, Module, Function, Opts}] ->
	  gen_iq_handler:stop_iq_handler(Module, Function, Opts);
      _ -> ok
    end,
    ets:delete(sm_iqtable, {XMLNS, Host}),
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ejabberd_commands:unregister_commands(commands()),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

set_session(SID, User, Server, Resource, Priority, Info) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    LResource = jlib:resourceprep(Resource),
    US = {LUser, LServer},
    USR = {LUser, LServer, LResource},

    F = fun () ->
		mnesia:write(#session{sid = SID, usr = USR, us = US,
				      priority = Priority, info = Info, show = <<"normal">>})
	end,
    mnesia:sync_dirty(F).

%% Recalculates alive sessions when Node goes down 
%% and updates session and session_counter tables 
recount_session_table(Node) ->
    F = fun() ->
		Es = mnesia:select(
		       session,
		       [{#session{sid = {'_', '$1'}, _ = '_'},
			 [{'==', {node, '$1'}, Node}],
			 ['$_']}]),
		lists:foreach(fun(E) ->
				      mnesia:delete({session, E#session.sid})
			      end, Es),
		%% reset session_counter table with active sessions
		mnesia:clear_table(session_counter),
		lists:foreach(fun(Server) ->
				LServer = jlib:nameprep(Server),
				Hs = mnesia:select(session,
				    [{#session{usr = '$1', _ = '_'},
				    [{'==', {element, 2, '$1'}, LServer}],
				    ['$1']}]),
				mnesia:write(
				    #session_counter{vhost = LServer, 
						     count = length(Hs)})
			      end, ?MYHOSTS)
	end,
    mnesia:async_dirty(F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_route(From, To, {broadcast, _} = Packet) ->
    case To#jid.lresource of
        <<"">> ->
            lists:foreach(fun(R) ->
                                  do_route(From,
                                           jlib:jid_replace_resource(To, R),
                                           Packet)
                          end,
                          get_user_resources(To#jid.user, To#jid.server));
        _ ->
            USR = jlib:jid_tolower(To),
            case mnesia:dirty_index_read(session, USR, #session.usr) of
                [] ->
                    ?DEBUG("packet dropped~n", []);
                Ss ->
                    Session = lists:max(Ss),
                    Pid = element(2, Session#session.sid),
                    ?DEBUG("sending to process ~p~n", [Pid]),
                    Pid ! {route, From, To, Packet}
            end
    end;
do_route(From, To, #xmlel{} = Packet) ->
%   ?DEBUG("session manager~n\tfrom ~p~n\tto ~p~n\tpacket "
%	   "~P~n",
%	   [From, To, Packet, 8]),
    #jid{user = User, server = Server,
	 luser = LUser, lserver = LServer, lresource = LResource} = To,
	#xmlel{name = Name, attrs = Attrs, children = Els} = Packet,
    case LResource of
      <<"">> ->
	  case Name of
	    <<"presence">> ->
		{Pass, _Subsc} = case xml:get_attr_s(<<"type">>, Attrs)
				     of
				   <<"subscribe">> ->
				       Reason = xml:get_path_s(Packet,
							       [{elem,
								 <<"status">>},
								cdata]),
				       {is_privacy_allow(From, To, Packet)
					  andalso
					  ejabberd_hooks:run_fold(roster_in_subscription,
								  LServer,
								  false,
								  [User, Server,
								   From,
								   subscribe,
								   Reason]),
					true};
				   <<"subscribed">> ->
				       {is_privacy_allow(From, To, Packet)
					  andalso
					  ejabberd_hooks:run_fold(roster_in_subscription,
								  LServer,
								  false,
								  [User, Server,
								   From,
								   subscribed,
								   <<"">>]),
					true};
				   <<"unsubscribe">> ->
				       {is_privacy_allow(From, To, Packet)
					  andalso
					  ejabberd_hooks:run_fold(roster_in_subscription,
								  LServer,
								  false,
								  [User, Server,
								   From,
								   unsubscribe,
								   <<"">>]),
					true};
				   <<"unsubscribed">> ->
				       {is_privacy_allow(From, To, Packet)
					  andalso
					  ejabberd_hooks:run_fold(roster_in_subscription,
								  LServer,
								  false,
								  [User, Server,
								   From,
								   unsubscribed,
								   <<"">>]),
					true};
				   _ -> {true, false}
				 end,
		if Pass ->
			handle_presence(LServer,From,To,Packet,Attrs);
		   true -> ok
		end;
	    <<"message">> -> 
			route_message(From, To, Packet);
	    <<"iq">> -> process_iq(From, To, Packet);
	    _ -> ok
	  end;
      _ ->
	  USR = {LUser, LServer, LResource},
	  case mnesia:dirty_index_read(session, USR, #session.usr)
	      of
	    [] ->
		case Name of
		  <<"message">> -> route_message(From, To, Packet);
		  <<"iq">> ->
		      case xml:get_attr_s(<<"type">>, Attrs) of
			<<"error">> -> ok;
			<<"result">> -> ok;
			_ ->
			    Err = jlib:make_error_reply(Packet,
							?ERR_SERVICE_UNAVAILABLE),
			    ejabberd_router:route(To, From, Err)
		      end;
		  _ -> ?DEBUG("packet droped~n", [])
		end;
	    Ss ->
		Session = lists:max(Ss),
		Pid = element(2, Session#session.sid),
		NewPacket  =
			case Name of
		   	<<"message">> ->
				make_new_packet(From,To,Packet,Name,Attrs,Els);	
			<<"iq">> ->
				insert_user_mucs(From,To,Packet),
				Packet;
	    	_ ->
				Packet
	  	  end,
		Pid ! {route, From, To, NewPacket}
		end
   	 end.

%% The default list applies to the user as a whole,
%% and is processed if there is no active list set
%% for the target session/resource to which a stanza is addressed,
%% or if there are no current sessions for the user.
is_privacy_allow(From, To, Packet) ->
    User = To#jid.user,
    Server = To#jid.server,
    PrivacyList =
	ejabberd_hooks:run_fold(privacy_get_user_list, Server,
				#userlist{}, [User, Server]),
    is_privacy_allow(From, To, Packet, PrivacyList).

%% Check if privacy rules allow this delivery
%% Function copied from ejabberd_c2s.erl
is_privacy_allow(From, To, Packet, PrivacyList) ->
    User = To#jid.user,
    Server = To#jid.server,
    allow ==
      ejabberd_hooks:run_fold(privacy_check_packet, Server,
			      allow,
			      [User, Server, PrivacyList, {From, To, Packet},
			       in]).

route_message(From, To, Packet) ->
    LUser = To#jid.luser,
    LServer = To#jid.lserver,
	case xml:get_tag_attr_s(<<"type">>, Packet) of
	<<"readmark">> -> 
		readmark:readmark_message(From,To,Packet);
	<<"revoke">> ->
		revoke:revoke_message(From,To,Packet);
	_ ->
    	PrioRes = get_user_present_resources(LUser, LServer),
		#xmlel{name = Name, attrs = Attrs, children = Els} = Packet,
		NewPacket = make_new_packet(From,To,Packet,Name,Attrs,Els),
    	case catch lists:max(PrioRes) of
    	  {Priority, _R}
		  when is_integer(Priority), Priority >= 0 ->
		  	catch send_max_priority_msg(LUser,LServer,Priority,From,To,NewPacket,PrioRes);
   	   _ ->
		  case xml:get_tag_attr_s(<<"type">>, Packet) of
		    <<"error">> -> ok;
		    <<"groupchat">> -> 	ok;
			<<"subscription">>->
				subscription:subscription_message(From,To,Packet);
			<<"readmark">> ->
				readmark:readmark_message(From,To,Packet);	
            <<"consult">> ->
                consult_message(From,To,Packet);
		    _ ->
				case ejabberd_auth:is_user_exists(LUser, LServer) of
				true ->
					case is_privacy_allow(From, To, Packet) of
					true ->
							case catch  check_carbon_msg(Packet) of
							true ->
								ok;
							_ ->
				    			ejabberd_hooks:run(offline_message_hook, LServer,
					    	   	[From, To, Packet])
							end;
                    _ ->
                            ok
                    end;
			  _ ->
			      Err = jlib:make_error_reply(Packet,
							  ?ERR_SERVICE_UNAVAILABLE),
			      ejabberd_router:route(To, From, Err)
			end
		  end
    	end
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clean_session_list(Ss) ->
    clean_session_list(lists:keysort(#session.usr, Ss), []).

clean_session_list([], Res) -> Res;
clean_session_list([S], Res) -> [S | Res];
clean_session_list([S1, S2 | Rest], Res) ->
    if S1#session.usr == S2#session.usr ->
	   if S1#session.sid > S2#session.sid ->
		  clean_session_list([S1 | Rest], Res);
	      true -> clean_session_list([S2 | Rest], Res)
	   end;
       true -> clean_session_list([S2 | Rest], [S1 | Res])
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% On new session, check if some existing connections need to be replace
check_for_sessions_to_replace(User, Server, Resource) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    LResource = jlib:resourceprep(Resource),
    check_existing_resources(LUser, LServer, LResource),
    check_max_sessions(LUser, LServer).

check_existing_resources(LUser, LServer, LResource) ->
    SIDs = get_resource_sessions(LUser, LServer, LResource),
    if SIDs == [] -> ok;
       true ->
	   MaxSID = lists:max(SIDs),
	   lists:foreach(fun ({_, Pid} = S) when S /= MaxSID ->
				 Pid ! replaced;
			     (_) -> ok
			 end,
			 SIDs)
    end.

-spec is_existing_resource(binary(), binary(), binary()) -> boolean().

is_existing_resource(LUser, LServer, LResource) ->
    [] /= get_resource_sessions(LUser, LServer, LResource).

get_resource_sessions(User, Server, Resource) ->
    USR = {jlib:nodeprep(User), jlib:nameprep(Server),
	   jlib:resourceprep(Resource)},
    mnesia:dirty_select(session,
			[{#session{sid = '$1', usr = USR, _ = '_'}, [],
			  ['$1']}]).

check_max_sessions(LUser, LServer) ->
    SIDs = mnesia:dirty_select(session,
			       [{#session{sid = '$1', us = {LUser, LServer},
					  _ = '_'},
				 [], ['$1']}]),
    MaxSessions = get_max_user_sessions(LUser, LServer),
    if length(SIDs) =< MaxSessions -> ok;
       true -> {_, Pid} = lists:min(SIDs), Pid ! replaced
    end.

%% Get the user_max_session setting
%% This option defines the max number of time a given users are allowed to
%% log in
%% Defaults to infinity
get_max_user_sessions(LUser, Host) ->
    case acl:match_rule(Host, max_user_sessions,
			jlib:make_jid(LUser, Host, <<"">>))
	of
      Max when is_integer(Max) -> Max;
      infinity -> infinity;
      _ -> ?MAX_USER_SESSIONS
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_iq(From, To, Packet) ->
    IQ = jlib:iq_query_info(Packet),
    case IQ of
      #iq{xmlns = XMLNS} ->
	  Host = To#jid.lserver,
	  case ets:lookup(sm_iqtable, {XMLNS, Host}) of
	    [{_, Module, Function}] ->
		ResIQ = Module:Function(From, To, IQ),
		if ResIQ /= ignore ->
		       ejabberd_router:route(To, From, jlib:iq_to_xml(ResIQ));
		   true -> ok
		end;
	    [{_, Module, Function, Opts}] ->
		gen_iq_handler:handle(Host, Module, Function, Opts,
				      From, To, IQ);
	    [] ->
		Err = jlib:make_error_reply(Packet,
					    ?ERR_SERVICE_UNAVAILABLE),
		ejabberd_router:route(To, From, Err)
	  end;
      reply -> insert_user_mucs(From,To,Packet), ok;
      _ ->
	  Err = jlib:make_error_reply(Packet, ?ERR_BAD_REQUEST),
	  ejabberd_router:route(To, From, Err),
	  ok
    end.

-spec force_update_presence({binary(), binary()}) -> any().

force_update_presence({LUser, _LServer} = US) ->
    case catch mnesia:dirty_index_read(session, US,
				       #session.us)
	of
      {'EXIT', _Reason} -> ok;
      Ss ->
	  lists:foreach(fun (#session{sid = {_, Pid}}) ->
				Pid ! {force_update_presence, LUser}
			end,
			Ss)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ejabberd commands

commands() ->
    [#ejabberd_commands{name = connected_users,
			tags = [session],
			desc = "List all established sessions",
			module = ?MODULE, function = connected_users, args = [],
			result = {connected_users, {list, {sessions, string}}}},
     #ejabberd_commands{name = connected_users_number,
			tags = [session, stats],
			desc = "Get the number of established sessions",
			module = ?MODULE, function = connected_users_number,
			args = [], result = {num_sessions, integer}},
     #ejabberd_commands{name = user_resources,
			tags = [session],
			desc = "List user's connected resources",
			module = ?MODULE, function = user_resources,
			args = [{user, binary}, {host, binary}],
			result = {resources, {list, {resource, string}}}},
     #ejabberd_commands{name = kick_user,
			tags = [session],
			desc = "Disconnect user's active sessions",
			module = ?MODULE, function = kick_user,
			args = [{user, binary}, {host, binary}],
			result = {num_resources, integer}}].

-spec connected_users() -> [binary()].

connected_users() ->
    USRs = dirty_get_sessions_list(),
    SUSRs = lists:sort(USRs),
    lists:map(fun ({U, S, R}) -> <<U/binary, $@, S/binary, $/, R/binary>> end,
	      SUSRs).

connected_users_number() ->
    length(dirty_get_sessions_list()).

user_resources(User, Server) ->
    Resources = get_user_resources(User, Server),
    lists:sort(Resources).

kick_user(User, Server) ->
    Resources = get_user_resources(User, Server),
    lists:foreach(
	fun(Resource) ->
		PID = get_session_pid(User, Server, Resource),
		PID ! disconnect
	end, Resources),
    length(Resources).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Update Mnesia tables

update_tables() ->
    case catch mnesia:table_info(session, attributes) of
      [ur, user, node] -> mnesia:delete_table(session);
      [ur, user, pid] -> mnesia:delete_table(session);
      [usr, us, pid] -> mnesia:delete_table(session);
      [usr, us, sid, priority, info] -> mnesia:delete_table(session);
      [usr, us, sid, priority, info, show] -> mnesia:delete_table(session);
      [sid, usr, us, priority] ->
	  mnesia:delete_table(session);
%%      [sid, usr, us, priority, info] -> ok;
      [sid, usr, us, priority, info] ->
		mnesia:delete_table(session);
      [sid, usr, us, priority, info,show] -> ok;
      {'EXIT', _} -> ok
    end,
    case lists:member(presence, mnesia:system_info(tables))
	of
      true -> mnesia:delete_table(presence);
      false -> ok
    end,
    case lists:member(local_session, mnesia:system_info(tables)) of
	true ->
	    mnesia:delete_table(local_session);
	false ->
	    ok
    end.


insert_chat_msg(Server,From, To,From_host,To_host, Msg,_Body,ID,InsertTime) ->
    case jlib:nodeprep(From) of
    error -> {error, invalid_jid};
    LUser ->
	         LFrom = ejabberd_odbc:escape(LUser),
	         LTo = ejabberd_odbc:escape(To),
	         LBody = ejabberd_odbc:escape(Msg),
	         LID = ejabberd_odbc:escape(ID),
	         LServer = get_server(From_host,To_host),
			 Time = ejabberd_public:pg2timestamp(InsertTime),
			 case str:str(LID,<<"http">>)  of 
			 0 ->
			 	case catch odbc_queries:insert_msg6(LServer, LFrom,LTo,From_host,To_host,LBody,LID,Time) of
	         	{updated, 1} -> {atomic, ok};
             	A ->
					?INFO_MSG("Insert Msg error Body: ~p ,~p ~n",[A,LBody]),
					{atomic, exists}
	        	 end;
			_ ->
			 	case ejabberd_public:judge_spec_jid(LFrom,LTo) of
				true ->
					case catch odbc_queries:insert_msg4(LServer, LFrom,LTo,From_host,To_host,LBody,LID,Time) of
					{updated, 1} -> {atomic, ok};
					_ ->
						?INFO_MSG("Insert Msg error Body: ~p ~n",[LBody]),
						{atomic, exists}
					end;
				_ ->
					case catch odbc_queries:insert_msg5(LServer, LFrom,LTo,From_host,To_host,LBody,LID,Time) of
					 {updated, 1} -> {atomic, ok};
					_ ->
						?INFO_MSG("Insert Msg error Body: ~p ~n",[LBody]),
						{atomic, exists}
					end
				end
			end
	 end.

timestamp_to_xml({{Year, Month, Day},
		          {Hour, Minute, Second}}) ->
    #xmlel{name = <<"stime">>,
	       attrs =   
              [{<<"xmlns">>, ?NS_TIME91},
	           {<<"stamp">>,iolist_to_binary(
					io_lib:format("~4..0w~2..0w~2..0wT~2..0w:~2..0w:~2..0w",[Year, Month, Day, Hour, Minute,Second]))}],children = []}.

record_show(User,Server,Resource,Show) ->
	LUser = jlib:nodeprep(User),
	LServer = jlib:nameprep(Server),
	LResource = jlib:resourceprep(Resource),
	US = {LUser, LServer},
	USR = {LUser, LServer, LResource},
	case mnesia:dirty_index_read(session, USR, #session.usr) of
	[] ->
		ok;
	Ss ->
		Session = lists:max(Ss),
    	F = fun () ->
			mnesia:write(Session#session{usr = USR,us = US, show = Show}) end,
		mnesia:sync_dirty(F)
	end.

%%判读特殊情况，聊天室用户非正常退出，导致存在于聊天室中的#state.users表中，
%%多域情况下给to用户会重复发送消息
judge_to_user_available(_FServer,<<"">>,_R)->
	true;
judge_to_user_available(FServer,Rescource,R) ->
	case str:str(FServer,<<"conference">>) of 
	0 ->
		true;
	_ ->
		if Rescource == R ->
			true;
		true ->
			false
		end
	end.
		
send_muc_room_remove_unavailable_user(From,To) ->
	case mnesia:dirty_read(muc_online_room, {From#jid.user, From#jid.server}) of
	[] ->
		ok;
	[Muc] ->
		Muc_Pid = Muc#muc_online_room.pid,
		Muc_Pid ! {delete_unavailable_user,To}
	end.

get_user_away_rescources(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    case catch mnesia:dirty_index_read(session, US, #session.us) of
	{'EXIT', _Reason} ->
	    [];
	[] ->
		[<<"none">>];
	Ss ->
		lists:flatmap(fun(S) ->
			case S#session.show of 
			<<"away">> ->
			[element(3, S#session.usr)];
			_ ->
				[]
			end
		end,clean_session_list(Ss))
    end.

insert_away_spool(From,To,LServer,Packet) ->
	FromUsername = ejabberd_odbc:escape(From#jid.luser),
	Username = ejabberd_odbc:escape(To#jid.luser),
	#xmlel{name = Name,attrs = Attrs, children = Els} = Packet,	
	TimeStamp = os:timestamp(),
	NewPacket = #xmlel{name = Name,attrs = Attrs,
						children = 
						   	Els ++ [jlib:timestamp_to_xml(calendar:now_to_universal_time(TimeStamp),
									utc, jlib:make_jid(<<"">>,From#jid.lserver ,<<"">>),
									<<"Offline Storage">>),
						jlib:timestamp_to_xml(calendar:now_to_universal_time(TimeStamp))]},
	case catch ets:lookup(mac_push_notice,{Username,FromUsername}) of
	[] ->
			catch odbc_queries:add_spool_away(LServer,FromUsername,Username,
					ejabberd_odbc:escape(xml:element_to_binary(NewPacket)),<<"0">>);
	_ ->
			catch odbc_queries:add_spool_away(LServer,FromUsername,Username,
					ejabberd_odbc:escape(xml:element_to_binary(NewPacket)),<<"1">>)
	end.

try_insert_msg(From,To,Packet,Mbody) ->
    #xmlel{name = Name, attrs = Attrs, children = Els} = Packet,
    LServer = To#jid.lserver,
    case  ejabberd_public:is_conference_server(From#jid.lserver) of
    false ->
        insert_user_chat_msg(From,To,Packet,Attrs,Els,Mbody,LServer);
    _ ->
        insert_muc_chat_msg(From,To,Packet,Name,Attrs,Els,Mbody,LServer)
    end.

insert_user_chat_msg(From,To,Packet,Attrs,Els,Mbody,LServer) ->
	catch mod_monitor:monitor_count(LServer,<<"chat_message">>,1),
	Carbon = xml:get_attr_s(<<"carbon_message">>, Attrs),
    	Now = mod_time:get_exact_timestamp(),
	
    case Carbon =/= <<"true">> andalso (not mod_muc_room:is_invitation(Els)) of
	true  ->
		Msg_id = xml:get_tag_attr_s(<<"id">>,xml:get_subtag(Packet,<<"body">>)),
		insert_chat_msg(LServer,From#jid.user,To#jid.user,From#jid.lserver,To#jid.lserver, xml:element_to_binary(
					Packet),Mbody,Msg_id,Now);
	_ ->
		ok
	end.

insert_muc_chat_msg(From,To,_Packet,Name,Attrs,Els,Mbody,LServer)->
	case mnesia:dirty_read(muc_online_room, {From#jid.user, From#jid.server}) of
	[] ->
		add_datetime_to_packet(Name,Attrs,Els);
	[Muc] ->
		Now = mod_time:get_exact_timestamp(),
	    Time = trunc(Now/1000),
   		NewPacket = add_datetime_to_packet(Name,[{<<"msec_times">>,integer_to_binary(Now )}] ++ Attrs,
				Els,timestamp_to_xml(mod_time:timestamp_to_datetime_utc1(Time))),
		Muc_Pid = Muc#muc_online_room.pid,
		Muc_Pid ! {insert_chat_msg,LServer,From,To,From#jid.lserver,To#jid.lserver,NewPacket,Mbody,Time},
		NewPacket
	end.


send_max_priority_msg(LUser,LServer,Priority,From,To,NewPacket,PrioRes) ->
    Status_and_Resources = send_msg_and_get_resources(LUser,LServer,Priority,From,To,NewPacket,PrioRes),
	case judge_away_flag(Status_and_Resources,false) of
	false ->
		ok;
	_ ->
		#xmlel{attrs = Attrs, children = Els} = NewPacket,
		Mtype = xml:get_attr_s(<<"type">>, Attrs),Mbody = xml:get_subtag_cdata(NewPacket, <<"body">>),
		Delay =  xml:get_subtag_cdata(NewPacket,<<"delay">>),
		Carbon_message = xml:get_attr_s(<<"carbon_message">>, Attrs),
		case (Mtype == <<"normal">> orelse Mtype == <<"chat">>) 
	   		andalso  Mbody /= <<>> andalso Delay /= <<"Offline Storage">>
			andalso Carbon_message /= <<"true">> of
		true ->
		    case (not mod_muc_room:is_invitation(Els)) andalso 
                xml:get_tag_attr_s(<<"msgType">>,xml:get_subtag(NewPacket,<<"body">>)) =/= <<"1024">> of 
			true -> insert_away_spool(From,To,LServer,NewPacket);
			_ -> ok
			end;
		_ ->
			ok
		end
	end.


send_msg_and_get_resources(LUser,LServer,Priority,From,To,NewPacket,PrioRes) ->
	lists:flatmap(fun ({P, R}) when P == Priority ->
		LResource = jlib:resourceprep(R),
		USR = {LUser, LServer, LResource},
		case mnesia:dirty_index_read(session, USR, #session.usr)   of
		[] ->
	      []; % Race condition
		Ss ->
		  	  case judge_to_user_available(From#jid.server,To#jid.lresource,LResource) of
			  true ->
			      	Session = lists:max(Ss),
			      	Pid = element(2, Session#session.sid),
			      	?DEBUG("sending to process ~p~n", [Pid]),
			      	Pid ! {route, From, To, NewPacket},
					[{Session#session.show,LResource}];
			  _ ->
				  	?INFO_MSG("Rescoure not match ~p : ~p ~n",[R,To#jid.lresource]), 
					send_muc_room_remove_unavailable_user(From,To),
				  	[]
			  end
		end;
	    %% Ignore other priority:
	    ({_Prio, _Res}) -> []
		end,PrioRes).

judge_away_flag(Resources,Offline_send_flag) ->
	{Away_lan_num,Normal_lan_num,Away_wlan_num,Normal_wlan_num} = get_resources_num(Resources,0,0,0,0),
	case Resources of
	[] ->
		case Offline_send_flag of
		true ->
			true;
		_ ->
			false
		end;
	_ ->
		case Away_wlan_num + Away_lan_num =:= 0 of
		true ->
			false;
		_ ->
			case Away_lan_num > 0 of
			true ->
				true;
			_ ->
				case Away_wlan_num =/= 0 andalso Normal_lan_num =:= 0 of 
				true ->
					true;
				_ ->
					false
				end
			end
		end
	end.

get_resources_num([],A_lan_num,N_lan_num,A_wlan_num,N_wlan_num) ->
	{A_lan_num,N_lan_num,A_wlan_num,N_wlan_num};
get_resources_num(Rs,A_lan_num,N_lan_num,A_wlan_num,N_wlan_num) ->
	[{S,R} | L ] = Rs,
	if  S =:=  <<"away">> ->
		case str:str(R,<<"iPhone">>) =/= 0 orelse str:str(R,<<"Android">>) =/= 0  orelse str:str(R,<<"IOS">>) =/= 0 of
		true ->
			get_resources_num(L, A_lan_num,N_lan_num,A_wlan_num + 1,N_wlan_num);
		_ ->
			get_resources_num(L, A_lan_num+1,N_lan_num,A_wlan_num,N_wlan_num)
		end;
	true ->
		case str:str(R,<<"iPhone">>) =/= 0 orelse str:str(R,<<"Android">>) =/= 0 orelse str:str(R,<<"IOS">>) =/= 0 of
		true ->
			get_resources_num(L, A_lan_num,N_lan_num,A_wlan_num,N_wlan_num+1);
		_ ->
			get_resources_num(L, A_lan_num,N_lan_num+1,A_wlan_num,N_wlan_num)
		end
	end.

make_new_packet(From,To,Packet,Name,Attrs,Els) ->
    Mtype = xml:get_attr_s(<<"type">>, Attrs),
    case  Mtype == <<"normal">> orelse Mtype == <<"chat">> orelse Mtype == <<"consult">> orelse  Mtype == <<"note">> of
	true ->
		Reply = xml:get_attr_s(<<"auto_reply">>, Attrs),
		Reply1 = xml:get_attr_s(<<"atuo_reply">>, Attrs),
		Mbody = xml:get_subtag_cdata(Packet, <<"body">>),
    	Delay = xml:get_subtag_cdata(Packet,<<"delay">>),
    	case Mbody =/= <<>> andalso Delay =/= <<"Offline Storage">> andalso Reply =/= <<"true">>
                andalso Reply1 =/= <<"true">> of
		true ->
            case xml:get_attr_s(<<"msec_times">>, Attrs)  of
            <<"">> -> 
			    NewPacket = add_datetime_to_packet(Name,Attrs,Els), 
                try_insert_msg(From,To,NewPacket,Mbody),
                NewPacket;
            _ ->
                ?INFO_MSG("Packet ~p ~n",[Packet]),
                Packet
            end;
		_ ->
			Packet
		end;
	_ ->
        Packet
	end.


add_datetime_to_packet(Name,Attrs,Els) ->
	Now = mod_time:get_exact_timestamp(),
    add_datetime_to_packet(Name,
		[{<<"msec_times">>,integer_to_binary(Now )}] ++ Attrs,Els,
			timestamp_to_xml(mod_time:timestamp_to_datetime_utc1(trunc(Now/1000)))).

add_datetime_to_packet(Name,Attrs,Els,Time) ->
    #xmlel{name = Name, attrs = Attrs, children =  Els ++ [Time]}.

add_msectime_to_packet(Name,Attrs,Els,Time) ->
	#xmlel{name = Name, attrs =  [{<<"msec_times">>,integer_to_binary(Time)}] ++ Attrs, 
			children =  Els ++ [timestamp_to_xml(mod_time:timestamp_to_datetime_utc1(trunc(Time/1000)))]}.

make_verify_friend_packet(Num,Rslt,From,To,Packet,Attrs) ->
	Num1  = case proplists:get_value(<<"num">>,Rslt) of
			undefined ->
				0;
			V  ->
				V
			end,
	case Num < 300 andalso Num1 < 300 of
	true ->
		case proplists:get_value(<<"mode">>,Rslt) of
		<<"0">> ->
            do_make_verify_friend_packet(?NS_VER_FRI,<<"refused">>,<<"all_refuse">>,2);
		<<"1">> ->
			{Packet,1};
		<<"2">> ->
			case xml:get_attr_s(<<"answer">>, Attrs) == proplists:get_value(<<"answer">>,Rslt) of
			true ->
        		do_make_verify_friend_packet(?NS_VER_FRI,<<"success">>,<<"answer_right">>,2) ;
			_ ->
        		do_make_verify_friend_packet(?NS_VER_FRI,<<"refused">>,<<"answer_errror">>,2)
			end;
		<<"3">> ->
                do_make_verify_friend_packet(?NS_VER_FRI,<<"success">>,<<"all_accpet">>,2);
		_ ->
       			do_make_verify_friend_packet(?NS_VER_FRI,<<"success">>,<<"default_config">>,2)
		end;
	false ->
		do_make_verify_friend_packet(?NS_VER_FRI,<<"refused">>,<<"out of max friend num">>,2)
	end.

do_make_verify_friend_packet(XMLNS,Rslt,Reason,Two_ways) ->
		{#xmlel{name = <<"presence">>,
			attrs = [{<<"xmlns">>,XMLNS},{<<"type">>,<<"handle_friend_result">>},{<<"result">>,Rslt},{<<"reason">>,Reason}],
		    	    children = []},Two_ways}.

do_make_verify_friend_packet1(XMLNS,Rslt,Reason,Two_ways) ->
		{#xmlel{name = <<"presence">>,
			attrs = [{<<"xmlns">>,XMLNS},{<<"body">>,Reason}],
		    	    children = []},Two_ways}.

send_presence_packet(From,To,Packet) ->
		PResources = get_user_present_resources(To#jid.luser, To#jid.lserver),
		lists:foreach(fun ({_, R}) ->
					     do_route(From,
						      jlib:jid_replace_resource(To,
										R),
						      Packet)
				     end,
				     PResources).

send_presence_packet1(From,To,Packet,Attrs) ->
		PResources = get_user_present_resources(To#jid.luser, To#jid.lserver),
		case PResources of 
		[] ->
			Body = xml:get_attr_s(<<"body">>, Attrs),
			catch insert_presence_spool(From#jid.server,From#jid.luser,To#jid.luser,Body);	
		_ ->
			lists:foreach(fun ({_, R}) ->
					     do_route(From,
						      jlib:jid_replace_resource(To,
										R),
						      Packet)
				     end,
				     PResources)
		end.

insert_presence_spool(Server,From,To,Body) ->
	Timestamp  = integer_to_binary(mod_time:get_timestamp()),
	case catch ejabberd_odbc:sql_query(Server,
		[<<"udpate invite_spool set  timestamp = ">>,Timestamp,<<",Body = '">>,Body,<<"' where username = '">>,To,<<"' and inviter = '">>,
		From,<<"';">>]) of
	{updated,1} ->
		ok;
	_ ->
		case catch ejabberd_odbc:sql_query(Server,
				[<<"insert into invite_spool(username,inviter,body,timestamp) values ('">>,
						To,<<"','">>,From,<<"','">>,ejabberd_odbc:escape(Body),<<"',">>,Timestamp,<<")">>]) of
		{updated,1} ->
			ok;
		_ ->
			ok
		end
	end.

delete_presence_spool(Server,From,To) ->
	case catch ejabberd_odbc:sql_query(Server,
		[<<"delete from invite_spool where username = '">>,From,<<"' and inviter = '">>,To,<<"';">>]) of
	{updated,1} ->
		ok;
	_ ->
		ok
	end.
	
handle_presence(LServer,From,To,Packet,Attrs) ->
	User = To#jid.luser,
	case xml:get_attr_s(<<"type">>, Attrs) of
	<<"verify_friend">> ->
		Rslt = mod_user_relation:do_verify_friend(LServer,User),
		Num = 
			case xml:get_attr_s(<<"friend_num">>,Attrs) of
	    	<<>> ->
				0;
			N when is_binary(N) ->
				binary_to_integer(N);
			_ ->
				0
			end,
		{NewPacket,Two_way} = make_verify_friend_packet(Num,Rslt,From,To,Packet,Attrs),
		case Two_way of
		1 ->
			send_presence_packet1(From,To,NewPacket,Attrs);
		_ ->
			ejabberd_router:route(From,jlib:jid_replace_resource(To,<<"">>),NewPacket),
			ejabberd_router:route(To,jlib:jid_replace_resource(From,<<"">>),xml:replace_tag_attr(<<"direction">>, <<"2">>, NewPacket))
		end;
	<<"manual_authentication_confirm">> ->
		case xml:get_attr_s(<<"result">>, Attrs) of
		<<"allow">> ->
			Num1 =  mod_user_relation:get_users_friend_num(LServer,User),
			Num = binary_to_integer(xml:get_attr_s(<<"friend_num">>,Attrs)),
			case Num1 < 300 andalso Num < 300 of
			true ->
				{NewPacket,_} = do_make_verify_friend_packet(?NS_VER_FRI,<<"success">>,<<"manual_authentication_confirm_success">>,1),
				ejabberd_router:route(From,jlib:jid_replace_resource(To,<<"">>),NewPacket),
				ejabberd_router:route(To,jlib:jid_replace_resource(From,<<"">>),xml:replace_tag_attr(<<"direction">>, <<"2">>, NewPacket));
			_ ->
				{NewPacket,_} = do_make_verify_friend_packet(?NS_VER_FRI,<<"refused">>,<<"out of max friend num">>,1),
				ejabberd_router:route(From,jlib:jid_replace_resource(To,<<"">>),NewPacket),
				ejabberd_router:route(To,jlib:jid_replace_resource(From,<<"">>),xml:replace_tag_attr(<<"direction">>, <<"2">>, NewPacket))
			end;
		_ ->
			ok
		end;
	<<"handle_friend_result">> ->
	 	case xml:get_attr_s(<<"result">>, Attrs) of
		<<"success">> ->
			mod_hash_user:add_user_friend(LServer,To,From,<<"1">>),
			send_presence_packet(From,To,Packet);
		_ ->
			send_presence_packet(From,To,Packet)
		end;
	<<"two_way_del">> ->
		case xml:get_attr_s(<<"result">>, Attrs) of
		<<"success">> ->
			Del_user = xml:get_attr_s(<<"jid">>, Attrs),
			Domain = xml:get_attr_s(<<"domain">>, Attrs),
			catch  mod_hash_user:handle_del_friend(To#jid.lserver,To#jid.luser,Del_user,Domain,<<"1">>),
			send_presence_packet(From,To,Packet);
		_ ->
			ok
		end;
	_ ->
		send_presence_packet(From,To,Packet) 
	end.

insert_user_mucs(From,To, #xmlel{attrs = PAttrs,children = Els}) ->
	case Els of
	[#xmlel{name = <<"del_user">>,attrs = Attrs}] ->
		case xml:get_attr_s(<<"xmlns">>, Attrs) of
		?NS_MUC_DEL_USER  ->
            catch odbc_queries:update_register_mucs(To#jid.lserver,To#jid.luser,From#jid.luser,From#jid.lserver,<<"0">>);
		_ ->
			ok
		end;
	[#xmlel{name = <<"add_user">>,attrs = Attrs}] ->
		case xml:get_attr_s(<<"xmlns">>, Attrs) of
		?NS_MUC_ADD_USER ->
            catch odbc_queries:insert_user_register_mucs(To#jid.lserver,To#jid.luser,From#jid.luser,From#jid.lserver);
		_ ->
			ok
		end;
	[#xmlel{name = <<"query">>,attrs = Attrs,children = [{xmlel,<<"set_register">>,[],[]}]}] ->
		case xml:get_attr_s(<<"xmlns">>, Attrs) of
		?NS_MUC_REGISTER ->
			case xml:get_attr_s(<<"type">>,PAttrs) of
			<<"result">> ->
                catch odbc_queries:insert_user_register_mucs(To#jid.lserver,To#jid.luser,From#jid.luser,From#jid.lserver);
			_ ->
				ok
			end;
		_ ->
			ok
		end;
	_ ->
		ok
	end.
	
get_server(From_host,To_host) ->
	if From_host =:= To_host ->
		From_host;
	true ->
		lists:nth(1,ejabberd_config:get_myhosts())
	end.

check_carbon_msg(Packet) ->
	case catch xml:get_tag_attr_s(<<"carbon_message">>, Packet) of
	<<"true">> ->
		true;
	_ ->
	    false
	end.

consult_message(From,To,Packet) ->
    {ThirdDirection, _CN, UsrType} = case xml:get_tag_attr_s(<<"channelid">>, Packet) of
    <<"">> ->
        {?DIRECTION, ?CN, ?USRTYPE};
    ChannelId ->
        {ok, {obj, ChannelIdJson}, []} = rfc4627:decode(ChannelId),
        {proplists:get_value("d", ChannelIdJson, ?DIRECTION),
         proplists:get_value("cn", ChannelIdJson, ?CN),
         proplists:get_value("usrType", ChannelIdJson, ?USRTYPE)}
    end,
    case ThirdDirection of
    <<"send">> -> 
        make_new_consult_message(From,To,Packet,UsrType);
    ?DIRECTION ->
        ok
    end.        

replace_subtag(#xmlel{name = Name} = Tag, Xmlel) ->
    Xmlel#xmlel{
        children = [Tag | lists:keydelete(Name, #xmlel.name, Xmlel#xmlel.children)]
    }. 

make_new_consult_message(From,To,Packet,UsrType) ->
    Message1 = jlib:remove_attr(<<"channelid">>, Packet),
    Channelid = rfc4627:encode({obj, [{"d", <<"recv">>}, {"cn", <<"consult">>}, {"usrType", UsrType}]}),
    RToStr = xml:get_tag_attr_s(<<"realto">>, Message1),
    RTo = jlib:string_to_jid(RToStr),
    Body = xml:get_subtag(Message1, <<"body">>),
  %%  Bid = list_to_binary("consult_" ++ uuid:to_string(uuid:random())),
    Msg_id = xml:get_tag_attr_s(<<"id">>,xml:get_subtag(Packet,<<"body">>)),
    Bid = str:concat(<<"consult_">>,Msg_id),
    NewBody = xml:replace_tag_attr(<<"id">>, Bid, Body),
    #xmlel{name = Name, attrs = Attrs, children = Children} = replace_subtag(NewBody, Message1),
    Attrs2 = jlib:replace_from_to_attrs(jlib:jid_to_string(To), RToStr, Attrs),
    NewPacket = #xmlel{name = Name, attrs = [{<<"channelid">>, Channelid}|Attrs2], children = Children},
%    ejabberd_router:route(jlib:string_to_jid(To), RTo, NewPacket),

    ejabberd_router:route(To, RTo, NewPacket).

 %   Msg_id = xml:get_tag_attr_s(<<"id">>,xml:get_subtag(Packet,<<"body">>)),
  %  Now = mod_time:get_exact_timestamp(),
   % ?DEBUG("Packet ~p ~n",[Packet]),
	%insert_chat_msg(To#jid.lserver,From#jid.user,To#jid.user,From#jid.lserver,To#jid.lserver, xml:element_to_binary(
	%				Packet),body,Msg_id,Now).
     
