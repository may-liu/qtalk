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

-module(mod_recent_contact).


-behaviour(gen_mod).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

-define(SUPERVISOR, ejabberd_sup).
-behaviour(gen_server).

%% gen_mod callbacks
-export([start/2, stop/1,start_link/2]).

-export([init/1,handle_call/3, handle_cast/2,
	        handle_info/2, terminate/2, code_change/3]).
%% Hook callbacks
-export([get_recent_contact/3,
		 handle_block_user/3]).


-record(recent_contact,{user,recent_contact_list}).
-record(block_user,{user,block_user}).
-record(state,{host,timer}).


%%=================================
%%gen_svrver callbacks
%%=================================
start_link(Host, Opts) ->
	Proc = gen_mod:get_module_proc(Host, ?MODULE),
	gen_server:start_link({local, Proc}, ?MODULE,[Host, Opts], []).
	

init([Host, Opts]) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),
	catch ets:new(recent_contact, [named_table, ordered_set, public, {keypos,#recent_contact.user}]),
	catch ets:new(block_user, [named_table, bag, public,{keypos, #block_user.user}]),
	init_block_user(Host),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
                                  ?NS_RECENT_CONTACT, ?MODULE, get_recent_contact, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
            ?NS_RECENT_CONTACT, ?MODULE, get_recent_contact, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
                                  ?NS_BLOCK, ?MODULE, handle_block_user, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
            ?NS_BLOCK, ?MODULE, handle_block_user, IQDisc),
    TRef = erlang:start_timer(7200 * 1000, self(),update),
	{ok,  #state{host = Host, timer = TRef }}.

terminate(_Reason, #state{host = Host,timer = Timer}) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
            ?NS_RECENT_CONTACT),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,
            ?NS_RECENT_CONTACT),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
            ?NS_BLOCK),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,
            ?NS_BLOCK),
   {ok, cancel} = timer:cancel(Timer).

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info({timeout, _TRef, update}, #state{host = Server}) ->
	init_block_user(Server),
    TRef = erlang:start_timer(7200 * 1000, self(),update),
	{ok,#state{host = Server, timer = TRef }};
handle_info(_Info, State) -> {noreply, State}.


code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%====================================================================
%% gen_mod callbacks
%%====================================================================
start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    Recent_contact_Spec = {Proc, {?MODULE, start_link, [Host, Opts]},
		        transient, 2000, worker, [?MODULE]},
			    supervisor:start_child(?SUPERVISOR, Recent_contact_Spec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:call(Proc, stop),
    supervisor:delete_child(?SUPERVISOR, Proc).

init_block_user(Server) ->
	LServer = ejabberd_odbc:escape(Server),
    ets:delete_all_objects(recent_contact),
    ets:delete_all_objects(block_user),
	case catch ejabberd_odbc:sql_query(LServer,[<<"select username,blockuser from user_block_list">>]) of 
	{selected, [<<"username">>,<<"blockuser">>], SRes} when is_list(SRes) ->
		lists:foreach(fun([U,B]) ->
			ets:insert(block_user,#block_user{user = U, block_user = B}) end,SRes);
	_ ->
		ok
	end.

get_recent_contact(From, _To, #iq{type = Type, sub_el = SubEl} = IQ) ->
    case {Type, SubEl} of
        {get, #xmlel{name = <<"recent_contact">>}} ->
            IQ#iq{type = result, sub_el = [get_recent_contact_users(From)]};
        _ ->
            IQ#iq{type = error,
            sub_el = [SubEl, ?ERR_FEATURE_NOT_IMPLEMENTED]}
    end.


get_recent_contact_users(From) ->
    User = From#jid.user,
    LServer = jlib:nameprep(From#jid.server),

	Rusers = handle_recent_contact(LServer,User),
	Fusers = list_to_binary(spell_user(Rusers)),
    #xmlel{name = <<"recent_contact">>,
           attrs = [{<<"xmlns">>,?NS_RECENT_CONTACT},
                    {<<"users">>,Fusers}],
           children = []}.

handle_recent_contact(Server,User) ->
	case catch ets:lookup(recent_contact,User) of
	[] ->
		get_recent_contact_user(Server,User);
	[Ru] when is_record(Ru,recent_contact) ->
		#recent_contact.recent_contact_list;
	_ ->
		[]
	end.

get_recent_contact_user(Server,User) ->
	Cuser = 
		case catch odbc_queries:get_concats(Server,User) of
		{selected, [<<"u">>], SRes} when is_list(SRes) ->
			lists:usort(lists:concat(SRes));
		 _ ->
		 	[]
		end,
	Buser = 
		case catch ets:select(block_user,[{#block_user{user = User,block_user = '$1', _ = '_'},[], ['$1']}]) of
		L when is_list(L) ->
			L;
		_ ->
			[]
		end,
	Fuser = 
		lists:foldl(fun(U,Acc) ->
			case lists:member(U,Acc) of
			true ->
				lists:delete(U,Acc);
			_ ->
		 		Acc
		   	end end,Cuser,Buser),
	ets:insert(recent_contact,#recent_contact{user = User ,recent_contact_list = Fuser}),
	Fuser.
		
spell_user(Users) ->
    lists:foldl(fun(U,Acc) ->
			case Acc of
			[] ->
				[U];
			_ ->
				lists:concat([Acc,[<<",">>,U]])
			end end,[],Users).

handle_block_user(From, _To, #iq{type = Type, sub_el = SubEl} = IQ) ->
	case {Type, SubEl} of
	{get, #xmlel{name = <<"block_user">>}} ->
		IQ#iq{type = result, sub_el = [get_block_user(From,SubEl)]};
	{set, #xmlel{name = <<"block_user">>}} ->
		IQ#iq{type = result, sub_el = [set_block_user(From,SubEl)]};
	{set, #xmlel{name = <<"cancel_block_user">>}} ->
		IQ#iq{type = result, sub_el = [cancel_block_user(From,SubEl)]};
	_ ->
		IQ#iq{type = error,
			sub_el = [SubEl, ?ERR_FEATURE_NOT_IMPLEMENTED]}
	end.	

get_block_user(From,_El) ->
	User = From#jid.user,
	Buser = 
		case catch ets:select(block_user,[{#block_user{user = User,block_user = '$1', _ = '_'},[], ['$1']}]) of
		L when is_list(L) ->
			L;
		_ ->
			[]
		end,
    #xmlel{name = <<"block_user">>,
            attrs = [{<<"xmlns">>,?NS_BLOCK},
                    {<<"users">>,list_to_binary(spell_user(Buser))}],
		            children = []}.

set_block_user(From,El) ->
	User = From#jid.user,
	LServer = jlib:nameprep(From#jid.server),
	Res = 
		case catch xml:get_tag_attr_s(<<"jid">>,El) of
		<<"">> ->
	 		<<"Sucess">>;
	   J ->
	   		case catch odbc_queries:set_block_user(LServer,User,J) of
			{updated,1} ->
				catch ets:insert(block_user,#block_user{user = User, block_user = J}),
				catch ets:delete(recent_contact,User),
				 <<"Sucess">>;
			{error,Reason}  ->
				case  proplists:get_value(code,Reason) of
				<<"23505">> ->
					catch ets:insert(block_user,#block_user{user = User, block_user = J}),
					catch ets:delete(recent_contact,User),
					<<"Sucess">>;
				_ ->
					?DEBUG("Reson is ~p ~n",[Reason]),
					<<"Failed">>
				end
			end
		end,
    #xmlel{name = <<"block_user">>,
            attrs = [{<<"xmlns">>,?NS_BLOCK},
                    {<<"result">>,Res}],
		            children = []}.

cancel_block_user(From,El) ->
	User = From#jid.user,
	LServer = jlib:nameprep(From#jid.server),
	Res = 
		case catch xml:get_tag_attr_s(<<"jid">>,El) of
		<<"">> ->
			<<"Sucess">>;
		J ->
			case catch ejabberd_odbc:sql_query(LServer,
				[<<"delete from user_block_list where username = '">>,User,<<"' and blockuser = '">>,J,<<"';">>]) of
				{updated,1} ->
					catch ets:delete_object(block_user,#block_user{user = User,block_user = J}),
					catch ets:delete(recent_contact,User),
					<<"Sucess">>;
			_ ->
				<<"Failed">>
			end
		end,
    #xmlel{name = <<"cancel_block_user">>,
            attrs = [{<<"xmlns">>,?NS_BLOCK},
                    {<<"result">>,Res}],
		            children = []}.
