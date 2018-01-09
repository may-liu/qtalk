-module(ejb_cache).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("logger.hrl").
-include("ejb_http_server.hrl").
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([get_host/0,update_user_rbt/3,update_user_vcard/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-record(state,{http_server,one_minute_timer,ten_minute_timer,one_hour_timer,three_hour_timer}).

start_link(Opts) ->
    gen_server:start_link(?MODULE, [Opts], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Args]) ->
	register(ejb_cache,self()),
	Ejabberd_Server = proplists:get_value(ejabberd_server,Args),
	Ejabberd_Opts = ejb_http_server_env:get_env(ejb_http_server,ejabberd_config,	[host,"ejabhost1"]),
	Ejabberd_Node = ejb_http_server_env:get_env(ejb_http_server,ejabberd_node, 		[node,"ejabberd@node"]),
	Http_other_Node = ejb_http_server_env:get_env(ejb_http_server,http_other_node,  [other_node,""]),
	Host = proplists:get_value(host,Ejabberd_Opts),
	Node = proplists:get_value(node,Ejabberd_Node),
	Other_node  = proplists:get_value(other_node,Http_other_Node),
	catch ets:insert(ejabberd_config,#ejabberd_config{key = <<"http_server">>,val = Ejabberd_Server}),
	catch ets:insert(ejabberd_config,#ejabberd_config{key = <<"host">>,val = list_to_binary(Host)}),
	catch ets:insert(ejabberd_config,#ejabberd_config{key = <<"node">>,val = list_to_atom(Node)}),
	catch ets:insert(ejabberd_config,#ejabberd_config{key = <<"http_other_node">>,val = list_to_atom(Other_node)}),
    {ok, #state{http_server = Ejabberd_Server},1000}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({update_user_vcard,Vcard},State) ->
	?DEBUG("Vcard ~p ~n",[Vcard]),
    spawn(?MODULE, update_user_vcard,[Vcard]),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout,#state{http_server = Http_Server } = State) ->
	?INFO_MSG("First init memory ~p ~n",[self()]),
	ejb_update_cache:update_online(Http_Server,false),	
	ejb_update_cache:get_monitor_info(Http_Server,false),	
	ejb_update_cache:update_department_info(false),
	ejb_update_cache:update_online_status_cache(false),
	ejb_update_cache:update_vcard_version(false),
	ejb_update_cache:update_muc_vcard_version(false),
	ejb_update_cache:update_robot_info(false),
	ejb_update_cache:update_user_rbt(false),
	ejb_update_cache:update_version_users(false),
	ejb_update_cache:update_domain_to_url(false),
	ejb_update_cache:update_user_profile(false),
	iplimit_util:update_iplimit(),
    iplimit_util:update_ip_limit(),
    ejb_update_cache:update_virtual_users(false),
	One_min_timer = erlang:start_timer(60*1000,self(),one_min),
	Ten_min_timer = erlang:start_timer(600*1000,self(),ten_min),
	One_hour_timer = erlang:start_timer(3600*1000,self(),one_hour),
	Three_hour_timer = erlang:start_timer(3600*3*1000,self(),three_hour),
	{noreply, State#state{one_minute_timer = One_min_timer,ten_minute_timer = Ten_min_timer,
                one_hour_timer =  One_hour_timer,three_hour_timer = Three_hour_timer}};
handle_info({timeout, TimerRef, one_min},State=#state{http_server = Server}) ->
	ejb_update_cache:update_online(Server,false),	
	ejb_update_cache:get_monitor_info(Server,false),	
	ejb_update_cache:update_online_status_cache(false),
    case  erlang:read_timer(TimerRef) of
    false ->
        New_tref = erlang:start_timer(60*1000,self(),one_min),
        NewState = State#state{one_minute_timer = New_tref},
        {noreply,NewState};
    _ ->
		{noreply,State}
	end;
handle_info({timeout, TimerRef, ten_min},State) ->
	ejb_update_cache:update_vcard_version(false),
	ejb_update_cache:update_user_profile(false),
	ejb_update_cache:update_muc_vcard_version(false),
	ejb_update_cache:update_robot_info(false),
	ejb_update_cache:update_user_rbt(false),
    ejb_update_cache:update_virtual_users(true),
	iplimit_util:update_iplimit(),
	iplimit_util:update_ip_limit(),
    case  erlang:read_timer(TimerRef) of
    false ->
        New_tref = erlang:start_timer(600*1000,self(),ten_min),
        NewState = State#state{ten_minute_timer = New_tref},
        {noreply,NewState};
    _ ->
		{noreply,State}
	end;
handle_info({timeout, TimerRef, one_hour},State) ->
		memory_free:memory_free(200),
        case  erlang:read_timer(TimerRef) of
        false ->
            New_tref = erlang:start_timer(3600*1000,self(),one_hour),
            NewState = State#state{one_hour_timer = New_tref},
            {noreply,NewState};
        _ ->
            {noreply,State}
        end;
handle_info({timeout, TimerRef, three_hour},State) ->
	ejb_update_cache:update_department_info(false),
	ejb_update_cache:update_version_users(false),
    case  erlang:read_timer(TimerRef) of
    false ->
        New_tref = erlang:start_timer(3600*3*1000,self(),three_hour),
        NewState = State#state{three_hour_timer = New_tref},
        {noreply,NewState};
    _ ->
		{noreply,State}
	end;
handle_info({update_muc_vcard,Muc},State)  ->
	catch ets:insert(muc_vcard,Muc),
	{noreply, State};	
handle_info({update_vcard,Vcard},State)  ->
		catch ets:insert(vcard_version,Vcard),
	{noreply, State};	
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

get_host() ->
	case catch  ets:lookup(ejabberd_config,<<"host">>) of
	[Http_server] when is_record(Http_server,ejabberd_config) ->
		Http_server#ejabberd_config.val;
	_ ->
		<<"ejabhost1">>
	end.

update_user_rbt(User,Rbt,Method) ->
	?DEBUG("update_user_rbt user ~p,rbt ~p ,Method ~p  ~n",[User,Rbt,Method]),
	case Method of
	<<"add">> ->
		catch ets:insert(user_rbts,#user_rbts{user = User,rbt = Rbt});
	<<"del">> ->
		catch ets:delete_object(user_rbts,#user_rbts{user = User,rbt = Rbt});
	_  ->
		?DEBUG("update user rbt unknown info ~p ~n",[Method])
	end.
	

check_memeoy() ->
	case  erlang:memory('processes') >  400000000 of 
	true ->
		true;
	_ ->
		R = random:uniform() * 10,
		R1  = trunc(R),
		case catch  R1 rem 2 =:= 0 of 
		true ->
			true;
		_ ->
			false
		end
	end.

update_user_vcard(Vcard) ->
    ets:insert(vcard_version,Vcard).
