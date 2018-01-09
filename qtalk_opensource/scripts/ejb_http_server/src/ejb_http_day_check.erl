-module(ejb_http_day_check).

-behaviour(gen_server).

-export([start_link/1,init/1]).

-export([handle_call/3, handle_cast/2,
 	    handle_info/2, terminate/2, code_change/3]).

-include("logger.hrl").
-include("ejb_http_server.hrl").

-define(SERVER, ?MODULE).

-record(state,{server,day_timer}).

start_link(Opts) ->
	gen_server:start_link(?MODULE, [Opts], []).

init([_Opts]) ->
	Server = 
		case ets:lookup(ejabberd_config,<<"http_server">>) of
		[Http_server] when is_record(Http_server,ejabberd_config) ->
			Http_server#ejabberd_config.val;
		_ ->
			 "http://127.0.0.1/"
		end,
	Timestamp = mod_time:get_timestamp(),
	Tref_day = 
		case mod_time:get_timestamp_of_end_day(Timestamp) of
		0 ->
    	    erlang:start_timer(10000,self(),first_day_sum);
		End_timestamp ->
			Count_time = End_timestamp - Timestamp,
    	    erlang:start_timer(Count_time*1000,self(),first_day_sum)
		end,
	{ok, #state{server = Server,day_timer = Tref_day}}.


handle_call(stop, _From, State=#state{day_timer = Day_tref}) ->
    {ok, cancel} = timer:cancel(Day_tref),
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, TimerRef, first_day_sum},State) ->
	case erlang:read_timer(TimerRef) of
	false ->
		New_day_tref = erlang:start_timer(86400*1000,self(),day_sum),
		NewState = State#state{day_timer = New_day_tref},
		{noreply, NewState};
	_ ->
		{noreply, State}
	end;
handle_info({timeout, TimerRef, day_sum},State=#state{server = Server}) ->
	case  erlang:read_timer(TimerRef) of
	false ->
	    ejb_update_cache:update_online(Server,true),
	    ejb_update_cache:get_monitor_info(Server,true),
	    ejb_update_cache:update_department_info(true),
	    ejb_update_cache:update_online_status_cache(true),
	    ejb_update_cache:update_vcard_version(true),
	    ejb_update_cache:update_muc_vcard_version(true),
	    ejb_update_cache:update_robot_info(true),
	    ejb_update_cache:update_user_rbt(true),
	    ejb_update_cache:update_version_users(true),
		New_day_tref = erlang:start_timer(86400*1000,self(),day_sum),
		NewState = State#state{day_timer = New_day_tref},
		{noreply,NewState};
	_ ->
	    ejb_update_cache:update_online(Server,true),
	    ejb_update_cache:get_monitor_info(Server,true),
	    ejb_update_cache:update_department_info(true),
	    ejb_update_cache:update_online_status_cache(true),
	    ejb_update_cache:update_vcard_version(true),
	    ejb_update_cache:update_muc_vcard_version(true),
	    ejb_update_cache:update_robot_info(true),
	    ejb_update_cache:update_user_rbt(true),
	    ejb_update_cache:update_version_users(true),
		{noreply,State}
	end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


