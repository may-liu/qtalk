-module(http_cowboy).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		                terminate/2,code_change/3,stop/0]).
-behaviour(gen_server).

-include("ejabberd.hrl").
-include("logger.hrl").

-record(state, {}).

start_link(Opts) ->
    gen_server:start_link(?MODULE, [Opts], [?MODULE]).

init([Opts]) ->
	?DEBUG("Init Cowboy Start ~n",[]),
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/get_users", http_get_users, []},
			{"/get_deps", http_get_deps, []},
			{"/get_msg_info",http_get_msgs,[]},
			{"/get_online_user",http_get_online_user,[]},
			{"/get_online_user2",http_get_online_user2,[]},
			{"/get_user_suoxie",http_get_suoxie,[]},
			{"/get_muc_msg",http_get_muc_msg,[]},
			{"/get_server_time",http_get_server_time,[]},
			{"/set_user_key",http_set_mac_key,[]},
			{"/get_vcard_info",http_get_vcard_info,[]},
			{"/set_vcard_info",http_set_vcard_info,[]},
			{"/set_muc_vcard",http_set_muc_vcard,[]},
			{"/get_muc_vcard",http_get_muc_vcard,[]},
			{"/send_message",http_send_message,[]},
			{"/send_notice",http_send_notice,[]},
			{"/qmonitor.jsp",http_qmonitor,[]},
			{"/healthcheck.html",http_healthcheck,[]},
			{"/get_user_status", http_get_user_status, []},
            {"/set_blacklist", http_set_blacklist, []},
            {"/set_whitelist", http_set_whitelist, []},
			{"/get_history",http_get_history,[]},
			{"/management_cmd",http_management,[]},
			{"/user_robot",http_subscription,[]},
			{"/get_robot",http_get_rbt_info,[]},
			{"/register_robot",http_rbtReg,[]},
			{"/send_rbt_msg",http_send_rbt_msg,[]},
            {"/register_user", http_register_user, []}
		]}
	]),
	cowboy:stop_listener(http),
	Http_port = gen_mod:get_opt(http_port, Opts, fun(A) -> A end, 10056),	
	{ok,_ } = cowboy:start_http(http, 600, [{port,Http_port}], [
		{env, [{dispatch, Dispatch},{max_connections, infinity}]}
	]).

stop() ->
	cowboy:stop_listener(http).

handle_call(Msg, _From, State) ->
    {reply, {ok, Msg}, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(_,State ) ->
	{noreply,State}.

terminate(_Reason, State) ->
	{ok,State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
