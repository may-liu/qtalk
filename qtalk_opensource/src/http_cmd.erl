%zr% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(http_cmd).

%% API.
-export([start/2]).
-export([stop/1]).
%% API.
-include("logger.hrl").

start(_Type, Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/setuserkey",http_setmackey,[]},
			{"/sendmessage",http_sendmessage,[]},
			{"/send_warn_msg",http_send_warn_msg,[]},
			{"/sendnotice",http_sendall,[]},
			{"/senddeprt",http_senddep,[]},
			{"/management_cmd",http_management,[]},
			{"/qmonitor.jsp",http_qmonitor,[]},
			{"/send_rbt_msg",http_send_rbt_msg,[]},
			{"/wlan_send_msg",http_wlan_send_msg,[]},
			{"/get_user_status",http_get_user_status,[]},
			{"/send_muc_presence",http_muc_vcard_presence,[]},
			{"/answer_http",http_answer_http,[]},
			{"/create_muc",http_create_muc,[]},
			{"/add_muc_user",http_add_muc_user,[]},
			{"/del_muc_user",http_del_muc_user,[]},
			{"/destroy_muc",http_destroy_muc,[]},
			{"/get_muc_info",http_get_muc_info,[]},
            {"/registeruser", http_registeruser, []}
		]}
	]),
	cowboy:stop_listener(http),
	Http_port = gen_mod:get_opt(http_port, Args, fun(A) -> A end, 10050),	
	{ok,_ } = cowboy:start_http(http, 200, [{port,Http_port}], [
		{env, [{dispatch, Dispatch},{max_connections, infinity}]}
	]).

stop(_State) ->
	ok.
