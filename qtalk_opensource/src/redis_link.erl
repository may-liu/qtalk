-module(redis_link).

-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		                terminate/2,code_change/3]).
-export([str_set/4,str_get/3,hash_set/5,hash_get/4,hash_del/4,expire_time/4,get_all_keys/2,str_setex/5,str_del/3,ttl_key/3]).
-export([redis_cmd/3]).

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-include("ejabberd.hrl").
-include("logger.hrl").

-record(state, {host,tab,redis_pid}).

start_link(Host,Tab,Opts) ->
    gen_server:start_link(?MODULE, [Host,Tab,Opts], []).

init([Host,Tab,Opts]) ->
	Redis_Port = gen_mod:get_opt(redis_port, Opts, fun(A) -> A end, 26379),
	Redis_Pass = gen_mod:get_opt(redis_password, Opts, fun(A) -> A end, <<"redis_password">>),
	StartMode = gen_mod:get_opt(redis_start_mode, Opts, fun(A) -> A end, 1),
	Rpid = 
		case StartMode  of 
		1 ->
			Redis_Master = gen_mod:get_opt(redis_master, Opts, fun(A) -> A end, <<"redis_server">>),
			case eredis:start_link(binary_to_list(Redis_Master),Redis_Port,Tab,binary_to_list(Redis_Pass),1000) of
			{ok,Pid} ->
				mod_redis:add_pid(Host,Tab,self()),
				Pid;
			_ ->
				[]
			end;
		_ ->
			Redis_Host = gen_mod:get_opt(redis_server, Opts, fun(A) -> A end, <<"redis_server">>),
			case eredis:start_link(binary_to_list(Redis_Host),Redis_Port,Tab,binary_to_list(Redis_Pass),1000) of	
			{ok,Pid} ->
				mod_redis:add_pid(Host,Tab,self()),
				Pid;
			_ ->
				[]
			end
		end,
    {ok, #state{host = Host,tab = Tab ,redis_pid = Rpid}}.

handle_call({strget,Key},_From,State) ->
    Ret =  eredis:q(State#state.redis_pid, ["GET",Key],1000),
	{reply,Ret,State};
handle_call({strset,Key,Val},_From,State) ->
    Ret =  eredis:q(State#state.redis_pid, ["SET", Key, Val],1000),
    {reply,Ret,State};
handle_call({ttlkey,Key},_From,State) ->
    Ret =  eredis:q(State#state.redis_pid, ["TTL", Key],1000),
    {reply,Ret,State};
handle_call({strsetex,Key,Time,Val},_From,State) ->
    Ret =  eredis:q(State#state.redis_pid, ["SETEX",Time, Key, Val],1000),
    {reply,Ret,State};
handle_call({expiretime,Key,Time},_From,State) ->
    Ret =  eredis:q(State#state.redis_pid, ["EXPIRE", Key, Time],1000),
    {reply,Ret,State};
handle_call({hashget,Key,Field},_From,State) ->
    Ret =  eredis:q(State#state.redis_pid, ["HGET",Key,Field],1000),
	{reply,Ret,State};
handle_call({hashset,Key,Field,Val},_From,State) ->
    Ret =  eredis:q(State#state.redis_pid, ["HSET", Key, Field,Val],1000),
    {reply,Ret,State};
handle_call(getkeys,_From,State) ->
    Ret =  eredis:q(State#state.redis_pid, ["KEYS", <<"*">>],5000),
    {reply,Ret,State};
handle_call({redis_cmd,Redis_cmd},_From,State) ->
    Ret =  eredis:q(State#state.redis_pid, Redis_cmd,5000),
    {reply,Ret,State};
handle_call(Msg, _From, State) ->
    {reply, {ok, Msg}, State}.

handle_cast({hashdel,Key,Field},State) ->
    case eredis:q(State#state.redis_pid, ["HDEL", Key, Field]) of
	{error,Reason} ->
    	?DEBUG("Run redis error ~p ~n",[Reason]);
	_ ->
		ok
	end,
	{noreply,State};
handle_cast({strdel,Key},State) ->
    case eredis:q(State#state.redis_pid, ["DEL", Key]) of
	{error,Reason} ->
    	?DEBUG("Run redis error ~p ~n",[Reason]);
	_ ->
		ok
	end,
	{noreply,State};
handle_cast(stop, State) ->
	mod_redis:remove_pid(State#state.host,State#state.tab,self()),
    {stop, normal, State}.

handle_info(_From,State) ->
    {noreply,State}.

terminate(_Reason, State) ->
	{ok,State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
	
expire_time(Host,Tab,Key,Time) ->
	Rpid = mod_redis:get_random_pid(Host,Tab),
	do_call(Rpid, {expiretime,Key,Time}).

str_set(Host,Tab,Key,Str) ->
   Rpid = mod_redis:get_random_pid(Host,Tab),
   do_call(Rpid, {strset,Key,Str}).

str_setex(Host,Tab,Time,Key,Str) ->
   Rpid = mod_redis:get_random_pid(Host,Tab),
   do_call(Rpid, {strsetex,Key,Time,Str}).

str_get(Host,Tab,Key) ->
   Rpid = mod_redis:get_random_pid(Host,Tab),
   do_call(Rpid,{strget,Key}).

hash_set(Host,Tab,Key,Field,Val) ->
	Rpid = mod_redis:get_random_pid(Host,Tab),
	do_call(Rpid, {hashset,Key,Field,Val}).

hash_get(Host,Tab,Key,Field) ->
   Rpid = mod_redis:get_random_pid(Host,Tab),
   do_call(Rpid,{hashget,Key,Field}).

hash_del(Host,Tab,Key,Field) ->
	Rpid = mod_redis:get_random_pid(Host,Tab),
	do_cast(Rpid, {hashdel,Key,Field}).

str_del(Host,Tab,Key) ->
	Rpid = mod_redis:get_random_pid(Host,Tab),
	do_cast(Rpid, {strdel,Key}).

get_all_keys(Host,Tab) ->
	Rpid = mod_redis:get_random_pid(Host,Tab),
	do_call(Rpid,getkeys).

ttl_key(Host,Tab,Key) ->
	Rpid = mod_redis:get_random_pid(Host,Tab),
	do_call(Rpid, {ttlkey,Key}).

redis_cmd(Host,Tab,Cmd) ->
	Rpid = mod_redis:get_random_pid(Host,Tab),
	do_call(Rpid, {redis_cmd,Cmd}).

do_call(Pid, Message) ->
	try gen_server:call(Pid, Message) of
	{error, Error} ->
		{error, Error};
	{ok, Reply} ->
		{ok, Reply}
	catch
		_Type:Error ->
			{error, Error}
	end.

do_cast(Pid,Message) ->
	try gen_server:cast(Pid, Message) of
	{error, Error} ->
		{error, Error};
	ok ->
		ok
	catch
		_Type:Error ->
			{error, Error}
	end.
