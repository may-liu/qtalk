-module(redis_link).

-include("logger.hrl").

-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		                terminate/2,code_change/3]).
-export([str_set/3,str_get/2,hash_set/4,hash_get/3,hash_del/3,expire_time/3]).
-export([redis_cmd/2]).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-record(state, {tab,redis_pid}).

start_link(PoolName, Table, Opts) ->
    gen_server:start_link(?MODULE, [PoolName, Table, Opts], []).

init([PoolName, Table, Opts]) ->
    process_flag(trap_exit, true),
    StartMode = proplists:get_value(start_mode, Opts, 0),
	Redis_Host = 
		case StartMode of
		1 ->
			proplists:get_value(master, Opts, "sentinel:mymaster");
		_ ->
			proplists:get_value(server, Opts, "redis_server")
		end,
	Redis_Port = 
		case StartMode of
		1 ->
			proplists:get_value(port, Opts, 26379);
		_ ->
			proplists:get_value(port, Opts, 6379)
		end,
	Redis_Pass = proplists:get_value(password, Opts, "redis_password"),
	ReconnectSleep = proplists:get_value(reconnect_sleep, Opts, 60000),
	ConnectTimeout = proplists:get_value(connect_timeout, Opts, 1000),
	Pid = start_redis(PoolName, Redis_Host, Redis_Port, Table, Redis_Pass, ReconnectSleep, ConnectTimeout),
    ?DEBUG("Redis init redis_pid ~p ~n",[self()]),
    {ok, #state{tab = Table, redis_pid = Pid}}.

handle_call({strget,Key}, _From,State) ->
    Ret =  eredis:q(State#state.redis_pid, ["GET",Key]),
	{reply,Ret,State};
handle_call({strset,Key,Val}, _From,State) ->
    Ret =  eredis:q(State#state.redis_pid, ["SET", Key, Val]),
    {reply,Ret,State};
handle_call({expiretime,Key,Time}, _From,State) ->
    Ret =  eredis:q(State#state.redis_pid, ["EXPIRE", Key, Time]),
    {reply,Ret,State};
handle_call({hashget,Key,Field}, _From,State) ->
    Ret =  eredis:q(State#state.redis_pid, ["HGET",Key,Field]),
	{reply,Ret,State};
handle_call({hashset,Key,Field,Val}, _From,State) ->
    Ret =  eredis:q(State#state.redis_pid, ["HSET", Key, Field,Val]),
    {reply,Ret,State};
handle_call({redis_cmd,Redis_cmd},_From,State) ->
    Ret =  eredis:q(State#state.redis_pid, Redis_cmd,1000),
    {reply,Ret,State};
handle_call(Msg, _From, State) ->
    {reply, {ok, Msg}, State}.

handle_cast({hashdel,Key,Field},State) ->
    case eredis:q(State#state.redis_pid, ["HDEL", Key, Field]) of
	{error,_Reason} ->
        ok;
	_ ->
		ok
	end,
	{noreply,State};
handle_cast(stop, State) ->
	redis_link_sup:remove_pid(State#state.tab,self()),
    {stop, normal, State}.

handle_info(_Info,State) ->
    {noreply,State}.

terminate(_Reason, State) ->
	redis_link_sup:remove_pid(State#state.tab,self()),
	{ok,State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

expire_time(Tab,Key,Time) ->
	Rpid = redis_link_sup:get_random_pid(Tab),
	do_call(Rpid, {expiretime,Key,Time}).

str_set(Tab,Key,Str) ->
   Rpid = redis_link_sup:get_random_pid(Tab),
   do_call(Rpid, {strset,Key,Str}).

str_get(Tab,Key) ->
   Rpid = redis_link_sup:get_random_pid(Tab),
   do_call(Rpid,{strget,Key}).

hash_set(Tab,Key,Field,Val) ->
	Rpid = redis_link_sup:get_random_pid(Tab),
	do_call(Rpid, {hashset,Key,Field,Val}).

hash_get(Tab,Key,Field) ->
   Rpid = redis_link_sup:get_random_pid(Tab),
   do_call(Rpid,{hashget,Key,Field}).

hash_del(Tab,Key,Field) ->
	Rpid = redis_link_sup:get_random_pid(Tab),
	gen_server:cast(Rpid, {hashdel,Key,Field}).

redis_cmd(Tab,Cmd) ->
	Rpid = redis_link_sup:get_random_pid(Tab),
	do_call(Rpid, {redis_cmd,Cmd}).

start_redis(PoolName, Redis_Host, Redis_Port, Tab, Redis_Pass, ReconnectSleep, ConnectTimeout) ->
    case catch eredis:start_link(Redis_Host, Redis_Port, Tab, Redis_Pass, ConnectTimeout) of
        {ok,Pid} ->
            redis_link_sup:add_pid(Tab,self()),
            Pid;
        Error ->
            ?DEBUG("the redis connect error for ~p, attemp to reconnect......~n", [Error]),
            timer:sleep(ReconnectSleep),
            start_redis(PoolName, Redis_Host, Redis_Port, Tab, Redis_Pass,ReconnectSleep, ConnectTimeout)
    end.

do_call(none, Message) ->
    {error, lists:flatten(io_lib:format("can not find redis link process for <~p>", [Message]))};
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
