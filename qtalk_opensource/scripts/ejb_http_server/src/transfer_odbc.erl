-module(transfer_odbc).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		                terminate/2,code_change/3]).
-export([sync_sql/2,async_sql/2]).

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(HOST, <<"ejb_http_server">>).
-include("logger.hrl").

-record(state, {host,pid}).

start_link(Opts) ->
    gen_server:start_link(?MODULE, [?HOST,Opts], []).

init([Host,_Opts]) ->
	transfer_odbc_sup:add_pid(Host,self()),
    {ok, #state{host = Host,pid = self()}}.

handle_call({sync_sql,Host,Sql}, _From, State) ->
	Ret = do_run_sql(Host,Sql),
    {reply, Ret, State};
handle_call(Msg, _From, State) ->
    {reply, {ok, Msg}, State}.

handle_cast({sql,Host,Sql},State) ->
	do_run_sql(Host,Sql),
	{noreply,State};
handle_cast(stop, State) ->
	transfer_odbc_sup:remove_pid(State#state.host,self()),
    {stop, normal, State}.

handle_info(_From,State) ->
    {noreply,State}.

terminate(_Reason, State) ->
	transfer_odbc_sup:remove_pid(State#state.host,self()),
	{ok,State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%异步sql
async_sql(Host,Sql) ->
	Spid = transfer_odbc_sup:get_random_pid(Host), 
	gen_server:cast(Spid, {sql,Host,Sql}).

%%同步sql
sync_sql(Host,Sql) ->
	Spid = transfer_odbc_sup:get_random_pid(Host), 
	gen_server:call(Spid, {sync_sql,Host,Sql}).

do_run_sql(Host,Sql) ->
	pg_odbc:sql_query(Host,Sql).
