%%%----------------------------------------------------------------------
%%% File    : pg_odbc.erl
%%%----------------------------------------------------------------------

-module(pg_odbc).

-define(GEN_FSM, gen_fsm).

-behaviour(?GEN_FSM).

-include("logger.hrl").

%% External exports
-export([
         start_link/1,
	     sql_query/2,
	     sql_query_t/1,
	     sql_transaction/2,
	     sql_bloc/2,
	     escape/1,
	     escape_like/1,
	     to_bool/1,
         encode_term/1,
         decode_term/1,
	     keep_alive/1]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4,
	 handle_info/3, terminate/3, print_state/1,
	 code_change/4]).

%% gen_fsm states
-export([connecting/2, connecting/3,
	 session_established/2, session_established/3]).


-record(state,
	{db_ref = self()                     :: pid(),
     options = [],
     pending_requests = {0, queue:new()} :: {non_neg_integer(), queue()}}).

-define(STATE_KEY, pg_odbc_state).
-define(NESTING_KEY, pg_odbc_nesting_level).
-define(TOP_LEVEL_TXN, 0).
-define(PGSQL_PORT, 5432).
-define(MYSQL_PORT, 3306).
-define(MAX_TRANSACTION_RESTARTS, 10).
-define(TRANSACTION_TIMEOUT, 60000).
-define(KEEPALIVE_TIMEOUT, 60000).
-define(KEEPALIVE_QUERY, <<"SELECT 1;">>).
-define(START_INTERVAL, 6000).
-define(MAXPENDING_REQUESTS_LEN, 10).
-define(DEFAULT_DB, pgsql).

%%-define(DBGFSM, true).

-ifdef(DBGFSM).

-define(FSMOPTS, [{debug, [trace]}]).

-else.

-define(FSMOPTS, []).

-endif.

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link(Options) ->
    (?GEN_FSM):start_link(pg_odbc,
			  [Options],
			  fsm_limit_opts(Options) ++ (?FSMOPTS)).

-type sql_query() :: [sql_query() | binary()].
-type sql_query_result() :: {updated, non_neg_integer()} |
                            {error, binary()} |
                            {selected, [binary()],
                             [[binary()]]}.

-spec sql_query(binary(), sql_query()) -> sql_query_result().

sql_query(Host, Query) ->
    catch spawn(ejb_monitor, monitor_count, [<<"pgsql_query">>,1]),
    sql_call(Host, {sql_query, Query}).

%% SQL transaction based on a list of queries
%% This function automatically
-spec sql_transaction(binary(), [sql_query()] | fun(() -> any())) ->
                             {atomic, any()} |
                             {aborted, any()}.

sql_transaction(Host, Queries)
    when is_list(Queries) ->
    F = fun () ->
		lists:foreach(fun (Query) -> sql_query_t(Query) end,
			      Queries)
	end,
    sql_transaction(Host, F);
%% SQL transaction, based on a erlang anonymous function (F = fun)
sql_transaction(Host, F) when is_function(F) ->
    sql_call(Host, {sql_transaction, F}).

%% SQL bloc, based on a erlang anonymous function (F = fun)
sql_bloc(Host, F) -> sql_call(Host, {sql_bloc, F}).

sql_call(Host, Msg) ->
    case get(?STATE_KEY) of
      undefined ->
        case pg_odbc_sup:get_random_pid(Host) of
          none -> {error, <<"Unknown Host">>};
         Pid ->
            (?GEN_FSM):sync_send_event(Pid,{sql_cmd, Msg, now()},
                                       ?TRANSACTION_TIMEOUT)
          end;
      _State -> ?DEBUG("need otp ~n",[]),nested_op(Msg)
    end.

keep_alive(PID) ->
    (?GEN_FSM):sync_send_event(PID,
			       {sql_cmd, {sql_query, ?KEEPALIVE_QUERY}, now()},
			       ?KEEPALIVE_TIMEOUT).

-spec sql_query_t(sql_query()) -> sql_query_result().

%% This function is intended to be used from inside an sql_transaction:
sql_query_t(Query) ->
    QRes = sql_query_internal(Query),
    case QRes of
      {error, Reason} -> throw({aborted, Reason});
      Rs when is_list(Rs) ->
	  case lists:keysearch(error, 1, Rs) of
	    {value, {error, Reason}} -> throw({aborted, Reason});
	    _ -> QRes
	  end;
      _ -> QRes
    end.

%% Escape character that will confuse an SQL engine
escape(S) ->
	<<  <<(ejb_odbc_query:escape(Char))/binary>> || <<Char>> <= S >>.

%% Escape character that will confuse an SQL engine
%% Percent and underscore only need to be escaped for pattern matching like
%% statement
escape_like(S) when is_binary(S) ->
    << <<(escape_like(C))/binary>> || <<C>> <= S >>;
escape_like($%) -> <<"\\%">>;
escape_like($_) -> <<"\\_">>;
escape_like(C) when is_integer(C), C >= 0, C =< 255 -> ejb_odbc_query:escape(C).

to_bool(<<"t">>) -> true;
to_bool(<<"true">>) -> true;
to_bool(<<"1">>) -> true;
to_bool(true) -> true;
to_bool(1) -> true;
to_bool(_) -> false.

encode_term(Term) ->
    escape(list_to_binary(
             erl_prettypr:format(erl_syntax:abstract(Term)))).

decode_term(Bin) ->
    Str = binary_to_list(<<Bin/binary, ".">>),
    {ok, Tokens, _} = erl_scan:string(Str),
    {ok, Term} = erl_parse:parse_term(Tokens),
    Term.

%%%----------------------------------------------------------------------
%%% Callback functions from gen_fsm
%%%----------------------------------------------------------------------
init([Options]) ->
    KeepaliveTimeout = proplists:get_value(keepalive_timeout, Options, ?KEEPALIVE_TIMEOUT),
%    timer:apply_interval(KeepaliveTimeout, ?MODULE,keep_alive, [self()]),

    (?GEN_FSM):send_event(self(), connect),

    {ok, connecting,
     #state{
        options = Options,
	    pending_requests = {0, queue:new()}
    }}.

connecting(connect, #state{options = Options} = State) ->
      Host = proplists:get_value(host, Options, <<"localhost">>),
      Server = proplists:get_value(odbc_server, Options, <<"127.0.0.1">>),
      Port = proplists:get_value(odbc_port, Options, 5432),
      DB = proplists:get_value(odbc_database, Options, <<"test">>),
      User = proplists:get_value(odbc_user, Options, <<"test">>),
      Pass = proplists:get_value(odbc_password, Options, <<"test">>),
      Args = [Server, Port, DB, User, Pass],
      ConnectRes = apply(fun pgsql_connect/5, Args),
      {_, PendingRequests} = State#state.pending_requests,
      case ConnectRes of
          {ok, Ref} ->
              erlang:monitor(process, Ref),
              lists:foreach(fun (Req) ->
                                    (?GEN_FSM):send_event(self(), Req)
                            end,
                            queue:to_list(PendingRequests)),
              ?DEBUG("Connecting to pgsql Server [~p] DB [~p] Pid [~p]  sucess~n",[Server,DB,self()]),
              pg_odbc_sup:add_pid(Host, self()),
              {next_state, session_established,
               State#state{db_ref = Ref,
                           pending_requests = {0, queue:new()}}};
          {error, Reason} ->
              StartInterval = proplists:get_value(start_interval, Options, ?START_INTERVAL),
              ?DEBUG("connection failed Reason: ~p~n** ", [Reason]),
              (?GEN_FSM):send_event_after(StartInterval, connect),
              {next_state, connecting, State}
      end;
connecting(Event, State) ->
    ?DEBUG("unexpected event in 'connecting': ~p", [Event]),
    {next_state, connecting, State}.

connecting({sql_cmd, {sql_query, ?KEEPALIVE_QUERY},
	    _Timestamp},
	   From, State) ->
    (?GEN_FSM):reply(From,
		     {error, <<"SQL connection failed">>}),
    {next_state, connecting, State};
connecting({sql_cmd, Command, Timestamp} = Req, From, #state{options = Options} = State) ->
    ?DEBUG("queuing pending request while connecting:~n\t~p", [Req]),
    {Len, PendingRequests} = State#state.pending_requests,
    MaxPendingRequestLen = proplists:get_value(max_queue, Options, ?MAXPENDING_REQUESTS_LEN),
    NewPendingRequests = if Len < MaxPendingRequestLen ->
				{Len + 1,
				 queue:in({sql_cmd, Command, From, Timestamp},
					  PendingRequests)};
			    true ->
				lists:foreach(fun ({sql_cmd, _, To,
						    _Timestamp}) ->
						      (?GEN_FSM):reply(To,
								       {error,
									<<"SQL connection failed">>})
					      end,
					      queue:to_list(PendingRequests)),
				{1,
				 queue:from_list([{sql_cmd, Command, From,
						   Timestamp}])}
			 end,
    {next_state, connecting,
     State#state{pending_requests = NewPendingRequests}};
connecting(Request, {Who, _Ref}, State) ->
    ?DEBUG("unexpected call ~p from ~p in 'connecting'",
		 [Request, Who]),
    {reply, {error, badarg}, connecting, State}.

session_established({sql_cmd, Command, Timestamp}, From,
		    State) ->
    run_sql_cmd(Command, From, State, Timestamp);
session_established(Request, {Who, _Ref}, State) ->
    ?DEBUG("unexpected call ~p from ~p in 'session_establ"
		 "ished'",
		 [Request, Who]),
    {reply, {error, badarg}, session_established, State}.

session_established({sql_cmd, Command, From, Timestamp},
		    State) ->
    run_sql_cmd(Command, From, State, Timestamp);
session_established(Event, State) ->
    ?DEBUG("unexpected event in 'session_established': ~p",
		 [Event]),
    {next_state, session_established, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, {error, badarg}, StateName, State}.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% We receive the down signal when we loose the MySQL connection (we are
%% monitoring the connection)
handle_info({'DOWN', _MonitorRef, process, _Pid, _Info},
	    _StateName, State) ->
    (?GEN_FSM):send_event(self(), connect),
    {next_state, connecting, State};
handle_info(Info, StateName, State) ->
    ?DEBUG("unexpected info in ~p: ~p",
		 [StateName, Info]),
    {next_state, StateName, State}.

terminate(_Reason, _StateName, #state{options = Options} = State) ->
    DbType = proplists:get_value(dbtype, Options, ?DEFAULT_DB), 
    Host = proplists:get_value(host, Options, <<"localhost">>), 
    pg_odbc_sup:remove_pid(Host, self()),
    case DbType of
      mysql -> catch p1_mysql_conn:stop(State#state.db_ref);
      _ -> ok
    end,
    ok.

%%----------------------------------------------------------------------
%% Func: print_state/1
%% Purpose: Prepare the state to be printed on error log
%% Returns: State to print
%%----------------------------------------------------------------------
print_state(State) -> State.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

run_sql_cmd(Command, From, State, Timestamp) ->
    case timer:now_diff(now(), Timestamp) div 1000 of
      Age when Age < (?TRANSACTION_TIMEOUT) ->
	  put(?NESTING_KEY, ?TOP_LEVEL_TXN),
	  put(?STATE_KEY, State),
	  abort_on_driver_error(outer_op(Command), From);
      Age ->
	  ?DEBUG("Database was not available or too slow, "
		     "discarding ~p milliseconds old request~n~p~n",
		     [Age, Command]),
	  {next_state, session_established, State}
    end.

%% Only called by handle_call, only handles top level operations.
%% @spec outer_op(Op) -> {error, Reason} | {aborted, Reason} | {atomic, Result}
outer_op({sql_query, Query}) ->
    sql_query_internal(Query);
outer_op({sql_transaction, F}) ->
    outer_transaction(F, ?MAX_TRANSACTION_RESTARTS, <<"">>);
outer_op({sql_bloc, F}) -> execute_bloc(F).

%% Called via sql_query/transaction/bloc from client code when inside a
%% nested operation
nested_op({sql_query, Query}) ->
    sql_query_internal(Query);
nested_op({sql_transaction, F}) ->
    NestingLevel = get(?NESTING_KEY),
    if NestingLevel =:= (?TOP_LEVEL_TXN) ->
	   outer_transaction(F, ?MAX_TRANSACTION_RESTARTS, <<"">>);
       true -> inner_transaction(F)
    end;
nested_op({sql_bloc, F}) -> execute_bloc(F).

%% Never retry nested transactions - only outer transactions
inner_transaction(F) ->
    PreviousNestingLevel = get(?NESTING_KEY),
    case get(?NESTING_KEY) of
      ?TOP_LEVEL_TXN ->
	  {backtrace, T} = process_info(self(), backtrace),
	  ?DEBUG("inner transaction called at outer txn "
		     "level. Trace: ~s",
		     [T]),
	  erlang:exit(implementation_faulty);
      _N -> ok
    end,
    put(?NESTING_KEY, PreviousNestingLevel + 1),
    Result = (catch F()),
    put(?NESTING_KEY, PreviousNestingLevel),
    case Result of
      {aborted, Reason} -> {aborted, Reason};
      {'EXIT', Reason} -> {'EXIT', Reason};
      {atomic, Res} -> {atomic, Res};
      Res -> {atomic, Res}
    end.

outer_transaction(F, NRestarts, _Reason) ->
    PreviousNestingLevel = get(?NESTING_KEY),
    case get(?NESTING_KEY) of
      ?TOP_LEVEL_TXN -> ok;
      _N ->
	  {backtrace, T} = process_info(self(), backtrace),
	  ?DEBUG("outer transaction called at inner txn "
		     "level. Trace: ~s",
		     [T]),
	  erlang:exit(implementation_faulty)
    end,
    sql_query_internal(<<"begin;">>),
    put(?NESTING_KEY, PreviousNestingLevel + 1),
    Result = (catch F()),
    put(?NESTING_KEY, PreviousNestingLevel),
    case Result of
      {aborted, Reason} when NRestarts > 0 ->
	  sql_query_internal(<<"rollback;">>),
	  outer_transaction(F, NRestarts - 1, Reason);
      {aborted, Reason} when NRestarts =:= 0 ->
	  ?DEBUG("SQL transaction restarts exceeded~n** "
		     "Restarts: ~p~n** Last abort reason: "
		     "~p~n** Stacktrace: ~p~n** When State "
		     "== ~p",
		     [?MAX_TRANSACTION_RESTARTS, Reason,
		      erlang:get_stacktrace(), get(?STATE_KEY)]),
	  sql_query_internal(<<"rollback;">>),
	  {aborted, Reason};
      {'EXIT', Reason} ->
	  sql_query_internal(<<"rollback;">>), {aborted, Reason};
      Res -> sql_query_internal(<<"commit;">>), {atomic, Res}
    end.

execute_bloc(F) ->
    case catch F() of
      {aborted, Reason} -> {aborted, Reason};
      {'EXIT', Reason} -> {aborted, Reason};
      Res -> {atomic, Res}
    end.

sql_query_internal(Query) ->
    State = #state{options = Options} = get(?STATE_KEY),
    DbType = proplists:get_value(dbtype, Options, pgsql),
    Res = case DbType of
              pgsql ->
                  pgsql_to_odbc(pgsql:squery(State#state.db_ref, Query))
          end,
    case Res of
        {error, <<"No SQL-driver information available.">>} ->
            {updated, 0};
        _Else -> Res
    end.

%% Generate the OTP callback return tuple depending on the driver result.
abort_on_driver_error({error, <<"query timed out">>} =
			  Reply,
		      From) ->
    (?GEN_FSM):reply(From, Reply),
    {stop, timeout, get(?STATE_KEY)};
abort_on_driver_error({error,
		       <<"Failed sending data on socket", _/binary>>} =
			  Reply,
		      From) ->
    (?GEN_FSM):reply(From, Reply),
    {stop, closed, get(?STATE_KEY)};
abort_on_driver_error(Reply, From) ->
    (?GEN_FSM):reply(From, Reply),
    {next_state, session_established, get(?STATE_KEY)}.

%% == pure ODBC code

%% part of init/1
%% Open an ODBC database connection
%%%%%%-----------------------------------------------------%%%%%
%%odbc_connect(SQLServer) ->
  %%  ejabberd:start_app(odbc),
   %% odbc:connect(binary_to_list(SQLServer), [{scrollable_cursors, off}]).
%%%%%%-----------------------------------------------------%%%%%%

%% == Native PostgreSQL code

%% part of init/1
%% Open a database connection to PostgreSQL
pgsql_connect(Server, Port, DB, Username, Password) ->
    case pgsql:connect([{host, Server},
                        {database, DB},
                        {user, Username},
                        {password, Password},
                        {port, Port},
                        {as_binary, true}]) of
        {ok, Ref} ->
            pgsql:squery(Ref, [<<"alter database ">>, DB, <<" set ">>,
                               <<"standard_conforming_strings='off';">>]),
            {ok, Ref};
        Err ->
            Err
    end.

%% Convert PostgreSQL query result to Erlang ODBC result formalism
pgsql_to_odbc({ok, PGSQLResult}) ->
    case PGSQLResult of
      [Item] -> pgsql_item_to_odbc(Item);
      Items -> [pgsql_item_to_odbc(Item) || Item <- Items]
    end.

pgsql_item_to_odbc({<<"SELECT", _/binary>>, Rows,
		    Recs}) ->
    {selected, [element(1, Row) || Row <- Rows], Recs};
pgsql_item_to_odbc({<<"FETCH", _/binary>>, Rows,
		    Recs}) ->
    {selected, [element(1, Row) || Row <- Rows], Recs};
pgsql_item_to_odbc(<<"INSERT ", OIDN/binary>>) ->
    [_OID, N] = str:tokens(OIDN, <<" ">>),
    {updated, list_to_integer(binary_to_list(N))};
pgsql_item_to_odbc(<<"DELETE ", N/binary>>) ->
    {updated, list_to_integer(binary_to_list(N))};
pgsql_item_to_odbc(<<"UPDATE ", N/binary>>) ->
    {updated, list_to_integer(binary_to_list(N))};
pgsql_item_to_odbc({error, Error}) -> {error, Error};
pgsql_item_to_odbc(_) -> {updated, undefined}.

fsm_limit_opts(Options) ->
    case proplists:get_value(max_queue, Options, undefined) of
        undefined -> [];
        N when is_integer(N) -> [{max_queue, N},{spawn_opt, [{fullsweep_after, 0}]}];
        _ -> [{spawn_opt, [{fullsweep_after, 0}]}]
    end.
