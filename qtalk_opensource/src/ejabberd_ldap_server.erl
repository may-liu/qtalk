-module(ejabberd_ldap_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

-export([login/3,
		 get_dep/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {dep = [], handle, user, passwd, hosts, port}).

-define(TIMEOUT, 600000).
-define(UPDATEINTERVAL, 60000).
%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link(Opt) ->
    gen_server:start_link(?MODULE, [Opt], []).

login(Server, User, Passwd) ->
	gen_server:call(Server, {login, User, Passwd}).

get_dep(Server) ->
	gen_server:call(Server, get_dep).
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Arg]) ->
	User = proplists:get_value("user", Arg, "user"),
	Passwd = proplists:get_value("passwd", Arg, "passwd"),
	Hosts = proplists:get_value("host", Arg, [""]),
	Port = proplists:get_value("port", Arg, 389),
	erlang:send_after(?UPDATEINTERVAL, self(), update_dep),
	State0 = #state{user = User, passwd = Passwd, hosts = Hosts, port = Port}, 
	State =
	case update_dep(State0) of
		{error, _} -> State0;
		State1 -> State1
	end,
	ejabberd_ldap_sup:add_pid(self()),
	{ok, State}.

handle_call(get_dep, _Request, #state{dep = Dep} = State) ->
	{reply, Dep, State};
handle_call({login, User, Passwd}, _Request, State) ->
	case simple_bind(User, Passwd, State) of
		{ok, NewState} ->
			{reply, ok, NewState};
		{error, NewState} ->
			{reply, error, NewState}
	end;
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(update_dep, State) ->
	io:format("update for dep~n", []),
	erlang:send_after(?UPDATEINTERVAL, self(), update_dep),
	{noreply, update_dep(State)};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
get_handle(Hosts, Port) ->
	case eldap:open(Hosts, [{port, Port}, {timeout, ?TIMEOUT}]) of
		{ok, Handle} -> Handle;
		Other -> io:format("get handle error for ~p~n", [Other]), Other
	end.

%% TODO 这个地方是需要根据北工大的逻辑进行修改的
%%------------------------------------------------------------------
%%ou=qunarstaff(root) dc=qunarservers(domain,xxx@qunarservers)
%%-------------------------------------------------------------------
do_get_dep(Handle) ->
	{ok, {_, L, []}} = eldap:search(Handle, [{base, "ou=qunarstaff,dc=qunarservers,dc=com"}, {filter, eldap:present("sAMAccountName")}, {attributes, ["sAMAccountType", "sAMAccountName", "distinguishedName"]}]),
	Dep = lists:map(fun({_, _, T}) ->
		[AccountName] = proplists:get_value("sAMAccountName", T),
		[AccountType] = proplists:get_value("sAMAccountType", T),
		[DistinguishedName] = proplists:get_value("distinguishedName", T),
		{AccountName, AccountType, DistinguishedName}
	end, L),
	Dep.
	

update_dep(#state{handle = undefined, hosts = Hosts, port = Port} = State) ->
	case get_handle(Hosts, Port) of
		{error, _} = Error -> Error;
		Handle -> update_dep(State#state{handle = Handle})
	end;
update_dep(#state{handle = Handle, user = User, passwd = Passwd} = State) ->
	case eldap:simple_bind(Handle, User, Passwd) of
		ok ->
			Dep = do_get_dep(Handle),
			State#state{dep = Dep};
		%%the connection may be timeout
		{error,{gen_tcp_error,closed}} -> update_dep(State#state{handle=undefined});
		{error, Error} -> io:format("simple_bind error for ~p~n", [Error]), {error, Error}
	end.

simple_bind(User, Passwd, #state{handle = undefined, hosts = Hosts, port = Port} = State) ->
	case get_handle(Hosts, Port) of
		{error, _} -> {error, State};
		Handle -> simple_bind(User, Passwd, State#state{handle = Handle})
	end;
simple_bind(User, Passwd, #state{handle = Handle} = State) ->
	case eldap:simple_bind(Handle, User, Passwd) of
		ok -> {ok, State};
		%%the connection may be timeout
		{error, {gen_tcp_error,closed}} -> simple_bind(User, Passwd, State#state{handle = undefined});
		{error, Error} -> io:format("simple_bind error for ~p~n", [Error]), {error, State}
	end.
