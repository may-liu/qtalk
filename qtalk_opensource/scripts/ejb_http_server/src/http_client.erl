-module(http_client).
-include("logger.hrl").
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		                terminate/2,code_change/3]).
-export([http_post/6,
         http_get/4,
         get/1,
         post/2]).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

-record(state, {pid,profile}).

start_link(Opts) ->
    gen_server:start_link(?MODULE, [Opts], []).

init([Opts]) ->
	http_client_sup:add_pid(self()),
	Seq =  proplists:get_value(<<"seq">>,Opts),
	Profile = list_to_atom("http_client_" ++ integer_to_list(Seq)),
	{ok, Pid} = inets:start(httpc, [{profile, Profile}]),
    {ok, #state{pid = Pid,profile = Profile}}.

handle_call({http_post,Url, Header, Type, Body, HTTPOptions, Options}, _From, State) ->
	Method = post,
	Profile = State#state.profile,
	Return = httpc:request(Method, {Url, Header, Type, Body}, HTTPOptions, Options, Profile),
    {reply, Return, State};
handle_call({http_get,Url, Header, HTTPOptions, Options}, _From, State) ->
	Method = get,
	Profile = State#state.profile,
	Return = httpc:request(Method, {Url, Header}, HTTPOptions, Options, Profile),
    {reply, Return, State};
handle_call(Msg, _From, State) ->
    {reply, {ok, Msg}, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(Info,State) ->
    {noreply,State}.

terminate(_Reason, State) ->
	inets:stop(httpc, State#state.pid),
	http_client_sup:remove_pid(self()),
	{ok,State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

http_post(Url, Header, Type, Body, HTTPOptions, Options) ->
	Http_client_pid  = http_client_sup:get_random_pid(),
	do_call(Http_client_pid, {http_post,Url, Header, Type, Body, HTTPOptions, Options}).

http_get(Url, Header, HTTPOptions, Options) ->
	Http_client_pid  = http_client_sup:get_random_pid(),
	do_call(Http_client_pid, {http_get, Url, Header, HTTPOptions, Options}).

do_call(Pid, Message) ->
    try gen_server:call(Pid, Message)
    catch
        Type:Error ->
            {error, Error}
    end.

get(Url) ->
    case http_get(Url, [{"connection", "close"}], [], []) of
        {ok, {_Status, Body}} ->
            {ok, "", Body};
        {ok, {_StatusLine, Headers, Body}} ->
            {ok, Headers, Body};
        {error,Reason} ->                                                                                                                                                         
            {error, "", Reason}
    end.

post(Url, Data) ->
    case http_post(Url, [{"connection", "close"}], "application/x-www-form-urlencoded", Data, [], []) of
        {ok, {_Status, Body}} ->
            {ok, "", Body};
        {ok, {_StatusLine, Headers, Body}} ->
            {ok, Headers, Body};
        {error,Reason} ->
            {error, "", Reason}
    end.
