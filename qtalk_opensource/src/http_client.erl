-module(http_client).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		                terminate/2,code_change/3]).
-export([http_post/7,http_get/5,get/2,post/3]).
-behaviour(gen_server).

-define(SERVER, ?MODULE).
-include("ejabberd.hrl").
-include("logger.hrl").

-record(state, {host,pid,profile}).

start_link(Host,Opts) ->
    gen_server:start_link(?MODULE, [Host,Opts], []).

init([Host,Opts]) ->
	mod_http_client:add_pid(Host,self()),
	Seq =  proplists:get_value(<<"seq">>,Opts),
	Profile = list_to_atom("http_client_" ++ integer_to_list(Seq)),
	{ok, Pid} = inets:start(httpc, [{profile, Profile}]),
    {ok, #state{host = Host,pid = Pid,profile = Profile}}.
%%同步
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

handle_info(_From,State) ->
    {noreply,State}.

terminate(_Reason, State) ->
	inets:stop(httpc, State#state.pid),
	mod_http_client:remove_pid(State#state.host,self()),
	{ok,State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


do_call(Pid, Message) ->
    try gen_server:call(Pid, Message)
    catch
 	   Type:Error ->
 		   {error, Error}
    end.

http_post(Host,Url, Header, Type, Body, HTTPOptions, Options) ->
	Http_client_pid  = mod_http_client:get_random_pid(Host),
	do_call(Http_client_pid, {http_post,Url, Header, Type, Body, HTTPOptions, Options}).

http_get(Host,Url,Header, HTTPOptions, Options) ->
	Http_client_pid  = mod_http_client:get_random_pid(Host),
	do_call(Http_client_pid, {http_get,Url, Header, HTTPOptions, Options}).

get(Host,Url) ->
    case http_get(Host,Url, [{"connection", "close"}], [], []) of
    {ok, {_Status, Body}} ->
		{ok, "", Body};
    {ok, {_StatusLine, Headers, Body}} ->
        {ok, Headers, Body};
    {error,Reason} ->                                                                                                                             
	    {error, "", Reason}
	 end.

post(Host,Url, Data) ->
    case http_post(Host,Url, [{"connection", "close"}], "application/x-www-form-urlencoded", Data, [], []) of
    {ok, {_Status, Body}} ->
       {ok, "", Body};
    {ok, {_StatusLine, Headers, Body}} ->
       {ok, Headers, Body};
    {error,Reason} ->
       {error, "", Reason}
    end.

