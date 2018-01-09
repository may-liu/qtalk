-module(mod_cowboy).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		                terminate/2,code_change/3]).
-behaviour(gen_server).

-define(SERVER, ?MODULE).
-include("ejabberd.hrl").

-record(state, {info}).

start_link(Info) ->
    gen_server:start_link(?MODULE, [], [Info]).

init(Info) ->
	{ok, #state{info = ok}, 0}.

handle_call(Msg, _From, State) ->
    {reply, {ok, Msg}, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(timeout, State) ->
	cowboy_app:start(1,2),
	{noreply,State}.

terminate(_Reason, State) ->
	{ok,State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



