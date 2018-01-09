-module(ejb_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("logger.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-record(state,{cowboy_opts,http_port}).

start_link(Opts) ->

    gen_server:start_link(?MODULE, [Opts], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([_Args]) ->
	register(ejb_server,self()),
%%	Cowboy_Opts = proplists:get_value(cowboy_opts,Args),
	Start_Opts = ejb_http_server_env:get_env(ejb_http_server,route_rules,[]),
	Port_Opts = ejb_http_server_env:get_env(ejb_http_server,http_port,[{port,10056}]),

	Http_port = proplists:get_value(port,Port_Opts),
    {ok, #state{cowboy_opts = Start_Opts,http_port = Http_port}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(start_cowboy, #state{cowboy_opts = Opts,http_port = Http_port} = State) ->
	start_cowboy(Opts,Http_port),
    {noreply, State};
handle_cast(restart_cowboy,State) ->
	ejb_http_server_config:reload_conf("./config/ejb_http_server.config",ejb_http_server),
	Start_Opts = ejb_http_server_env:get_env(ejb_http_server,route_rules,[]),
	Http_port = ejb_http_server_env:get_env(ejb_http_server,http_port,10056),		
	start_cowboy(Start_Opts,Http_port),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
	cowboy:stop_listener(http),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

start_cowboy(Opts,Port) ->	
	Dispatch = cowboy_router:compile([{'_', Opts}]),
	cowboy:stop_listener(http),
    {ok,_ } = 
		cowboy:start_http(http, 600, [{port,Port}], 
			[{compress, true},{env, [{dispatch, Dispatch},{max_connections, infinity}]} ]),
	?INFO_MSG("start_cowboy ok  ~n",[]).
