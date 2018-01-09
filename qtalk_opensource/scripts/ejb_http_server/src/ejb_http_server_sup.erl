-module(ejb_http_server_sup).

-include("ejb_http_server.hrl").

-behaviour(supervisor).

%% API
-export([start_link/0]).

-export([
         redis_spec/1,
         http_client_spec/1,
		 ejb_cache_spec/1,
         pg_odbc_spec/1,
		 ejb_server_spec/1,
		 ejb_monitor_spec/1,
%		 transfer_odbc_spec/1,
		 day_check_spec/1
        ]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	ejb_update_cache:create_ets_table(),
    ChildrenSpec = children_spec(),
    {ok, {{one_for_one, 5, 10}, ChildrenSpec}}.

children_spec() ->
    Options = ejb_http_server_env:get_env(start_options),
    lists:map(fun({_Id, OptionFlag, Function}) ->
                Option = proplists:get_value(OptionFlag, Options, []),
                ?MODULE:Function(Option)
        end, [
              {pg_odbc, pg_odbc, pg_odbc_spec},
              {redis, redis, redis_spec},
			  {ejb_cache,ejb_cache,ejb_cache_spec},
	%		  {transfer_odbc,transfer_odbc,transfer_odbc_spec},
              {http_client, http_client, http_client_spec},
			  {ejb_server,ejb_server,ejb_server_spec},
			  {ejb_monitor,ejb_monitor,ejb_monitor_spec},
			  {ejb_http_day_check,ejb_http_day_check,day_check_spec}
             ]).

redis_spec(Options) ->
    {redis_link_sup, {redis_link_sup, start_link, [Options]}, permanent, infinity, supervisor, [redis_link_sup]}.

pg_odbc_spec(Options) ->
    {pg_odbc_sup, {pg_odbc_sup, start_link, [Options]}, permanent, infinity, supervisor, [pg_odbc_sup]}.

http_client_spec(Options) ->
    {http_client_sup, {http_client_sup, start_link, [Options]}, permanent, infinity, supervisor, [http_client_sup]}.

ejb_cache_spec(Options) ->
    {ejb_cache, {ejb_cache, start_link, [Options]}, permanent, 5000, worker, [ejb_cache]}.

ejb_server_spec(Options) ->
    {ejb_server, {ejb_server, start_link, [Options]}, permanent, 5000, worker, [ejb_server]}.

ejb_monitor_spec(Options) ->
    {ejb_monitor, {ejb_monitor, start_link, [Options]}, permanent, 5000, worker, [ejb_monitor]}.

day_check_spec(Options) ->
    {ejb_http_day_check, {ejb_http_day_check, start_link, [Options]}, permanent, 5000, worker, [ejb_http_day_check]}.

%transfer_odbc_spec(Options) ->
 %   {transfer_odbc_sup, {transfer_odbc_sup, start_link, [Options]}, permanent, infinity, supervisor, [transfer_odbc_sup]}.
