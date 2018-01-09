-module(ejb_http_server_app).

-behaviour(application).

-include("ejb_http_server.hrl").
%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ejb_http_server_sup:start_link().

stop(_State) ->
    ok.

