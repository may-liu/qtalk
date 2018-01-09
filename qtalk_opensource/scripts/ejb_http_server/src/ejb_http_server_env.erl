-module(ejb_http_server_env).

-include("ejb_http_server.hrl").

-export([get_env/1, get_env/2, get_env/3, get_env/4]).

-define(DEFAULT_APP, ejb_http_server).

get_env(Key) ->
    get_env(Key, undefined).

get_env(Key, Default) ->
    get_env(?DEFAULT_APP, Key, Default).

get_env(App, Key, Default) ->
    get_env(App, Key, fun(X) ->
                          X
                      end,
                      Default).

get_env(App, Key, Fun, Default) ->
    case application:get_env(App, Key) of
        {ok, Value} ->
            Fun(Value);
        undefined ->
            Default
    end.
