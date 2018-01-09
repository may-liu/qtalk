-module(ejb_http_server_config).

-export([reload_conf/2]).

%%--------------------------------------------------------------------
%% @date 2015-09
%% 重新读取配置文件
%%--------------------------------------------------------------------

reload_conf(File, App) ->
    {ok, [Config]} = file:consult(File),
    lists:foldl(fun({A, C}, Acc) when A =:= App ->
                    R = set_env(App, C),
                    [{A, R}|Acc];
                (_, Acc) ->
                    Acc
            end, [], Config).

%%--------------------------------------------------------------------
%% @date 2015-09
%% 设置环境参数
%%--------------------------------------------------------------------
set_env(App, C) ->
    lists:foldl(fun({K, V}, Acc) ->
                    application:set_env(App, K, V),
                    [K|Acc]
            end, [], C).
