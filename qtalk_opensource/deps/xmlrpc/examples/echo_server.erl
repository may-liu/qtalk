-module(echo_server).
-export([handler/2]).

%%
%% A server which echoes back any incoming parameters.
%%

handler(_, {call, echo, Params}) ->
    {false, {response, [{array, Params}]}};
handler(_, Payload) ->
    FaultString = lists:flatten(io_lib:format("Unknown call: ~p", [Payload])),
    {false, {response, {fault, -1, FaultString}}}.
