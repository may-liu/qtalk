-module(fib_server).
-export([handler/2]).

%%
%% A server which calculates Fibonacci values.
%%

handler(_State, {call, fib, [N]}) when integer(N) ->
    {false, {response, [fib(N)]}};
handler(_State, Payload) ->
    FaultString = lists:flatten(io_lib:format("Unknown call: ~p", [Payload])),
    {false, {response, {fault, -1, FaultString}}}.

fib(0) -> 1;
fib(1) -> 1;
fib(N) -> fib(N-1)+fib(N-2).
