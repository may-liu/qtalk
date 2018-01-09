-module(randoms).

-author('alexey@process-one.net').

-export([get_string/0, integer_to_binary/1, integer_to_binary/2]).

-export([start/0, init/0]).

start() ->
    register(random_generator, spawn(randoms, init, [])).

init() ->
    {A1, A2, A3} = now(), random:seed(A1, A2, A3), loop().

loop() ->
    receive
      {From, get_random, N} ->
	  From ! {random, random:uniform(N)}, loop();
      _ -> loop()
    end.

get_string() ->
    random_generator ! {self(), get_random, 65536 * 65536},
    receive
      {random, R} -> randoms:integer_to_binary(R)
    end.

integer_to_binary(I) ->
    list_to_binary(integer_to_list(I)).

integer_to_binary(I, Base) ->
    list_to_binary(erlang:integer_to_list(I, Base)).

