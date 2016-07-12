-module(robust_bank_server).
-export([start/1, stop/1]).
-export([handler/2]). % internal

-include("reply.hrl").

start(Port) ->
    mnesia:start(),
    xmlrpc:start_link(Port, 100, 60*1000, {?MODULE, handler}, undefined).

stop(Pid) ->
    mnesia:stop(),
    xmlrpc:stop(Pid).

handler(_, {call, deposit, [Tag, Who, X]}) ->
    {false, {response, [lookup(Tag, deposit, [Who, X])]}};
handler(_, {call, withdraw, [Tag, Who, X]}) ->
    case lookup(Tag, withdraw, [Who, X]) of
	ok -> {false, {response, ["ok"]}};
	{error, Reason} ->
	    FaultString = lists:flatten(io_lib:format("~p", [Reason])),
	    {false, {response, {fault, -1, FaultString}}}
    end;
handler(_, {call, balance, [Tag, Who]}) ->
    case lookup(Tag, balance, [Who]) of
	{ok, Result} -> {false, {response, [Result]}};
	{error, Reason} ->
	    FaultString = lists:flatten(io_lib:format("~p", [Reason])),
	    {false, {response, {fault, -2, FaultString}}}
    end;
handler(_, {call, delete_tag, [Tag]}) ->
    mnesia:transaction(fun() -> mnesia:delete({reply, Tag}) end),
    {false, {response, ["ok"]}}.

lookup(Tag, F, A) ->
    Fun = fun() ->
		  case mnesia:read({reply, Tag}) of
		      [] ->
			  Val = get_val(F, A),
			  mnesia:write(#reply{tag = Tag, val = Val}),
			  Val;
		      [Reply] -> Reply#reply.val
		  end
	  end,
    {atomic, Result} = mnesia:transaction(Fun),
    Result.

get_val(deposit, [Who, X]) -> (bank:deposit(Who, X))();
get_val(withdraw, [Who, X]) -> (bank:withdraw(Who, X))();
get_val(balance, [Who]) -> (bank:balance(Who))().
