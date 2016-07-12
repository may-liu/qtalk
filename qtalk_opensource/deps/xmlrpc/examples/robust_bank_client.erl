-module(robust_bank_client).
-export([deposit/2, withdraw/2, balance/1]).

deposit(Who, X) -> robust_xmlrpc(deposit, [Who, X]).
withdraw(Who, X) -> robust_xmlrpc(withdraw, [Who, X]).
balance(Who) -> robust_xmlrpc(balance, [Who]).

robust_xmlrpc(F, A) ->
    Tag = mk_tag(),
    case call(3020, Tag, F, A) of
	{error, Reason} -> call(3030, Tag, F, A);
	Result -> Result
    end.

mk_tag() ->
    {MegaSecs, Secs, MicroSecs} = erlang:now(),
    lists:concat([atom_to_list(node()),
		  ".", integer_to_list(MegaSecs),
		  ".", integer_to_list(Secs),
		  ".", integer_to_list(MicroSecs)]).

call(Port, Tag, F, A) ->
    case xmlrpc:call("localhost", Port, "/", {call, F, [Tag|A]}, false,
		     10000) of
	{error, Reason} -> {error, Reason};
	{ok, Result} ->
	    xmlrpc:call("localhost", Port, "/", {call, delete_tag, [Tag]}),
	    Result
    end.
