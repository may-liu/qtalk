-module(validator).
-export([handler/2]).

-record(e, {lt = 0, gt = 0, amp = 0, apos = 0, quot = 0}).

handler(_, {call, 'validator1.arrayOfStructsTest', [{array, Array}]}) ->
    {false, {response, [sum_array(Array)]}};
handler(_, {call, 'validator1.countTheEntities', [String]}) ->
    {false, {response, [count(String, #e{})]}};
handler(_, {call, 'validator1.easyStructTest', [{struct, Elements}]})->
    {false, {response, [sum_struct(Elements)]}};
handler(_, {call, 'validator1.echoStructTest', [Struct]}) ->
    {false, {response, [Struct]}};
handler(_, {call, 'validator1.manyTypesTest', Params}) ->
    {false, {response, [{array, Params}]}};
handler(_, {call, 'validator1.moderateSizeArrayCheck', [{array, Values}]}) ->
    {false, {response, [hd(Values)++hd(lists:reverse(Values))]}};
handler(_, {call, 'validator1.nestedStructTest', [{struct, Years}]}) ->
    {value, {_, {struct, Months}}} = lists:keysearch('2000', 1, Years),
    {value, {_, {struct, Days}}} = lists:keysearch('04', 1, Months),
    {value, {_, {struct, Elements}}} = lists:keysearch('01', 1, Days),
    {false, {response, [sum_struct(Elements)]}};
handler(_, {call, 'validator1.simpleStructReturnTest', [N]}) ->
    Elements = [{times10, N*10}, {times100, N*100}, {times1000, N*1000}], 
    {false, {response, [{struct, Elements}]}}.

sum_array([]) -> 0;
sum_array([{struct, Elements}|Rest]) ->
    {value, {_, N}} = lists:keysearch('curly', 1, Elements),
    N+sum_array(Rest).

count([], E) ->
    {struct, [{ctLeftAngleBrackets, E#e.lt}, {ctRightAngleBrackets, E#e.gt},
	      {ctAmpersands, E#e.amp}, {ctApostrophes, E#e.apos},
	      {ctQuotes, E#e.quot}]};
count([$<|Rest], #e{lt = LT} = E) -> count(Rest, E#e{lt = LT+1});
count([$>|Rest], #e{gt = GT} = E) -> count(Rest, E#e{gt = GT+1});
count([$&|Rest], #e{amp = AMP} = E) -> count(Rest, E#e{amp = AMP+1});
count([$'|Rest], #e{apos = APOS} = E) -> count(Rest, E#e{apos = APOS+1});
count([$"|Rest], #e{quot = QUOT} = E) -> count(Rest, E#e{quot = QUOT+1});
count([_|Rest], E) -> count(Rest, E).

sum_struct([]) -> 0;
sum_struct([{moe, N}|Rest]) -> N+sum_struct(Rest);
sum_struct([{larry, N}|Rest]) -> N+sum_struct(Rest);
sum_struct([{curly, N}|Rest]) -> N+sum_struct(Rest);
sum_struct([_|Rest]) -> sum_struct(Rest).
