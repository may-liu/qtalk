-module(date_server).
-export([handler/2]).

%%
%% A server which calculates date value(s) such as:
%%
%% * Calculates the number of days since year 0 for a given date.
%% * Calculates if a given date is a leap year.
%% * Calculates the day of the week for a given day.
%%
%% In order to perform the calculation a client must login with
%% username "Slarti" and password "Bartfast". After login the HTTP/1.1
%% keepalive timeout is set to infinity. The server returns a session
%% counter for each call using the state variable.
%%

handler({false, N}, {call, login, ["Slarti", "Bartfast"]}) ->
    {true, infinity, {true, N+1}, {response, ["ok"]}};
handler({false, N}, {call, login, [_, _]}) ->
    {false, {response, {fault, -1, "Invalid authentication"}}};
handler({true, N}, {call, login, [_, _]}) ->
    {true, infinity, {true, N+1},
     {response, {fault, -2, "Already authenticated"}}};
handler({true, N}, {call, logout, []}) ->
    {false, {response, ["ok"]}};
handler({true, N}, {call, calc, [{struct, Elements}]}) ->
    {true, infinity, {true, N+1},
     {response, [{array, [N, {struct, calc(Elements)}]}]}};
handler({false, N}, {call, calc, Params}) ->
    {false, {response, {fault, -3, "Not authenticated"}}};
handler({Status, N}, Payload) ->
    FaultString = lists:flatten(io_lib:format("Unknown call: ~p", [Payload])),
    {true, 300000, {Status, N+1}, {response, {fault, -4, FaultString}}}.

calc([]) -> [];
calc([{days_since, {struct, Elements}}|Rest]) ->
    {Y, M, D} = extract_date(Elements),
    [{days_since, calendar:date_to_gregorian_days({Y, M, D})}|
     calc(Rest)];
calc([{is_leap_year, Y}|Rest]) ->
    [{is_leap_year, calendar:is_leap_year(Y)}|calc(Rest)];
calc([{day_of_week, {struct, Elements}}|Rest]) ->
    {Y, M, D} = extract_date(Elements),
    [{day_of_week, day(calendar:day_of_the_week(Y, M, D))}|calc(Rest)].

extract_date(Elements) -> extract_date(Elements, 0, 0, 0).

extract_date([], Y, M, D) -> {Y, M, D};
extract_date([{y, Y}|Rest], _, M, D) -> extract_date(Rest, Y, M, D);
extract_date([{m, M}|Rest], Y, _, D) -> extract_date(Rest, Y, M, D);
extract_date([{d, D}|Rest], Y, M, _) -> extract_date(Rest, Y, M, D).

day(1) -> "Monday";
day(2) -> "Tuesday";
day(3) -> "Wednesday";
day(4) -> "Thursday";
day(5) -> "Friday";
day(6) -> "Saturday";
day(7) -> "Sunday".
