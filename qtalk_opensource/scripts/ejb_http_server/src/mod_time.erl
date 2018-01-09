-module(mod_time).

-export([get_timestamp/0,datetime_to_timestamp/1,timestamp_to_datetime/1,get_month_first_day/1,get_datetime_before_day/2]).
-export([get_timestamp_of_end_day/1,get_timestamp_of_start_day/1,timestamp_to_datetime_utc1/1,deal_timestamp/1,get_exact_timestamp/0]).

-define(DAY_TIMESTAMP,86400).

-include("logger.hrl").

get_timestamp() ->
	{MegaSecs, Secs,_MicroSec} = os:timestamp(),
	MegaSecs * 1000000 + Secs.

get_exact_timestamp() ->
	{MegaSecs, Secs,MicroSec} = os:timestamp(),
	 1000000000 * MegaSecs + Secs * 1000 + MicroSec div 1000.

deal_timestamp(Time) ->
	{MegaSecs, Secs,_MicroSec} = Time,
	MegaSecs * 1000000 + Secs.
%%UTC+8:use {{1970,1,1}, {8,0,0},not use {{1970,1,1}, {0,0,0}
datetime_to_timestamp(DateTime) ->
	calendar:datetime_to_gregorian_seconds(DateTime) -
		calendar:datetime_to_gregorian_seconds({{1970,1,1}, {8,0,0}}).

%%UTC+8:use {{1970,1,1}, {8,0,0},not use {{1970,1,1}, {0,0,0}
timestamp_to_datetime(Timestamp) ->
	calendar:gregorian_seconds_to_datetime(Timestamp +
		calendar:datetime_to_gregorian_seconds({{1970,1,1}, {8,0,0}})).

timestamp_to_datetime_utc1(Timestamp) ->
    calendar:gregorian_seconds_to_datetime(Timestamp +
		      calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}})).

get_month_first_day(Timestamp) ->
	case mod_time:timestamp_to_datetime(Timestamp) of
	{{Year,Month,_Day},{_,_,_}} ->
		mod_time:datetime_to_timestamp({{Year,Month,1},{0,0,0}});
	_ ->
		0
	end.

get_datetime_before_day(Timestamp,Day) ->
	Timestamp - ?DAY_TIMESTAMP * Day.

get_timestamp_of_end_day(Timestamp) ->
	case mod_time:timestamp_to_datetime(Timestamp)  of
	{{Year,Month,Day},{_,_,_}} ->
		datetime_to_timestamp({{Year,Month,Day},{23,59,59}});
	_ ->
		0
	end.

get_timestamp_of_start_day(Timestamp) ->
	case  mod_time:timestamp_to_datetime(Timestamp)  of
	{{Year,Month,Day},{_,_,_}} ->
		datetime_to_timestamp({{Year,Month,Day},{0,0,0}});
	 _ ->
			0
	end.
