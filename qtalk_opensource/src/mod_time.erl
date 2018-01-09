%%%----------------------------------------------------------------------
%%% File    : mod_time.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : 
%%% Purpose :
%%% Created : 18 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2014   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

-module(mod_time).

-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2, stop/1, process_local_iq90/3,
	 process_local_iq/3]).

% TODO: Remove once XEP-0090 is Obsolete

-export([get_timestamp/0,datetime_to_timestamp/1,timestamp_to_datetime/1,get_month_first_day/1,get_datetime_before_day/2]).
-export([get_timestamp_of_end_day/1,get_timestamp_of_start_day/1,timestamp_to_datetime_utc1/1,deal_timestamp/1,get_exact_timestamp/0]).

-define(DAY_TIMESTAMP,86400).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_TIME90, ?MODULE, process_local_iq90,
				  IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_TIME, ?MODULE, process_local_iq, IQDisc).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
				     ?NS_TIME90),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
				     ?NS_TIME).

%% TODO: Remove this function once XEP-0090 is Obsolete
process_local_iq90(_From, _To,
		   #iq{type = Type, sub_el = SubEl} = IQ) ->
    case Type of
      set ->
	  IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
      get ->
	  UTC = jlib:timestamp_to_iso(calendar:universal_time()),
	  IQ#iq{type = result,
		sub_el =
		    [#xmlel{name = <<"query">>,
			    attrs = [{<<"xmlns">>, ?NS_TIME90}],
			    children =
				[#xmlel{name = <<"utc">>, attrs = [],
					children = [{xmlcdata, UTC}]}]}]}
    end.

process_local_iq(_From, _To,
		 #iq{type = Type, sub_el = SubEl} = IQ) ->
    case Type of
      set ->
	  IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
      get ->
	  Now = os:timestamp(),
	  Now_universal = calendar:now_to_universal_time(Now),
	  Now_local = calendar:now_to_local_time(Now),
	  {UTC, UTC_diff} = jlib:timestamp_to_iso(Now_universal,
						  utc),
	  Seconds_diff =
	      calendar:datetime_to_gregorian_seconds(Now_local) -
		calendar:datetime_to_gregorian_seconds(Now_universal),
	  {Hd, Md, _} =
	      calendar:seconds_to_time(abs(Seconds_diff)),
	  {_, TZO_diff} = jlib:timestamp_to_iso({{0, 1, 1},
						 {0, 0, 0}},
						{sign(Seconds_diff), {Hd, Md}}),
	  IQ#iq{type = result,
		sub_el =
              [#xmlel{name = <<"time">>,
                  attrs = [{<<"xmlns">>, ?NS_TIME}],
			    children =
				[#xmlel{name = <<"tzo">>, attrs = [],
					children = [{xmlcdata, TZO_diff}]},
				 #xmlel{name = <<"utc">>, attrs = [],
					children =
					    [{xmlcdata,
                                              <<UTC/binary,
                                                UTC_diff/binary>>}]}]}]}
    end.

sign(N) when N < 0 -> <<"-">>;
sign(_) -> <<"+">>.

get_timestamp() ->
	{MegaSecs, Secs,_MicroSec} = os:timestamp(),
	MegaSecs * 1000000 + Secs.

get_exact_timestamp() ->
	{MegaSecs, Secs,MicroSec} = os:timestamp(),
	1000000000 * MegaSecs + Secs * 1000 + MicroSec div 1000.

get_msec_timestamp(Time) ->
	{MegaSecs, Secs,MicroSec} = Time,
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
