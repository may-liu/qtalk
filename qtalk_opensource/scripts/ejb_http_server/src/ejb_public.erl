-module(ejb_public).
-export([get_url_by_domain/1,checek_domain/1,get_host/0]).
-export([md5_hex/1,format_time/1,check_time/1,handle_max_time/2]).
-export([pg2timestamp/1,get_value_with_default/2,get_user_nick/1,get_default_vcard/2]).
-export([check_virtual_user/2,check_access_limit/1,check_user_in_muc/2]).


-include("ejb_http_server.hrl").
-include("logger.hrl").

get_url_by_domain(Domain) ->
	case catch ets:lookup(domain_to_url,Domain)  of
	[D2U] when is_record(D2U,domain_to_url) ->
		D2U#domain_to_url.url;
	_ ->
		[]
	end.

checek_domain(Domain) ->
	Domain == undefined orelse Domain == get_host() orelse Domain == str:concat(<<"conference.">>,get_host()).

get_host() ->
	case catch  ets:lookup(ejabberd_config,<<"host">>) of
	[Ec] when is_record(Ec,ejabberd_config) ->
		Ec#ejabberd_config.val;
	_ ->
		<<"">>
	end.

md5_hex(S) ->
    Md5_bin =  erlang:md5(S),
    Md5_list = binary_to_list(Md5_bin),
    lists:flatten(list_to_hex(Md5_list)).

list_to_hex(L) ->
    lists:map(fun(X) -> int_to_hex(X) end, L).


int_to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 ->
    $0+N;
hex(N) when N >= 10, N < 16 ->
    $a + (N-10).

	
format_time(Time) when is_binary(Time)->
	handle_time(binary_to_integer(Time));
format_time(Time) when is_integer(Time)->
	handle_time(Time);
format_time(Time) when is_list(Time) ->
	handle_time(list_to_integer(Time));
format_time(Time) ->
	<<"error">>.

handle_time(Time) ->
	case Time > 1000000000000 of
	true ->
		Msec = Time rem 1000,
		Time1 = Time div 1000,
		do_format_time(Time1,Msec);
	_ ->
		do_format_time(Time,0)
	end.

do_format_time(Time,Msec) ->
	{{Y,M,D},{H,Mi,S}} = mod_time:timestamp_to_datetime(Time),
	list_to_binary(
			[integer_to_list(Y),"-",integer_to_list(M),"-",integer_to_list(D)," ",
				       integer_to_list(H),":",integer_to_list(Mi),":",integer_to_list(S),msec_time(Msec),integer_to_list(Msec)]).


msec_time(Msec) when Msec > 100 ->
	".";
msec_time(Msec) when Msec > 10 ->
	".0";
msec_time(Msec) ->
	".00".

check_time(Timestamp) ->
	Time = http_utils:to_integer(Timestamp),
	T = 
   		case Time > 1000000000000 of
		true ->
			Time div 1000;
		_ ->
			Time
		end,
	Month_ago = mod_time:get_timestamp() - 30*86400,
	if T > Month_ago ->
		true;
	true ->
		false
	end.

pg2timestamp(Time) when is_binary(Time)->
	do_pg2timestamp(binary_to_integer(Time));
pg2timestamp(Time) when is_list(Time)->
	do_pg2timestamp(list_to_integer(Time));
pg2timestamp(Time) when is_integer(Time) ->
	do_pg2timestamp(Time);
pg2timestamp(Time)  ->
	 <<"error">>.
	
do_pg2timestamp(T) ->
	Time = 
		if T > 1000000000000 ->
			T /1000;
		true ->
			T/1
		end,
	Str = str:concat(float_to_binary(Time),<<")">>),
	str:concat(<<"to_timestamp(">>,Str).

		
handle_max_time(Time,Direc) ->
        case Direc of
        <<"0">> ->
               if Time =:= 0 orelse Time =:= <<"0">> orelse Time =:= -1 orelse Time =:= <<"-1">> ->
            	   mod_time:get_timestamp();
			   true ->
				   ITime = http_utils:to_integer(Time),
                   if ITime > 15006744073709500 ->
                             mod_time:get_timestamp();
                   true ->
                          Time
                   end
                end;
       _ ->
              Time
       end.

get_value_with_default(null,Default) ->
	Default;
get_value_with_default(undefined,Default) ->
	Default;
get_value_with_default(Value,_) ->
	Value.


get_user_nick(User) ->
	case catch ets:lookup(user_name,User) of
	[{_,N}] when N =/= undefined  ->
		N;
	_ ->
		User
	end.
		
get_default_vcard(<<"">>,Def) ->
	Def;
get_default_vcard(null,Def) ->
	Def;
get_default_vcard(Url,Def) when is_binary(Url)->
	Url;
get_default_vcard(_,Def) ->
	Def.


check_virtual_user(VUser,RUser) ->
    case catch ets:lookup(virtual_users,<<VUser/binary,<<"_">>/binary,RUser/binary>>) of
    [{_,1}] ->   
            true;
    _ ->
        false
    end.   
      
update_one_virtual_user(VUser,RUser) ->
     case catch pg_odbc:sql_query(<<"ejb_http_server">>,
        [<<"select on_duty_flag from virtual_user_list where virtual_user = '">>,VUser,<<"' and real_user = '">>,RUser,<<"';">>]) of
     {selected, _ , [[<<"1">>]]} ->
            catch ets:insert(virtual_users,{<<VUser/binary,<<"_">>/binary,RUser/binary>>,1});
    _ ->
        ok
    end.
 
check_access_limit(User) ->
	Now = get_time(),
	case ets:lookup(access_limit,User) of
	[] ->
		ets:insert(access_limit,{User,{1,Now}}),
		true;
	[{_,{N,T}}] ->
		Diff = Now - T, 
		case Diff > 250000 of
		true ->
			ets:insert(access_limit,{User,{1,Now}}),
			true;
		_ ->
            if N > 3 -> 
			    false;
            true ->
                ets:insert(access_limit,{User,{N+1,T}}),
                true
            end
		end;
	_ ->
		false
	end.

get_time() ->
 	{MegaSecs, Secs,MicroSec} = os:timestamp(),
	MegaSecs * 1000000000000 + Secs*1000000 + MicroSec.

check_user_in_muc(User,Muc) ->
	case catch pg_odbc:sql_query(<<"ejb_http_server">>,
		[<<"select  username from muc_room_users where muc_name = '">>,Muc,<<"' and username = '">>,User,<<"';">>]) of
	{selected, _ , [[User]]} ->
		true;
	_ ->
		false
	end.
		
