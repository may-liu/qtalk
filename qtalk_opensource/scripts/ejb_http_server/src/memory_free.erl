-module(memory_free).

-export([find_max_memory_process/1,memory_free/1]).

-include("logger.hrl").


find_max_memory_process(Num) ->
    ProcessL = processes() -- [self()],
    AttrName = memory,
    F = fun(Pid, L) ->
		case process_info(Pid, [AttrName, registered_name, current_function, initial_call]) of
		[Attr, Name, Init, Cur] ->
			Info = {Attr, [{pid, Pid}, Name, Init, Cur]},
			[Info | L];
		undefined ->
			L
		end	end,
	ProInfoL = lists:foldl(F, [], ProcessL),
	CompF = fun({A, _},{B, _}) ->  A > B     end,
	ProInfoSortL = lists:usort(CompF, ProInfoL),
	lists:sublist(ProInfoSortL, Num).

memory_free(Num) ->
	L = find_max_memory_process(Num),
	lists:foreach(fun({_,Info}) ->
			Pid = proplists:get_value(pid,Info),
			erlang:garbage_collect(Pid),
			?DEBUG("Pid ~p ~n",[Pid]) end,L).

		
			
