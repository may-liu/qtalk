-module(msg_id_queue).

-behaviour(gen_server).
-export([start_link/2,init/1]).

-export([handle_call/3, handle_cast/2,
 	    handle_info/2, terminate/2, code_change/3,queue_in/2]).

-include("ejabberd.hrl").
-include("logger.hrl").

-define(SERVER, ?MODULE).
-define(MAXSIZE, 300).

-record(state,{server,msg_queue,max_size,queue_in_timer}).

start_link(Host,Opts) ->
	 gen_server:start_link(?MODULE, [Host,Opts], []).

init([Server,_Opts]) ->
	mod_msg_id_queue:add_pid(Server,self()),
	Timer_Tref = 
    	   erlang:start_timer(5000,self(),update_queue),
	{ok, #state{server = Server,max_size = ?MAXSIZE,queue_in_timer = Timer_Tref,msg_queue = queue:new()}}.

handle_call(stop, _From, State=#state{queue_in_timer = Timer_Tref}) ->
    {ok, cancel} = timer:cancel(Timer_Tref),
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({in, Msg}, State = #state{max_size=Max,msg_queue=OldQueue}) ->
	Queue = 
		case queue:member(Msg,OldQueue) of
		true ->
			?INFO_MSG("ID [~p] is already in queue. ~n",[Msg]),
			OldQueue;
		_ ->
			case Max =< queue:len(OldQueue) of
			true ->
				Q1 = queue:liat(OldQueue),
				queue:in(Msg, Q1);
			_ ->
				queue:in(Msg,OldQueue)
			end
		end,	
		
	{noreply, State#state{msg_queue = Queue}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, TimerRef, update_queue},State=#state{server = Server,msg_queue = Queue}) ->
	case erlang:read_timer(TimerRef) of
	false ->
		do_update_msg_id(Server,Queue),
		New_Timer_Tref = erlang:start_timer(5000,self(),update_queue),
		NewQueue = queue:new(),
		NewState = State#state{queue_in_timer = New_Timer_Tref,msg_queue = NewQueue},
		{noreply, NewState};
	_ ->
		do_update_msg_id(Server,Queue),
		NewQueue = queue:new(),
		NewState = State#state{msg_queue = NewQueue},
		{noreply, NewState}
	end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State=#state{server = Host}) ->
	mod_msg_id_queue:remove_pid(Host,self()),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

get_proc_name(Host) ->
    gen_mod:get_module_proc(Host, ?MODULE).

do_update_msg_id(Host,Queue) ->
	IDs = queue:to_list(Queue),
	case IDs of
	[] ->
		ok;
	_ ->
		catch asy_sql:update_readmark_by_id(Host,IDs)
	end.

queue_in(Msg,Host) ->
	gen_server:cast(mod_msg_id_queue:get_pid_by_id(Host,Msg), {in, Msg}).
