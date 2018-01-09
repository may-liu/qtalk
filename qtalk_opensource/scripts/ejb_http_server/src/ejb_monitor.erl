-module(ejb_monitor).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-include("logger.hrl").
-include("ejb_http_server.hrl").

-record(monitor_rec,{key,count=0}).
%%-record(monitor_val,{key,value}).
-record(state,{}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).
-export([add_ejb_http_monitor/1,monitor_count/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
	

start_link(Opts) ->
    gen_server:start_link(?MODULE, [Opts], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([_Args]) ->
%%	catch ets:new(monitor_val,		[named_table, ordered_set, public,{keypos, 2},{write_concurrency, true}, {read_concurrency, true}]),
    register(ejb_monitor,self()),
	catch ets:new(monitor_count,	[named_table, ordered_set, public,{keypos, 2},{write_concurrency, true}, {read_concurrency, true}]),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({monitor_count,Key,Val}, State) ->
	case catch ets:lookup(monitor_count,Key) of
	[] ->
		catch ets:insert(monitor_count,#monitor_rec{key = Key,count = Val});
	[Mc] ->
		catch ets:insert(monitor_count,#monitor_rec{key = Key,count = Mc#monitor_rec.count + Val})
	end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
%%	catch ets:delete(monitor_val),
	catch ets:delete(monitor_count),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

add_ejb_http_monitor(Ejabberd_monintr_info) ->
	Ejb_http_monitor = ets:foldr(fun(Mc,Acc) ->
			 case erlang:is_record(Mc,monitor_rec) of
		 	 true ->
			 	[Mc#monitor_rec.key,<<"_Count=">> ,integer_to_binary(Mc#monitor_rec.count),<<"\n">>] ++ Acc;
			 _ ->
			 	Acc
			end end,[],monitor_count),
	catch ets:delete_all_objects(monitor_count),
	ets:insert(cache_info,#cache_info{name = <<"monitor_info">>,cache = list_to_binary(Ejabberd_monintr_info ++ Ejb_http_monitor)}).

monitor_count(Key,Val) ->
	catch gen_server:cast('ejb_monitor',{monitor_count,Key,Val}).
