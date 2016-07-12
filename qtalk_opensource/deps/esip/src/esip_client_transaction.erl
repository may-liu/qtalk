%%%----------------------------------------------------------------------
%%% File    : esip_client_transaction.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Purpose : 
%%% Created : 20 Dec 2010 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% p1_sip, Copyright (C) 2002-2015   ProcessOne
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
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(esip_client_transaction).

-behaviour(gen_fsm).

%% API
-export([start_link/4, start/3, start/4, stop/1, route/2, cancel/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4,
         handle_info/3, terminate/3, code_change/4]).

%% gen_fsm states
-export([trying/2,
         proceeding/2,
         accepted/2,
         completed/2]).

-include("esip.hrl").
-include("esip_lib.hrl").

-define(MAX_TRANSACTION_LIFETIME, timer:minutes(5)).

-record(state, {req, tu, sock, branch, cancelled = false}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(SIPSocket, Request, TU, Opts) ->
    gen_fsm:start_link(?MODULE, [SIPSocket, Request, TU, Opts], []).

start(SIPSocket, Request, TU) ->
    start(SIPSocket, Request, TU, []).

start(SIPSocket, Request, TU, Opts) ->
    case esip_tmp_sup:start_child(
	   esip_client_transaction_sup,
	   ?MODULE, gen_fsm, [SIPSocket, Request, TU, Opts]) of
        {ok, Pid} ->
            {ok, make_trid(Pid)};
	{error, _} = Err ->
            Err
    end.

route(Pid, R) ->
    gen_fsm:send_event(Pid, R).

cancel(Pid, TU) ->
    gen_fsm:send_event(Pid, {cancel, TU}).

stop(Pid) ->
    gen_fsm:send_all_state_event(Pid, stop).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================
init([SIPSocket, Request, TU, _Opts]) ->
    gen_fsm:send_event(self(), Request),
    erlang:send_after(?MAX_TRANSACTION_LIFETIME, self(), timeout),
    {ok, trying, #state{tu = TU, sock = SIPSocket}}.

trying(#sip{type = request, hdrs = Hdrs, method = Method} = Request,
       #state{sock = #sip_socket{type = Type}} = State) ->
    Branch = esip:get_branch(Hdrs),
    esip_transaction:insert(Branch, Method, client, self()),
    T1 = esip:timer1(),
    if Type == udp, Method == <<"INVITE">> ->
	    gen_fsm:send_event_after(T1, {timer_A, T1});
       Type == udp ->
	    gen_fsm:send_event_after(T1, {timer_E, T1});
       true ->
	    ok
    end,
    if Method == <<"INVITE">> ->
	    gen_fsm:send_event_after(64*T1, timer_B);
       true ->
	    gen_fsm:send_event_after(64*T1, timer_F)
    end,
    NewState = State#state{branch = Branch, req = Request},
    case send(NewState, Request) of
	ok ->
	    {next_state, trying, NewState};
	_ ->
	    {stop, normal, NewState}
    end;
trying({timer_A, T}, State) ->
    gen_fsm:send_event_after(2*T, {timer_A, 2*T}),
    case send(State, State#state.req) of
        ok ->
            {next_state, trying, State};
        _ ->
            {stop, normal, State}
    end;
trying({timer_E, T}, State) ->
    T4 = esip:timer4(),
    case 2*T < T4 of
        true ->
            gen_fsm:send_event_after(2*T, {timer_E, 2*T});
        false ->
            gen_fsm:send_event_after(T4, {timer_E, T4})
    end,
    case send(State, State#state.req) of
        ok ->
            {next_state, trying, State};
        _ ->
            {stop, normal, State}
    end;
trying(Timer, State) when Timer == timer_B; Timer == timer_F ->
    pass_to_transaction_user(State, {error, timeout}),
    {stop, normal, State};
trying(#sip{type = response} = Resp, State) ->
    case State#state.cancelled of
        {true, TU} ->
            gen_fsm:send_event(self(), {cancel, TU});
        _ ->
            ok
    end,
    proceeding(Resp, State);
trying({cancel, TU}, State) ->
    {next_state, trying, State#state{cancelled = {true, TU}}};
trying(_Event, State) ->
    {next_state, trying, State}.

proceeding(#sip{type = response, status = Status} = Resp, State) when Status < 200 ->
    pass_to_transaction_user(State, Resp),
    {next_state, proceeding, State};
proceeding(#sip{type = response, status = Status} = Resp,
           #state{req = #sip{method = <<"INVITE">>}} = State) when Status < 300 ->
    pass_to_transaction_user(State, Resp),
    gen_fsm:send_event_after(64*esip:timer1(), timer_M),
    {next_state, accepted, State};
proceeding(#sip{type = response, status = Status} = Resp,
           #state{req = #sip{method = <<"INVITE">>}} = State) when Status >= 300 ->
    pass_to_transaction_user(State, Resp),
    if (State#state.sock)#sip_socket.type == udp ->
            gen_fsm:send_event_after(64*esip:timer1(), timer_D),
            case send_ack(State, Resp) of
                ok ->
                    {next_state, completed, State};
                _ ->
                    {stop, normal, State}
            end;
       true ->
            send_ack(State, Resp),
            {stop, normal, State}
    end;
proceeding(#sip{type = response} = Resp, State) ->
    pass_to_transaction_user(State, Resp),
    if (State#state.sock)#sip_socket.type == udp ->
            gen_fsm:send_event_after(esip:timer4(), timer_K),
            {next_state, completed, State};
       true ->
            {stop, normal, State}
    end;
proceeding({timer_E, T}, State) ->
    gen_fsm:send_event_after(esip:timer2(), {timer_E, T}),
    case send(State, State#state.req) of
        ok ->
            {next_state, proceeding, State};
        _ ->
            {stop, normal, State}
    end;
proceeding(timer_F, State) ->
    pass_to_transaction_user(State, {error, timeout}),
    {stop, normal, State};
proceeding({cancel, TU}, #state{req = #sip{hdrs = Hdrs} = Req} = State) ->
    Hdrs1 = esip:filter_hdrs(['call-id', 'to', 'from', 'cseq',
                              'max-forwards', 'route'], Hdrs),
    [Via|_] = esip:get_hdrs('via', Hdrs),
    Hdrs2 = case esip:get_config_value(software) of
                undefined ->
                    [{'via', [Via]}|Hdrs1];
                UA ->
                    [{'via', [Via]},{'user-agent', UA}|Hdrs1]
            end,
    CancelReq = #sip{type = request,
                     method = <<"CANCEL">>,
                     uri = Req#sip.uri,
                     hdrs = Hdrs2},
    esip_client_transaction:start(State#state.sock, CancelReq, TU),
    {next_state, proceeding, State};
proceeding(_Event, State) ->
    {next_state, proceeding, State}.

accepted(timer_M, State) ->
    {stop, normal, State};
accepted(#sip{type = response, status = Status} = Resp, State)
  when Status >= 200, Status < 300 ->
    pass_to_transaction_user(State, Resp),
    {next_state, accepted, State};
accepted(_Event, State) ->
    {next_state, accepted, State}.

completed(timer_D, State) ->
    {stop, normal, State};
completed(timer_K, State) ->
    {stop, normal, State};
completed(#sip{type = response, status = Status} = Resp, State) when Status >= 300 ->
    case send_ack(State, Resp) of
        ok ->
            {next_state, completed, State};
        _ ->
            {stop, normal, State}
    end;
completed(_Event, State) ->
    {next_state, completed, State}.

handle_event(stop, _StateName, State) ->
    {stop, normal, State};
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info(timeout, _StateName, State) ->
    pass_to_transaction_user(State, {error, timeout}),
    {stop, normal, State};
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, #state{req = Req, branch = Branch}) ->
    if Req /= undefined ->
            catch esip_transaction:delete(Branch, Req#sip.method, client);
       true ->
            ok
    end.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
pass_to_transaction_user(#state{tu = TU, sock = Sock}, Resp) ->
    TrID = make_trid(),
    case TU of
        F when is_function(F) ->
            esip:callback(F, [Resp, Sock, TrID]);
        {M, F, A} ->
            esip:callback(M, F, [Resp, Sock, TrID | A]);
        _ ->
            TU
    end.

send_ack(#state{req = #sip{uri = URI, hdrs = Hdrs,
                           method = <<"INVITE">>}} = State, Resp) ->
    Hdrs1 = esip:filter_hdrs(['call-id', 'from', 'cseq',
                              'route', 'max-forwards',
                              'authorization',
                              'proxy-authorization'], Hdrs),
    To = esip:get_hdr('to', Resp#sip.hdrs),
    [Via|_] = esip:get_hdrs('via', Hdrs),
    Hdrs2 = case esip:get_config_value(software) of
                undefined ->
                    [{'via', [Via]},{'to', To}|Hdrs1];
                Software ->
                    [{'via', [Via]},{'to', To},{'user-agent', Software}|Hdrs1]
            end,
    ACK = #sip{type = request,
               uri = URI,
               method = <<"ACK">>,
               hdrs = Hdrs2},
    send(State, ACK);
send_ack(_, _) ->
    ok.

send(State, Resp) ->
    case esip_transport:send(State#state.sock, Resp) of
        ok ->
            ok;
        {error, _} = Err ->
            pass_to_transaction_user(State, Err),
            Err
    end.

make_trid() ->
    make_trid(self()).

make_trid(Pid) ->
    #trid{owner = Pid, type = client}.
