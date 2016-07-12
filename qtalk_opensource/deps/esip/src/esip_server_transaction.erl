%%%----------------------------------------------------------------------
%%% File    : esip_server_transaction.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Purpose : Server transaction layer. See RFC3261 and friends.
%%% Created : 15 Jul 2009 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
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

-module(esip_server_transaction).

-behaviour(gen_fsm).

%% API
-export([start_link/2, start/2, stop/1, route/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4,
	 handle_info/3, terminate/3, code_change/4]).

%% gen_fsm states
-export([trying/2,
	 proceeding/2,
         accepted/2,
	 completed/2,
         confirmed/2]).

-include("esip.hrl").
-include("esip_lib.hrl").

-define(MAX_TRANSACTION_LIFETIME, timer:minutes(5)).

-record(state, {sock, branch, method, resp, tu}).

%%====================================================================
%% API
%%====================================================================
start_link(SIPSocket, Request) ->
    gen_fsm:start_link(?MODULE, [SIPSocket, Request], []).

start(SIPSocket, Request) ->
    case esip_tmp_sup:start_child(esip_server_transaction_sup,
                                  ?MODULE, gen_fsm, [SIPSocket, Request]) of
        {ok, Pid} ->
            {ok, make_trid(Pid)};
        Err ->
            Err
    end.

route(Pid, R) ->
    gen_fsm:send_event(Pid, R).

stop(Pid) ->
    gen_fsm:send_all_state_event(Pid, stop).

%%====================================================================
%% gen_fsm callbacks
%%====================================================================
init([SIPSock, Request]) ->
    Branch = esip:get_branch(Request#sip.hdrs),
    State = #state{sock = SIPSock, branch = Branch},
    gen_fsm:send_event(self(), Request),
    erlang:send_after(?MAX_TRANSACTION_LIFETIME, self(), timeout),
    esip_transaction:insert(Branch, Request#sip.method, server, self()),
    {ok, trying, State}.

trying(#sip{method = <<"CANCEL">> = Method, type = request} = Req, State) ->
    Resp = esip:make_response(Req,
                              #sip{type = response, status = 200},
                              esip:make_tag()),
    proceeding(Resp, State#state{method = Method});
trying(#sip{type = request, method = Method} = Req, State) ->
    case find_transaction_user(Req, State#state.sock) of
        {dialog, TU} ->
            NewState = State#state{tu = TU, method = Method},
            case pass_to_transaction_user(NewState, Req) of
                #sip{type = response} = Resp ->
                    proceeding(Resp, NewState);
                wait ->
                    maybe_send_trying(Req),
                    {next_state, proceeding, NewState};
                NewTU ->
                    case is_transaction_user(NewTU) of
                        true ->
                            maybe_send_trying(Req),
                            {next_state, proceeding, NewState#state{tu = NewTU}};
                        false ->
                            Resp = esip:make_response(
                                     Req, #sip{status = 500, type = response},
                                     esip:make_tag()),
                            proceeding(Resp, State#state{method = Method})
                    end
            end;
        #sip{type = response} = Resp ->
            proceeding(Resp, State#state{method = Method});
        cseq_out_of_order ->
            Resp = esip:make_response(
                     Req, #sip{status = 500, type = response,
                               reason = <<"CSeq is Out of Order">>},
                     esip:make_tag()),
            proceeding(Resp, State#state{method = Method});
        wait ->
            maybe_send_trying(Req),
            {next_state, proceeding, State#state{method = Method}};
        TU ->
            case is_transaction_user(TU) of
                true ->
                    maybe_send_trying(Req),
                    {next_state, proceeding, State#state{tu = TU,
                                                         method = Method}};
                false ->
                    Resp = esip:make_response(
                             Req, #sip{status = 500, type = response},
                             esip:make_tag()),
                    proceeding(Resp, State#state{method = Method})
            end
    end;
trying(_Event, State) ->
    {next_state, trying, State}.

proceeding({trying, TryingResp}, #state{resp = undefined} = State) ->
    %% TU didn't respond in 200 ms
    case send(State, TryingResp) of
        ok ->
            {next_state, proceeding, State};
        _ ->
            {stop, normal, State}
    end;
proceeding(#sip{type = response, status = Status} = Resp, State) when Status < 200 ->
    update_remote_seqnum(Resp, State),
    case send(State, Resp) of
        ok ->
            {next_state, proceeding, State#state{resp = Resp}};
        _ ->
            {stop, normal, State}
    end;
proceeding(#sip{type = response, status = Status} = Resp,
           #state{method = <<"INVITE">>} = State) when Status < 300 ->
    update_remote_seqnum(Resp, State),
    gen_fsm:send_event_after(64*esip:timer1(), timer_L),
    case send(State, Resp) of
        ok ->
            {next_state, accepted, State#state{resp = Resp}};
        _ ->
            {stop, normal, State}
    end;
proceeding(#sip{type = response, status = Status} = Resp,
           #state{method = <<"INVITE">>} = State) when Status >= 300 ->
    update_remote_seqnum(Resp, State),
    T1 = esip:timer1(),
    if (State#state.sock)#sip_socket.type == udp ->
            gen_fsm:send_event_after(T1, {timer_G, T1});
       true ->
            ok
    end,
    gen_fsm:send_event_after(64*T1, timer_H),
    case send(State, Resp) of
        ok ->
            {next_state, completed, State#state{resp = Resp}};
        _ ->
            {stop, normal, State}
    end;
proceeding(#sip{type = response, status = Status} = Resp, State) when Status >= 200 ->
    update_remote_seqnum(Resp, State),
    if (State#state.sock)#sip_socket.type == udp ->
            gen_fsm:send_event_after(64*esip:timer1(), timer_J),
            case send(State, Resp) of
                ok ->
                    {next_state, completed, State#state{resp = Resp}};
                _ ->
                    {stop, normal, State}
            end;
       true ->
            send(State, Resp),
            {stop, normal, State}
    end;
proceeding(#sip{type = request, method = <<"CANCEL">>} = Req, State) ->
    if State#state.method == <<"INVITE">> ->
            case pass_to_transaction_user(State, Req) of
                #sip{type = response} = Resp ->
                    proceeding(Resp#sip{method = <<"INVITE">>}, State);
                _ ->
                    {next_state, proceeding, State}
            end;
       true ->
            {next_state, proceeding, State}
    end;
proceeding(#sip{type = request, method = Method} = Req,
           #state{method = Method, resp = Resp} = State) ->
    if Resp /= undefined ->
            case send(State, Resp) of
                ok ->
                    {next_state, proceeding, State};
                _ ->
                    {stop, normal, State}
            end;
       Method == <<"INVITE">> ->
            Trying = esip:make_response(Req, #sip{type = response,
                                                  status = 100}),
            case send(State, Trying) of
                ok ->
                    {next_state, proceeding, State};
                _ ->
                    {stop, normal, State}
            end;
       true ->
            {next_state, proceeding, State}
    end;
proceeding(_Event, State) ->
    {next_state, proceeding, State}.

accepted(#sip{type = request, method = <<"ACK">>} = Req, State) ->
    pass_to_transaction_user(State, Req),
    {next_state, accepted, State};
accepted(#sip{type = response, status = Status} = Resp, State)
  when Status >= 200, Status < 300 ->
    send(State, Resp),
    {next_state, accepted, State};
accepted(timer_L, State) ->
    {stop, normal, State};
accepted(_Event, State) ->
    {next_state, accepted, State}.

completed(#sip{type = request, method = <<"ACK">>},
          #state{method = <<"INVITE">>} = State) ->
    if (State#state.sock)#sip_socket.type == udp ->
            gen_fsm:send_event_after(esip:timer4(), timer_I),
            {next_state, confirmed, State};
       true ->
            {stop, normal, State}
    end;
completed(#sip{type = request, method = Method},
          #state{method = Method, resp = Resp} = State) ->
    case send(State, Resp) of
        ok ->
            {next_state, completed, State};
        _ ->
            {stop, normal, State}
    end;
completed(timer_H, State) ->
    pass_to_transaction_user(State, {error, timeout}),
    {stop, normal, State};
completed({timer_G, T}, State) ->
    T2 = esip:timer2(),
    case 2*T < T2 of
        true ->
            gen_fsm:send_event_after(2*T, {timer_G, 2*T});
        false ->
            gen_fsm:send_event_after(T2, {timer_G, T2})
    end,
    case send(State, State#state.resp) of
        ok ->
            {next_state, completed, State};
        _ ->
            {stop, normal, State}
    end;
completed(timer_J, State) ->
    {stop, normal, State};
completed(_Event, State) ->
    {next_state, completed, State}.

confirmed(timer_I, State) ->
    {stop, normal, State};
confirmed(_Event, State) ->
    {next_state, confirmed, State}.

handle_event(stop, _StateName, State) ->
    {stop, normal, State};
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

handle_info(timeout, _StateName, State) ->
    pass_to_transaction_user(State, {error, timeout}),
    {stop, normal, State};
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, #state{branch = Branch, method = Method}) ->
    esip_transaction:delete(Branch, Method, server).

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
is_transaction_user(TU) when is_function(TU) ->
    true;
is_transaction_user({M, F, A}) when is_atom(M), is_atom(F), is_list(A) ->
    true;
is_transaction_user(_) ->
    false.

pass_to_transaction_user(#state{tu = TU, sock = Sock}, Req) ->
    TrID = make_trid(),
    case TU of
        F when is_function(F) ->
            esip:callback(F, [Req, Sock, TrID]);
        {M, F, A} ->
            esip:callback(M, F, [Req, Sock, TrID | A]);
        #sip{type = response} = Resp ->
            Resp;
        _ ->
            TU
    end.

find_transaction_user(#sip{method = Method} = Req, SIPSock) ->
    TrID = make_trid(),
    case esip_dialog:id(uas, Req) of
        #dialog_id{local_tag = Tag} = DialogID when Tag /= <<>> ->
            case esip_dialog:lookup(DialogID) of
                {ok, TU, _Dialog} when Method == <<"OPTIONS">> ->
                    {dialog, TU};
                {ok, TU, #dialog{remote_seq_num = RemoteSeqNum}} ->
                    CSeq = esip:get_hdr('cseq', Req#sip.hdrs),
                    if is_integer(RemoteSeqNum), RemoteSeqNum > CSeq ->
                            cseq_out_of_order;
                       true ->
                            {dialog, TU}
                    end;
                _ ->
                    esip:callback(request, [Req, SIPSock, TrID])
            end;
        _ ->
            esip:callback(request, [Req, SIPSock, TrID])
    end.

send(State, Resp) ->
    case esip_transport:send(State#state.sock, Resp) of
        ok ->
            ok;
        Err ->
            pass_to_transaction_user(State, Err),
            Err
    end.

maybe_send_trying(#sip{method = <<"INVITE">>} = Req) ->
    Trying = esip:make_response(Req, #sip{type = response,
                                          status = 100}),
    gen_fsm:send_event_after(200, {trying, Trying});
maybe_send_trying(_) ->
    ok.

update_remote_seqnum(#sip{status = S}, _State) when S == 100; S >= 300 ->
    ok;
update_remote_seqnum(_Resp, #state{method = <<"OPTIONS">>}) ->
    ok;
update_remote_seqnum(#sip{hdrs = Hdrs} = Resp, _State) ->
    DialogID = esip_dialog:id(uas, Resp),
    CSeq = esip:get_hdr('cseq', Hdrs),
    esip_dialog:update_remote_seqnum(DialogID, CSeq).

make_trid() ->
    make_trid(self()).

make_trid(Pid) ->
    #trid{owner = Pid, type = server}.
