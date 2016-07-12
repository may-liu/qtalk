%%%----------------------------------------------------------------------
%%% File    : esip_transaction.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Purpose : Route client/server transactions
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

-module(esip_transaction).

-behaviour(gen_server).

%% API
-export([start_link/0, process/2, reply/2, cancel/2,
         request/3, request/4, insert/4, delete/3, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("esip.hrl").
-include("esip_lib.hrl").

-record(state, {}).

%%====================================================================
%% API
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

process(SIPSock, #sip{method = Method, hdrs = Hdrs, type = request} = Req) ->
    Branch = esip:get_branch(Hdrs),
    case lookup(transaction_key(Branch, Method, server)) of
	{ok, Pid} ->
	    esip_server_transaction:route(Pid, Req);
        error when Method == <<"ACK">> ->
            case esip_dialog:lookup(esip_dialog:id(uas, Req)) of
                {ok, Core, _} ->
                    pass_to_core(Core, Req, SIPSock);
                _Err ->
                    esip:callback(request, [Req, SIPSock])
            end;
	error when Method == <<"CANCEL">> ->
            case lookup({esip:to_lower(Branch), server}) of
                {ok, Pid} ->
                    esip_server_transaction:route(Pid, Req),
                    start_server_transaction(SIPSock, Req);
                error ->
                    case esip:callback(request, [Req, SIPSock]) of
                        #sip{type = response} = Resp ->
                            esip_transport:send(SIPSock, Resp);
                        drop ->
                            ok;
                        _ ->
                            Resp = esip:make_response(
                                     Req,
                                     #sip{type = response, status = 481},
                                     esip:make_tag()),
                            esip_transport:send(SIPSock, Resp)
                    end
            end;
        error ->
            start_server_transaction(SIPSock, Req)
    end;
process(SIPSock, #sip{method = Method, hdrs = Hdrs, type = response} = Resp) ->
    Branch = esip:get_branch(Hdrs),
    case lookup(transaction_key(Branch, Method, client)) of
        {ok, Pid} ->
            esip_client_transaction:route(Pid, Resp);
        _ ->
            esip:callback(response, [Resp, SIPSock])
    end.

reply(#trid{owner = Pid, type = server}, #sip{type = response} = Resp) ->
    esip_server_transaction:route(Pid, Resp);
reply(#sip{method = Method, type = request, hdrs = Hdrs},
      #sip{type = response} = Resp) ->
    Branch = esip:get_branch(Hdrs),
    case lookup(transaction_key(Branch, Method, server)) of
        {ok, Pid} ->
            esip_server_transaction:route(Pid, Resp);
        error ->
            ok
    end;
reply(_, _) ->
    ok.

request(SIPSock, Req, TU) ->
    request(SIPSock, Req, TU, []).

request(SIPSock, #sip{type = request} = Req, TU, Opts) ->
    start_client_transaction(SIPSock, Req, TU, Opts).

cancel(#sip{method = Method, type = request, hdrs = Hdrs}, TU) ->
    Branch = esip:get_branch(Hdrs),
    case lookup(transaction_key(Branch, Method, client)) of
        {ok, Pid} ->
            esip_client_transaction:cancel(Pid, TU);
        error ->
            ok
    end;
cancel(#trid{type = client, owner = Pid}, TU) ->
    esip_client_transaction:cancel(Pid, TU).

stop(#trid{owner = Pid, type = client}) ->
    esip_client_transaction:stop(Pid);
stop(#trid{owner = Pid, type = server}) ->
    esip_server_transaction:stop(Pid).

insert(Branch, Method, Type, Pid) ->
    ets:update_counter(?MODULE, {transaction_number, Type}, 1),
    ets:insert(?MODULE, {transaction_key(Branch, Method, Type), Pid}).

delete(Branch, Method, Type) ->
    ets:update_counter(?MODULE, {transaction_number, Type}, -1),
    ets:delete(?MODULE, transaction_key(Branch, Method, Type)).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    ets:new(?MODULE, [public, named_table]),
    ets:insert(?MODULE, {{transaction_number, client}, 0}),
    ets:insert(?MODULE, {{transaction_number, server}, 0}),
    ets:insert(?MODULE, {{last_error_report, client}, {0, 0, 0}}),
    ets:insert(?MODULE, {{last_error_report, server}, {0, 0, 0}}),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, bad_request, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
lookup(TransactionKey) ->
    case ets:lookup(?MODULE, TransactionKey) of
	[{_, Pid}] ->
	    {ok, Pid};
	_ ->
	    error
    end.

transaction_key(Branch, <<"CANCEL">>, Type) ->
    {esip:to_lower(Branch), cancel, Type};
transaction_key(Branch, _Method, Type) ->
    {esip:to_lower(Branch), Type}.

pass_to_core(Core, Req, SIPSock) ->
    case Core of
        F when is_function(F) ->
            F(Req, SIPSock, undefined);
        {M, F, A} ->
            apply(M, F, [Req, SIPSock, undefined | A])
    end.

start_server_transaction(SIPSock, Req) ->
    MaxTransactions = esip:get_config_value(max_server_transactions),
    case ets:lookup(?MODULE, {transaction_number, server}) of
        [{_, N}] when N >= MaxTransactions ->
            maybe_report_error(server, N),
            {Status, Reason} = esip:error_status({error, too_many_transactions}),
            esip_transport:send(SIPSock,
                                esip:make_response(
                                  Req, #sip{type = response,
                                            status = Status,
                                            reason = Reason},
                                  esip:make_tag()));
        _ ->
            case esip_server_transaction:start(SIPSock, Req) of
                {ok, _} ->
                    ok;
                Err ->
                    {Status, Reason} = esip:error_status(Err),
                    esip_transport:send(SIPSock,
                                        esip:make_response(
                                          Req, #sip{type = response,
                                                    status = Status,
                                                    reason = Reason},
                                          esip:make_tag()))
            end
    end.

start_client_transaction(SIPSock, Req, TU, Opts) ->
    MaxTransactions = esip:get_config_value(max_client_transactions),
    case ets:lookup(?MODULE, {transaction_number, client}) of
        [{_, N}] when N >= MaxTransactions ->
            maybe_report_error(client, N),
            {error, too_many_transactions};
        _ ->
            esip_client_transaction:start(SIPSock, Req, TU, Opts)
    end.

maybe_report_error(Type, N) ->
    case ets:lookup(?MODULE, {last_error_report, Type}) of
        [{_, Now}] ->
            case timer:now_diff(now(), Now) of
                T when T > 60000000 ->
                    ets:insert(?MODULE, {{last_error_report, Type}, now()}),
                    ?ERROR_MSG("too many ~s transactions: ~p", [Type, N]);
                _ ->
                    ok
            end;
        _ ->
            ok
    end.
