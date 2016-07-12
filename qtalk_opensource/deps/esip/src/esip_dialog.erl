%%%----------------------------------------------------------------------
%%% File    : esip_dialog.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Purpose : 
%%% Created : 29 Dec 2010 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
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

-module(esip_dialog).

-behaviour(gen_server).

%% API
-export([start_link/0, open/4, id/2, close/1, lookup/1, prepare_request/2,
         update_remote_seqnum/2, update_local_seqnum/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("esip.hrl").
-include("esip_lib.hrl").

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

id(Type, #sip{hdrs = Hdrs}) ->
    CallID = esip:to_lower(esip:get_hdr('call-id', Hdrs)),
    {_, _, ToParams} = esip:get_hdr('to', Hdrs),
    {_, _, FromParams} = esip:get_hdr('from', Hdrs),
    ToTag = esip:to_lower(esip:get_param(<<"tag">>, ToParams)),
    FromTag = esip:to_lower(esip:get_param(<<"tag">>, FromParams)),
    case Type of
        uac ->
            #dialog_id{'call-id' = CallID,
                       remote_tag = ToTag,
                       local_tag = FromTag};
        uas ->
            #dialog_id{'call-id' = CallID,
                       remote_tag = FromTag,
                       local_tag = ToTag}
    end.

open(Req, #sip{type = response, hdrs = RespHdrs, status = Status}, uas, TU) ->
    {_, _, ToParams} = esip:get_hdr('to', RespHdrs),
    LocalTag = esip:get_param(<<"tag">>, ToParams),
    open(Req, LocalTag, state(Status), TU);
open(#sip{type = request, uri = URI, hdrs = ReqHdrs},
     #sip{type = response, hdrs = RespHdrs, status = Status}, uac, TU) ->
    case esip:get_hdr('contact', RespHdrs) of
        [{_, RemoteTarget, _}|_] ->
            [#via{transport = Transport}|_] = esip:get_hdr('via', ReqHdrs),
            Secure = (esip_transport:via_transport_to_atom(Transport) == tls)
                and (URI#uri.scheme == <<"sips">>),
            RouteSet = lists:foldl(
                         fun({_, U, _}, Acc) ->
                                 [U|Acc]
                         end, [], esip:get_hdrs('record-route', RespHdrs)),
            LocalSeqNum = esip:get_hdr('cseq', ReqHdrs),
            CallID = esip:get_hdr('call-id', ReqHdrs),
            {_, LocalURI, FromParams} = esip:get_hdr('from', ReqHdrs),
            {_, RemoteURI, ToParams} = esip:get_hdr('to', RespHdrs),
            LocalTag = esip:get_param(<<"tag">>, FromParams),
            RemoteTag = esip:get_param(<<"tag">>, ToParams),
            Dialog = #dialog{secure = Secure,
                             route_set = RouteSet,
                             remote_target = RemoteTarget,
                             local_seq_num = LocalSeqNum,
                             'call-id' = CallID,
                             local_tag = LocalTag,
                             remote_tag = RemoteTag,
                             remote_uri = RemoteURI,
                             local_uri = LocalURI,
                             state = state(Status)},
            DialogID = #dialog_id{'call-id' = esip:to_lower(CallID),
                                  remote_tag = esip:to_lower(RemoteTag),
                                  local_tag = esip:to_lower(LocalTag)},
            case call({open, DialogID, Dialog, TU}) of
                ok ->
                    {ok, DialogID};
                Err ->
                    Err
            end;
        _ ->
            {error, no_contact_header}
    end;
open(#sip{type = request, uri = URI, hdrs = Hdrs}, LocalTag, State, TU) ->
    case esip:get_hdr('contact', Hdrs) of
        [{_, RemoteTarget, _}|_] ->
            [#via{transport = Transport}|_] = esip:get_hdr('via', Hdrs),
            Secure = (esip_transport:via_transport_to_atom(Transport) == tls)
                and (URI#uri.scheme == <<"sips">>),
            RouteSet = [U || {_, U, _} <- esip:get_hdrs('record-route', Hdrs)],
            RemoteSeqNum = esip:get_hdr('cseq', Hdrs),
            CallID = esip:get_hdr('call-id', Hdrs),
            {_, RemoteURI, FromParams} = esip:get_hdr('from', Hdrs),
            {_, LocalURI, _} = esip:get_hdr('to', Hdrs),
            RemoteTag = esip:get_param(<<"tag">>, FromParams),
            Dialog = #dialog{secure = Secure,
                             route_set = RouteSet,
                             remote_target = RemoteTarget,
                             remote_seq_num = RemoteSeqNum,
                             'call-id' = CallID,
                             local_tag = LocalTag,
                             remote_tag = RemoteTag,
                             remote_uri = RemoteURI,
                             local_uri = LocalURI,
                             state = State},
            DialogID = #dialog_id{'call-id' = esip:to_lower(CallID),
                                  remote_tag = esip:to_lower(RemoteTag),
                                  local_tag = esip:to_lower(LocalTag)},
            case call({open, DialogID, Dialog, TU}) of
                ok ->
                    {ok, DialogID};
                Err ->
                    Err
            end;
        _ ->
            {error, no_contact_header}
    end.

prepare_request(DialogID,
                #sip{type = request, method = Method, hdrs = Hdrs} = Req) ->
    case lookup(DialogID) of
        {ok, _TU, #dialog{secure = _Secure,
                          route_set = RouteSet,
                          local_seq_num = LocalSeqNum,
                          remote_target = RemoteTarget,
                          'call-id' = CallID,
                          remote_uri = RemoteURI,
                          local_uri = LocalURI}} ->
            ToParams = if DialogID#dialog_id.remote_tag /= <<>> ->
                               [{<<"tag">>, DialogID#dialog_id.remote_tag}];
                          true ->
                               []
                       end,
            FromParams = if DialogID#dialog_id.local_tag /= <<>> ->
                                 [{<<"tag">>, DialogID#dialog_id.local_tag}];
                            true ->
                                 []
                         end,
            To = {<<>>, RemoteURI, ToParams},
            From = {<<>>, LocalURI, FromParams},
            CSeq = if is_integer(LocalSeqNum) ->
                           if Method /= <<"CANCEL">>, Method /= <<"ACK">> ->
                                   LocalSeqNum + 1;
                              true ->
                                   LocalSeqNum
                           end;
                      true ->
                           esip:make_cseq()
                   end,
            update_local_seqnum(DialogID, CSeq),
            {RequestURI, Routes} =
                case RouteSet of
                    [] ->
                        {RemoteTarget, []};
                    [#uri{params = Params} = URI|URIs] ->
                        case esip:has_param(<<"lr">>, Params) of
                            true ->
                                {RemoteTarget,
                                 [{'route', [{<<>>, U, []}]} || U <- RouteSet]};
                            false ->
                                {URI,
                                 [{'route', [{<<>>, U, []}]} ||
                                     U <- URIs ++ [RemoteTarget]]}
                        end
                end,
            {_, NewHdrs} = esip:split_hdrs(['from', 'to', 'cseq',
                                            'route', 'call-id'], Hdrs),
            Req#sip{uri = RequestURI,
                    hdrs = Routes ++ [{'to', To},
                                      {'from', From},
                                      {'cseq', CSeq},
                                      {'call-id', CallID}|NewHdrs]};
        _ ->
            Req
    end.

update_remote_seqnum(DialogID, CSeq) ->
    gen_server:cast(?MODULE, {update_seqnum, remote, DialogID, CSeq}).

update_local_seqnum(DialogID, CSeq) ->
    gen_server:cast(?MODULE, {update_seqnum, local, DialogID, CSeq}).

close(DialogID) ->
    call({close, DialogID}).

lookup(DialogID) ->
    call({lookup, DialogID}).

call(Msg) ->
    case catch gen_server:call(?MODULE, Msg, 5000) of
        {'EXIT', _} = Err ->
            ?ERROR_MSG("failed to comlete dialog operation:~n"
                       "** Msg: ~p~n"
                       "** Err: ~p",
                       [Msg, Err]),
            {error, internal_server_error};
        Res ->
            Res
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    ets:new(esip_dialog, [named_table, public]),
    {ok, #state{}}.

handle_call({open, DialogID, Dialog, TU}, _From, State) ->
    ets:insert(esip_dialog, {DialogID, TU, Dialog}),
    {reply, ok, State};
handle_call({close, DialogID}, _From, State) ->
    ets:delete(esip_dialog, DialogID),
    {reply, ok, State};
handle_call({lookup, DialogID}, _From, State) ->
    case ets:lookup(esip_dialog, DialogID) of
        [{_, TU, Dialog}] ->
            {reply, {ok, TU, Dialog}, State};
        _ ->
            {reply, {error, enoent}, State}
    end;
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({update_seqnum, Type, DialogID, CSeq}, State) ->
    case ets:lookup(esip_dialog, DialogID) of
        [{_, TU, Dialog}] ->
            NewDialog = case Type of
                            remote ->
                                Dialog#dialog{remote_seq_num = CSeq};
                            local ->
                                Dialog#dialog{local_seq_num = CSeq}
                        end,
            ets:insert(esip_dialog, {DialogID, TU, NewDialog}),
            {noreply, State};
        _ ->
            {noreply, State}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
state(Status) when Status < 200 ->
    early;
state(_) ->
    confirmed.
