%%%----------------------------------------------------------------------
%%% File    : sip.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Purpose : Main SIP instance
%%% Created : 14 Jul 2009 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
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

-module(esip).

-behaviour(gen_server).

%% API
-export([start_link/0, start/0, stop/0]).

-export([add_hdr/3,
	 add_listener/3,
         callback/1,
         callback/2,
         callback/3,
         cancel/1,
         cancel/2,
         check_auth/4,
         close_dialog/1,
	 connect/1,
	 connect/2,
         decode/1,
         decode_uri/1,
         decode_uri_field/1,
	 del_listener/2,
         dialog_id/2,
         encode/1,
         encode_uri/1,
         encode_uri_field/1,
         error_status/1,
         escape/1,
         filter_hdrs/2,
         get_branch/1,
         get_config/0,
         get_config_value/1,
         get_hdr/2,
         get_hdr/3,
         get_hdrs/2,
         get_local_tag/1,
         get_node_by_tag/1,
         get_param/2,
         get_param/3,
         get_so_path/0,
         has_param/2,
         hex_encode/1,
         make_auth/6,
         make_branch/0,
         make_branch/1,
         make_callid/0,
         make_cseq/0,
         make_hdrs/0,
         make_hexstr/1,
         make_response/2,
         make_response/3,
         make_tag/0,
         match/2,
         mod/0,
         open_dialog/4,
         quote/1,
         reason/1,
         reply/2,
         request/2,
         request/3,
         request/4,
         rm_hdr/2,
         send/2,
         set_config_value/2,
         set_hdr/3,
         set_param/3,
         split_hdrs/2,
         stop_transaction/1,
         timer1/0,
         timer2/0,
         timer4/0,
         to_lower/1,
         unescape/1,
         unquote/1,
         warning/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("esip.hrl").
-include("esip_lib.hrl").

-record(state, {node_id}).

-callback request(#sip{}, #sip_socket{}) -> #sip{} | ok | drop.
-callback request(#sip{}, #sip_socket{}, #trid{}) ->
    #sip{} | wait | {module(), atom(), list()} | function() | {error, any()}.
-callback response(#sip{}, #sip_socket{}) -> any().
-callback message_in(ping | #sip{}, #sip_socket{}) -> pang | pong | drop | ok | #sip{}.
-callback message_out(#sip{}, #sip_socket{}) -> #sip{} | ok | drop.
-callback locate(#sip{}) -> #uri{} | #via{} | ok.
-callback data_in(iodata(), #sip_socket{}) -> any().
-callback data_out(iodata(), #sip_socket{}) -> any().

%%====================================================================
%% API
%%====================================================================
start() ->
    application:start(esip).

stop() ->
    application:stop(esip).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_so_path() ->
    EbinDir = filename:dirname(code:which(?MODULE)),
    AppDir = filename:dirname(EbinDir),
    filename:join([AppDir, "priv", "lib"]).

add_listener(Port, Transport, Opts) ->
    esip_listener:add_listener(Port, Transport, Opts).

del_listener(Port, Transport) ->
    esip_listener:del_listener(Port, Transport).

connect(SIPMsg) ->
    connect(SIPMsg, []).

connect(SIPMsg, Opts) ->
    esip_transport:connect(SIPMsg, Opts).

request(SIPSocket, Request) ->
    request(SIPSocket, Request, undefined).

request(SIPSocket, Request, TU) ->
    request(SIPSocket, Request, TU, []).

request(SIPSocket, Request, TU, Opts) ->
    esip_transaction:request(SIPSocket, Request, TU, Opts).

reply(RequestOrTrID, Response) ->
    esip_transaction:reply(RequestOrTrID, Response).

cancel(RequestOrTrID) ->
    cancel(RequestOrTrID, undefined).

cancel(RequestOrTrID, TU) ->
    esip_transaction:cancel(RequestOrTrID, TU).

send(SIPSocket, ReqOrResp) ->
    esip_transport:send(SIPSocket, ReqOrResp).

stop_transaction(TrID) ->
    esip_transaction:stop(TrID).

make_tag() ->
    {NodeID, N} = gen_server:call(?MODULE, make_tag),
    iolist_to_binary([NodeID, $-, int_to_list(N)]).

make_branch() ->
    N = gen_server:call(?MODULE, make_branch),
    iolist_to_binary(["z9hG4bK-", int_to_list(N)]).

make_branch(Hdrs) ->
    case get_hdrs('via', Hdrs) of
        [] ->
            make_branch();
        [Via|_] ->
            TopBranch = to_lower(get_param(<<"branch">>, Via#via.params)),
            Cookie = atom_to_list(erlang:get_cookie()),
            ID = hex_encode(erlang:md5([TopBranch, Cookie])),
            iolist_to_binary(["z9hG4bK-", ID])
    end.

make_callid() ->
    N = gen_server:call(?MODULE, make_callid),
    iolist_to_binary([int_to_list(N)]).

make_cseq() ->
    gen_server:call(?MODULE, make_cseq).

make_hdrs() ->
    Hdrs = [{'cseq', make_cseq()},
            {'max-forwards', get_config_value(max_forwards)},
            {'call-id', make_callid()}],
    case get_config_value(software) of
        undefined ->
            Hdrs;
        Software ->
            [{'user-agent', Software}|Hdrs]
    end.

get_local_tag(TrID) ->
    esip_lib:get_local_tag(TrID).

dialog_id(Type, SIPMsg) ->
    esip_dialog:id(Type, SIPMsg).

open_dialog(Request, ResponseOrTag, TypeOrState, TU) ->
    esip_dialog:open(Request, ResponseOrTag, TypeOrState, TU).

close_dialog(DialogID) ->
    esip_dialog:close(DialogID).

decode(Data) ->
    esip_codec:decode(Data).

decode_uri(Data) ->
    esip_codec:decode_uri(Data).

decode_uri_field(Data) ->
    esip_codec:decode_uri_field(Data).

encode(R) ->
    esip_codec:encode(R).

encode_uri(URI) ->
    esip_codec:encode_uri(URI).

encode_uri_field(URI) ->
    esip_codec:encode_uri_field(URI).

match(Arg1, Arg2) ->
    esip_codec:match(Arg1, Arg2).

rm_hdr(Hdr, Hdrs) ->
    rm_key(Hdr, Hdrs).

add_hdr(Hdr, Val, Hdrs) ->
    add_key(Hdr, Val, Hdrs).

set_hdr(Hdr, Val, Hdrs) ->
    set_key(Hdr, Val, Hdrs).

get_hdr(Hdr, Hdrs) ->
    get_key(Hdr, Hdrs, undefined).

get_hdr(Hdr, Hdrs, Default) ->
    get_key(Hdr, Hdrs, Default).

get_hdrs(Hdr, Hdrs) ->
    get_keys(Hdr, Hdrs).

filter_hdrs(HdrList, Hdrs) ->
    filter_keys(HdrList, Hdrs).

split_hdrs(HdrOrHdrList, Hdrs) ->
    split_keys(HdrOrHdrList, Hdrs).

set_param(Param, Val, Params) ->
    set_key(Param, Val, Params).

get_param(Param, Params) ->
    get_key(Param, Params, <<>>).

get_param(Param, Params, Default) ->
    get_key(Param, Params, Default).

has_param(Param, Params) ->
    has_key(Param, Params).

get_branch(Hdrs) ->
    [Via|_] = get_hdr('via', Hdrs),
    get_param(<<"branch">>, Via#via.params).

escape(Bin) ->
    esip_codec:escape(Bin).

unescape(Bin) ->
    esip_codec:unescape(Bin).

-compile({inline, [{is_equal, 2},
                   {member, 2},
                   {keysearch, 2}]}).

is_equal(K1, K2) when is_binary(K1), is_binary(K2) ->
    esip_codec:strcasecmp(K1, K2);
is_equal(K1, K2) ->
    K1 == K2.

member(X, Xs) ->
    if is_atom(X) ->
            %% lists:member/2 is BIF, so works faster
            lists:member(X, Xs);
       true ->
            member1(X, Xs)
    end.

member1(X, [Y|T]) ->
    case is_equal(X, Y) of
        true ->
            true;
        _ ->
            member1(X, T)
    end;
member1(_X, []) ->
    false.

keysearch(K, Ks) ->
    if is_atom(K) ->
            %% lists:keysearch/3 is a BIF, so works faster
            case lists:keysearch(K, 1, Ks) of
                {value, {_, Val}} ->
                    {ok, Val};
                _ ->
                    error
            end;
       true ->
            keysearch1(K, Ks)
    end.

keysearch1(_K, []) ->
    error;
keysearch1(K, [{K1, V}|T]) ->
    case is_equal(K, K1) of
        true ->
            {ok, V};
        false ->
            keysearch1(K, T)
    end.

rm_key(Key, Keys) ->
    lists:filter(
      fun({K, _}) ->
              not is_equal(Key, K)
      end, Keys).

set_key(Key, Val, Keys) ->
    Res = lists:foldl(
            fun({K, V}, Acc) ->
                    case is_equal(K, Key) of
                        true ->                          
                            Acc;
                        false ->
                            [{K, V}|Acc]
                    end
            end, [], Keys),
    lists:reverse([{Key, Val}|Res]).

get_key(Key, Keys, Default) ->
    case keysearch(Key, Keys) of
        {ok, Val} ->
            Val;
        _ ->
            Default
    end.

has_key(Key, Keys) ->
    case get_key(Key, Keys, undefined) of
        undefined ->
            false;
        _ ->
            true
    end.

add_key(Key, Val, Keys) ->
    case lists:foldl(
           fun({K, V}, {S, Acc}) ->
                   case S == false andalso is_equal(K, Key) of
                       true ->
                           {true, [{K, V}, {Key, Val}|Acc]};
                       false ->
                           {S, [{K, V}|Acc]}
                   end
           end, {false, []}, Keys) of
        {true, Res} ->
            lists:reverse(Res);
        {false, Res} ->
            lists:reverse([{Key, Val}|Res])
    end.

get_keys(Key, Keys) ->
    lists:flatmap(
      fun({K, V}) ->
              case is_equal(K, Key) of
                  true when is_list(V) ->
                      V;
                  true ->
                      [V];
                  false ->
                      []
              end
      end, Keys).

filter_keys(KeyList, Keys) ->
    lists:filter(
      fun({Key, _}) ->
              member(Key, KeyList)
      end, Keys).

split_keys(KeyList, Keys) when is_list(KeyList) ->
    lists:partition(
      fun({Key, _}) ->
              member(Key, KeyList)
      end, Keys);
split_keys(Key, Keys) ->
    lists:foldr(
      fun({K, V}, {H, T}) ->
              case is_equal(K, Key) of
                  true when is_list(V) ->
                      {V++H, T};
                  true ->
                      {[V|H], T};
                  false ->
                      {H, [{K, V}|T]}
              end
      end, {[], []}, Keys).

make_response(Req, Resp) ->
    make_response(Req, Resp, <<>>).

make_response(#sip{hdrs = ReqHdrs,
                   method = Method,
                   type = request},
              #sip{status = Status,
                   hdrs = RespHdrs,
                   method = RespMethod,
                   type = response} = Resp, Tag) ->
    NeedHdrs = if Status == 100 ->
                       filter_hdrs(['via', 'from', 'call-id', 'cseq',
                                    'max-forwards', 'to', 'timestamp'], ReqHdrs);
                  Status > 100, Status < 300 ->
                       filter_hdrs(['via', 'record-route', 'from', 'call-id',
                                    'cseq', 'max-forwards', 'to'], ReqHdrs);
                  true ->
                       filter_hdrs(['via', 'from', 'call-id', 'cseq',
                                    'max-forwards', 'to'], ReqHdrs)
               end,
    NewNeedHdrs =
        if Status > 100 ->
                {ToName, ToURI, ToParams} = get_hdr('to', NeedHdrs),
                case has_param(<<"tag">>, ToParams) of
                    false ->
                        NewTo = {ToName, ToURI, [{<<"tag">>, Tag}|ToParams]},
                        set_hdr('to', NewTo, NeedHdrs);
                    true ->
                        NeedHdrs
                end;
           true ->
                NeedHdrs
        end,
    NewMethod = if RespMethod /= undefined ->
                        RespMethod;
                   true ->
                        Method
                end,
    ResultHdrs = case get_config_value(software) of
                     undefined ->
                         NewNeedHdrs ++ RespHdrs;
                     Software ->
                         NewNeedHdrs ++ [{'server', Software}|RespHdrs]
                 end,
    Resp#sip{method = NewMethod, hdrs = ResultHdrs}.

make_auth({Type, Params}, Method, Body, OrigURI, Username, Password) ->
    Nonce = esip:get_param(<<"nonce">>, Params),
    QOPs = esip:get_param(<<"qop">>, Params),
    Algo = case esip:get_param(<<"algorithm">>, Params) of
               <<>> ->
                   <<"MD5">>;
               Algo1 ->
                   Algo1
           end,
    Realm = esip:get_param(<<"realm">>, Params),
    OpaqueParam = case esip:get_param(<<"opaque">>, Params) of
                      <<>> ->
                          [];
                      Opaque ->
                          [{<<"opaque">>, Opaque}]
                  end,
    CNonce = make_hexstr(20),
    NC = <<"00000001">>, %% TODO
    URI = if is_binary(OrigURI) ->
                  OrigURI;
             is_record(OrigURI, uri) ->
                  iolist_to_binary(esip_codec:encode_uri(OrigURI))
          end,
    QOPList = esip_codec:split(unquote(QOPs), $,),
    QOP = case lists:member(<<"auth">>, QOPList) of
              true ->
                  <<"auth">>;
              false ->
                  case lists:member(<<"auth-int">>, QOPList) of
                      true ->
                          <<"auth-int">>;
                      false ->
                          <<>>
                  end
          end,
    Response = compute_digest(Nonce, CNonce, NC, QOP, Algo,
                              Realm, URI, Method, Body,
                              Username, Password),
    {Type, [{<<"username">>, quote(Username)},
            {<<"realm">>, Realm},
            {<<"nonce">>, Nonce},
            {<<"uri">>, quote(URI)},
            {<<"response">>, quote(Response)},
            {<<"algorithm">>, Algo} |
            if QOP /= <<>> ->
                    [{<<"cnonce">>, quote(CNonce)},
                     {<<"nc">>, NC},
                     {<<"qop">>, QOP}];
               true ->
                    []
            end] ++ OpaqueParam}.

check_auth({Type, Params}, Method, Body, Password) ->
    case to_lower(Type) of
        <<"digest">> ->
            NewMethod = case Method of
                            <<"ACK">> -> <<"INVITE">>;
                            _ -> Method
                        end,
            Nonce = esip:get_param(<<"nonce">>, Params),
            NC = esip:get_param(<<"nc">>, Params),
            CNonce = esip:get_param(<<"cnonce">>, Params),
            QOP = esip:get_param(<<"qop">>, Params),
            Algo = esip:get_param(<<"algorithm">>, Params),
            Username = esip:get_param(<<"username">>, Params),
            Realm = esip:get_param(<<"realm">>, Params),
            URI = esip:get_param(<<"uri">>, Params),
            Response = unquote(esip:get_param(<<"response">>, Params)),
            Response == compute_digest(Nonce, CNonce, NC, QOP,
                                       Algo, Realm, URI, NewMethod, Body,
                                       Username, Password);
        _ ->
            false
    end.

make_hexstr(N) ->
    hex_encode(crypto:rand_bytes(N)).

hex_encode(Data) ->
    << <<(esip_codec:to_hex(X))/binary>> || <<X>> <= Data >>.

to_lower(Bin) ->
    esip_codec:to_lower(Bin).

get_config() ->
    ets:tab2list(esip_config).

get_config_value(Key) ->
    case ets:lookup(esip_config, Key) of
        [{_, Val}] ->
            Val;
        _ ->
            undefined
    end.

mod() ->
    get_config_value(module).

set_config_value(Key, Val) ->
    ets:insert(esip_config, {Key, Val}).

callback({M, F, A}) ->
    callback(M, F, A).

callback(F, Args) when is_function(F) ->
    case catch apply(F, Args) of
        {'EXIT', _} = Err ->
            ?ERROR_MSG("failed to process callback:~n"
                       "** Function: ~p~n"
                       "** Args: ~p~n"
                       "** Reason: ~p",
                       [F, Args, Err]),
            {error, internal_server_error};
        Result ->
            Result
    end;
callback(F, Args) ->
    callback(get_config_value(module), F, Args).

callback(Mod, Fun, Args) ->
    case catch apply(Mod, Fun, Args) of
        {'EXIT', _} = Err ->
            ?ERROR_MSG("failed to process callback:~n"
                       "** Function: ~p:~p/~p~n"
                       "** Args: ~p~n"
                       "** Reason: ~p",
                       [Mod, Fun, length(Args), Args, Err]),
            {error, internal_server_error};
        Result ->
            Result
    end.

%% Basic Timers
timer1() -> get_config_value(timer1).
timer2() -> get_config_value(timer2).
timer4() -> get_config_value(timer4).

error_status({error, Err}) ->
    error_status(Err);
error_status(timeout) ->
    {408, reason(408)};
error_status(no_contact_header) ->
    {400, <<"Missed Contact header">>};
error_status(too_many_transactions) ->
    {500, <<"Too Many Transactions">>};
error_status(unsupported_uri_scheme) ->
    {416, reason(416)};
error_status(unsupported_transport) ->
    {503, "Unsupported Transport"};
error_status(Err) when is_atom(Err) ->
    case inet:format_error(Err) of
        "unknown POSIX error" ->
            {500, reason(500)};
        [H|T] when H >= $a, H =< $z ->
            {503, list_to_binary([H - $ |T])};
        Txt ->
            {503, Txt}
    end;
error_status(_) ->
    {500, reason(500)}.

get_node_by_tag(Tag) ->
    case esip_codec:split(Tag, $-, 1) of
        [NodeID, _] ->
            get_node_by_id(NodeID);
        _ ->
            node()
    end.

get_node_by_id(NodeID) when is_binary(NodeID) ->
    case catch erlang:binary_to_existing_atom(NodeID, utf8) of
        {'EXIT', _} ->
            node();
        Res ->
            get_node_by_id(Res)
    end;
get_node_by_id(NodeID) ->
    case global:whereis_name(NodeID) of
        Pid when is_pid(Pid) ->
            node(Pid);
        _ ->
            node()
    end.

%% From http://www.iana.org/assignments/sip-parameters
reason(100) -> <<"Trying">>;
reason(180) -> <<"Ringing">>;
reason(181) -> <<"Call Is Being Forwarded">>;
reason(182) -> <<"Queued">>;
reason(183) -> <<"Session Progress">>;
reason(200) -> <<"OK">>;
reason(202) -> <<"Accepted">>;
reason(204) -> <<"No Notification">>;
reason(300) -> <<"Multiple Choices">>;
reason(301) -> <<"Moved Permanently">>;
reason(302) -> <<"Moved Temporarily">>;
reason(305) -> <<"Use Proxy">>;
reason(380) -> <<"Alternative Service">>;
reason(400) -> <<"Bad Request">>;
reason(401) -> <<"Unauthorized">>;
reason(402) -> <<"Payment Required">>;
reason(403) -> <<"Forbidden">>;
reason(404) -> <<"Not Found">>;
reason(405) -> <<"Method Not Allowed">>;
reason(406) -> <<"Not Acceptable">>;
reason(407) -> <<"Proxy Authentication Required">>;
reason(408) -> <<"Request Timeout">>;
reason(410) -> <<"Gone">>;
reason(412) -> <<"Conditional Request Failed">>;
reason(413) -> <<"Request Entity Too Large">>;
reason(414) -> <<"Request-URI Too Long">>;
reason(415) -> <<"Unsupported Media Type">>;
reason(416) -> <<"Unsupported URI Scheme">>;
reason(417) -> <<"Unknown Resource-Priority">>;
reason(420) -> <<"Bad Extension">>;
reason(421) -> <<"Extension Required">>;
reason(422) -> <<"Session Interval Too Small">>;
reason(423) -> <<"Interval Too Brief">>;
reason(428) -> <<"Use Identity Header">>;
reason(429) -> <<"Provide Referrer Identity">>;
reason(430) -> <<"Flow Failed">>;
reason(433) -> <<"Anonymity Disallowed">>;
reason(436) -> <<"Bad Identity-Info">>;
reason(437) -> <<"Unsupported Certificate">>;
reason(438) -> <<"Invalid Identity Header">>;
reason(439) -> <<"First Hop Lacks Outbound Support">>;
reason(440) -> <<"Max-Breadth Exceeded">>;
reason(469) -> <<"Bad Info Package">>;
reason(470) -> <<"Consent Needed">>;
reason(480) -> <<"Temporarily Unavailable">>;
reason(481) -> <<"Call/Transaction Does Not Exist">>;
reason(482) -> <<"Loop Detected">>;
reason(483) -> <<"Too Many Hops">>;
reason(484) -> <<"Address Incomplete">>;
reason(485) -> <<"Ambiguous">>;
reason(486) -> <<"Busy Here">>;
reason(487) -> <<"Request Terminated">>;
reason(488) -> <<"Not Acceptable Here">>;
reason(489) -> <<"Bad Event">>;
reason(491) -> <<"Request Pending">>;
reason(493) -> <<"Undecipherable">>;
reason(494) -> <<"Security Agreement Required">>;
reason(500) -> <<"Server Internal Error">>;
reason(501) -> <<"Not Implemented">>;
reason(502) -> <<"Bad Gateway">>;
reason(503) -> <<"Service Unavailable">>;
reason(504) -> <<"Server Time-out">>;
reason(505) -> <<"Version Not Supported">>;
reason(513) -> <<"Message Too Large">>;
reason(580) -> <<"Precondition Failure">>;
reason(600) -> <<"Busy Everywhere">>;
reason(603) -> <<"Decline">>;
reason(604) -> <<"Does Not Exist Anywhere">>;
reason(606) -> <<"Not Acceptable">>;
reason(Status) when Status > 100, Status < 200 ->
    <<"Session Progress">>;
reason(Status) when Status > 200, Status < 300 ->
    <<"Accepted">>;
reason(Status) when Status > 300, Status < 400 ->
    <<"Multiple Choices">>;
reason(Status) when Status > 400, Status < 500 ->
    <<"Bad Request">>;
reason(Status) when Status > 500, Status < 600 ->
    <<"Server Internal Error">>;
reason(Status) when Status > 600, Status < 700 ->
    <<"Busy Everywhere">>.

warning(300) -> <<"\"Incompatible network protocol\"">>;
warning(301) -> <<"\"Incompatible network address formats\"">>;
warning(302) -> <<"\"Incompatible transport protocol\"">>;
warning(303) -> <<"\"Incompatible bandwidth units\"">>;
warning(304) -> <<"\"Media type not available\"">>;
warning(305) -> <<"\"Incompatible media format\"">>;
warning(306) -> <<"\"Attribute not understood\"">>;
warning(307) -> <<"\"Session description parameter not understood\"">>;
warning(330) -> <<"\"Multicast not available\"">>;
warning(331) -> <<"\"Unicast not available\"">>;
warning(370) -> <<"\"Insufficient bandwidth\"">>;
warning(380) -> <<"\"SIPS Not Allowed\"">>;
warning(381) -> <<"\"SIPS Required\"">>;
warning(399) -> <<"\"Miscellaneous warning\"">>;
warning(Code) when Code > 300, Code < 400 -> <<"\"\"">>.

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    {A, B, C} = now(),
    random:seed(A, B, C),
    ets:new(esip_config, [named_table, public]),
    set_config([]),
    NodeID = list_to_binary(integer_to_list(random:uniform(1 bsl 32))),
    register_node(NodeID),
    {ok, #state{node_id = NodeID}}.

handle_call(make_tag, _From, State) ->
    {reply, {State#state.node_id, random:uniform(1 bsl 32)}, State};
handle_call(make_branch, _From, State) ->
    {reply, random:uniform(1 bsl 48), State};
handle_call(make_callid, _From, State) ->
    {reply, random:uniform(1 bsl 48), State};
handle_call(make_cseq, _From, State) ->
    {reply, random:uniform(1 bsl 10), State};
handle_call(stop, _From, State) ->
    {stop, normal, State};
handle_call(_Request, _From, State) ->
    {reply, bad_request, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    ?ERROR_MSG("got unexpected info: ~p", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
default_config() ->
    Software = case catch application:get_key(esip, vsn) of
                   {ok, [_|_] = Ver} ->
                       list_to_binary(["esip/", Ver]);
                   _ ->
                       <<"esip">>
               end,
    [{max_forwards, 70},
     {timer1, 500},
     {timer2, 4000},
     {timer4, 5000},
     {software, Software},
     {max_msg_size, 128*1024}].

set_config(Opts) ->
    lists:foreach(
      fun({Key, Value}) ->
              ets:insert(esip_config, {Key, Value});
         (_) ->
              ok
      end, default_config() ++ Opts).

int_to_list(N) ->
    erlang:integer_to_list(N).

register_node(NodeID) ->
    global:register_name(erlang:binary_to_atom(NodeID, utf8), self()).

unquote(<<$", Rest/binary>>) ->
    case size(Rest) - 1 of
        Size when Size > 0 ->
            <<Result:Size/binary, _>> = Rest,
            Result;
        _ ->
            <<>>
    end;
unquote(Val) ->
    Val.

quote(Val) ->
    <<$", Val/binary, $">>.

md5_digest(Data) ->
    hex_encode(erlang:md5(Data)).

compute_digest(Nonce, CNonce, NC, QOP, Algo, Realm,
               URI, Method, Body, Username, Password) ->
    AlgoL = to_lower(Algo),
    QOPL = to_lower(QOP),
    A1 = if AlgoL == <<"md5">>; AlgoL == <<>> ->
                 [unquote(Username), $:, unquote(Realm), $:, Password];
            true ->
                 [md5_digest([unquote(Username), $:,
                              unquote(Realm), $:, Password]),
                  $:, unquote(Nonce), $:, unquote(CNonce)]
         end,
    A2 = if QOPL == <<"auth">>; QOPL == <<>> ->
                 [Method, $:, unquote(URI)];
            true ->
                 [Method, $:, unquote(URI), $:, md5_digest(Body)]
         end,
    if QOPL == <<"auth">>; QOPL == <<"auth-int">> ->
            md5_digest(
              [md5_digest(A1),
               $:, unquote(Nonce),
               $:, NC,
               $:, unquote(CNonce),
               $:, unquote(QOP),
               $:, md5_digest(A2)]);
       true ->
            md5_digest(
              [md5_digest(A1),
               $:, unquote(Nonce),
               $:, md5_digest(A2)])
    end.
