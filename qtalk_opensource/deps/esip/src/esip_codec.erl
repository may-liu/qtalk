%%%----------------------------------------------------------------------
%%% File    : esip_codec.erl
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

-module(esip_codec).

%% API
-export([start/0,
         decode/1,
         decode/2,
         decode_uri_field/1,
         decode_uri/1,
         split/1,
         split/2,
         split/3,
         encode_uri_field/1,
         encode_uri/1,
         encode/1,
         to_lower/1,
         to_integer/3,
         str/2,
         escape/1,
         unescape/1,
	 match/2,
         strcasecmp/2,
         to_hex/1]).

%% Test
-export([test/0, test_loop/0, test_loop/3, test_loop/4]).

-include("esip.hrl").
-include("esip_lib.hrl").

%%%===================================================================
%%% API
%%%===================================================================
-ifdef(USE_NIF).
start() ->
    case catch erlang:load_nif(atom_to_list(?MODULE), 0) of
        ok ->
            ok;
        _Err ->
            SOPath = filename:join(esip:get_so_path(), atom_to_list(?MODULE)),
            case catch erlang:load_nif(SOPath, 0) of
                ok ->
                    ok;
                Err ->
                    ?ERROR_MSG("unable to load ~s NIF: ~p", [?MODULE, Err])
            end
    end.
-else.
start() ->
    ok.
-endif.

decode(Data) ->
    decode(Data, datagram).

decode(Data, stream) ->
    case str(Data, <<"\r\n\r\n">>) of
        nomatch ->
            more;
        _ ->
            do_decode(Data, stream)
    end;
decode(Data, datagram) ->
    do_decode(Data, datagram).

do_decode(Data, Type) ->
    case str(Data, <<"\r\n">>) of
        nomatch ->
            more;
        Pos ->
            <<Head:Pos/binary, _:2/binary, Tail/binary>> = Data,
            case decode_start_line(Head) of
                {error, _Reason} = Err ->
                    Err;
                StartLine ->
                    do_decode(Tail, StartLine, Type)
            end
    end.

do_decode(Tail, StartLine, Type) ->
    Method = case StartLine of
                 {response, _, _, _} ->
                     <<>>;
                 {request, _, M, _} ->
                     M
             end,
    case decode_hdrs(Tail, {Method, []}) of
        {ok, CSeqMethod, Hdrs, Body} ->
            Msg = case StartLine of
                      {response, Ver, Status, Reason} ->
                          #sip{type = response,
                               method = CSeqMethod,
                               version = Ver,
                               status = Status,
                               reason = Reason,
                               hdrs = Hdrs};
                      {request, Ver, Method, URI} ->
                          #sip{type = request,
                               version = Ver,
                               method = Method,
                               uri = URI,
                               hdrs = Hdrs}
                  end,
            case Type of
                datagram ->
                    {ok, Msg#sip{body = Body}};
                _ ->
                    {ok, Msg, Body}
            end;
        Err ->
            Err
    end.

decode_uri(Data) ->
    decode_uri(Data, #uri{}).

decode_uri_field(Data) ->
    lists:reverse(decode_uri_field(Data, [])).

decode_via(ViaData) ->
    lists:flatmap(
      fun(<<>>) ->
              [];
         (Data) ->
              case split(Data, $/, 3) of
                  [Proto, <<Maj, $., Min>>, Tail] ->
                      case split(Tail, wsp, 1) of
                          [Transport, Tail1] ->
                              case decode_uri_host(Tail1, #uri{}) of
                                  #uri{host = Host, port = Port, params = Params} ->
                                      [#via{proto = Proto,
                                            version = {Maj-48, Min-48},
                                            transport = to_upper(Transport),
                                            host = Host,
                                            port = Port,
                                            params = Params}];
                                  error ->
                                      []
                              end;
                          _ ->
                              []
                      end;
                  _ ->
                      []
              end
      end, split(ViaData, $,)).

decode_uri_field(Data, Acc) ->
    case split(strip_wsp(Data), $<, 1) of
        [Name, Tail1] ->
            case split(Tail1, $>, 1) of
                [Head, Tail2] ->
                    case split(Tail2, $,, 1) of
                        [Params, Rest] ->
                            FParams = decode_params(Params),
                            case decode_uri(Head) of
                                error ->
                                    error;
                                U ->
                                    decode_uri_field(
                                      Rest, [{Name, U, FParams}|Acc])
                            end;
                        [Params] ->
                            FParams = decode_params(Params),
                            case decode_uri(Head) of
                                error ->
                                    error;
                                U ->
                                    [{Name, U, FParams}|Acc]
                            end
                    end;
                [Head] ->
                    case decode_uri(Head) of
                        error ->
                            error;
                        U ->
                            [{Name, U, []}|Acc]
                    end
            end;
        [URI] ->
            case split(URI, $;, 1) of
                [_] ->
                    case decode_uri(URI) of
                        error ->
                            error;
                        U ->
                            [{<<>>, U, []}|Acc]
                    end;
                [Head, Params] ->
                    case decode_uri(Head) of
                        error ->
                            error;
                        U ->
                            [{<<>>, U, decode_params(Params)}|Acc]
                    end
            end;
        _ ->
            error
    end.

decode_uri(Data, URI) ->
    case split(Data, $:, 1) of
        [Scheme, UserHost] ->
            case split(UserHost, $@, 1) of
                [UserPass, HostPort] ->
                    case split(UserPass, $:, 1) of
                        [User, Password] ->
                            decode_uri_host(HostPort,
                                            URI#uri{scheme = Scheme,
                                                    user = User,
                                                    password = Password});
                        _ ->
                            decode_uri_host(HostPort,
                                            URI#uri{scheme = Scheme,
                                                    user = UserPass})
                    end;
                [HostPort] ->
                    decode_uri_host(HostPort, URI#uri{scheme = Scheme})
            end;
        _ ->
            error
    end.

decode_uri_host(Data, URI) ->
    case split(Data, $;, 1) of
        [_] ->
            case split(Data, $?, 1) of
                [HostPort, Tail] ->
                    Headers = decode_param_hdrs(Tail),
                    Params = [];
                [HostPort] ->
                    Headers = Params = []
            end;
        [HostPort, Tail] ->
            case split(Tail, $?, 1) of
                [Ps, Hs] ->
                    Headers = decode_param_hdrs(Hs),
                    Params = decode_params(Ps);
                [Ps] ->
                    Headers = [],
                    Params = decode_params(Ps)
            end
    end,
    case split(HostPort, $:, 1) of
        [Host, Port] ->
            case to_integer(Port, 0, 65535) of
                {ok, PortInt} ->
                    URI#uri{host = to_lower(Host), port = PortInt,
                            params = Params, hdrs = Headers};
                _ ->
                    error
            end;
        [Host] ->
            URI#uri{host = to_lower(Host), params = Params, hdrs = Headers}
    end.

encode_uri(#uri{scheme = Scheme,
                user = User,
                password = Password,
                host = Host,
                port = Port,
                params = Params,
                hdrs = Hdrs}) ->
    EncParams = encode_params(Params),
    EncHdrs = case Hdrs of
                  [_|_] ->
                      [$?, join_params(Hdrs, "&")];
                  _ ->
                      ""
              end,
    HostPort = if Port >= 0, Port < 65536 ->
                       [Host, $:, integer_to_list(Port)];
                  true ->
                       Host
               end,
    UserPassHostPort = if User /= <<>>, Password /= <<>> ->
                               [User, $:, Password, $@, HostPort];
                          User /= <<>> ->
                               [User, $@, HostPort];
                          true ->
                               HostPort
                       end,
    list_to_binary([Scheme, $:, UserPassHostPort, EncParams, EncHdrs]).

encode_uri_field({Name, URI, FieldParams}) ->
    NewName = if Name /= <<>> ->
                      [Name, $ ];
                 true ->
                      <<>>
              end,
    [NewName, $<, encode_uri(URI), $>, encode_params(FieldParams)].

encode(#sip{type = Type,
            version = {Maj, Min},
            method = Method,
            hdrs = Hdrs,
            body = Body,
            uri = URI,
            status = Status,
            reason = Reason}) ->
    StartLine = if Type == request ->
                        [Method, $ , encode_uri(URI), $ ,
                         "SIP/", Maj+48, $., Min+48];
                   Type == response ->
                        Reason1 = if Reason == undefined; Reason == <<>> ->
                                          esip:reason(Status);
                                     true ->
                                          Reason
                                  end,
                        ["SIP/", Maj+48, $., Min+48, $ ,
                         integer_to_list(Status), $ , Reason1]
                end,
    Size = erlang:iolist_size(Body),
    NewHdrs = esip:set_hdr('content-length', Size, Hdrs),
    [StartLine, "\r\n", encode_hdrs(NewHdrs, Method), "\r\n", Body].

encode_via(#via{proto = Proto,
                version = {Maj,Min},
                transport = Transport,
                host = Host,
                port = Port,
                params = Params}) ->
    EncParams = encode_params(Params),
    HostPort = if Port >= 0, Port < 65536 ->
                       [Host, $:, integer_to_list(Port)];
                  true ->
                       Host
               end,
    [Proto, $/, Maj+48, $., Min+48, $/, Transport, $ , HostPort, EncParams].

escape(Bin) ->
    list_to_binary(
      [case S of
           $; -> escape_chr(S);
           $/ -> escape_chr(S);
           $? -> escape_chr(S);
           $: -> escape_chr(S);
           $@ -> escape_chr(S);
           $& -> escape_chr(S);
           $= -> escape_chr(S);
           $+ -> escape_chr(S);
           $$ -> escape_chr(S);
           $, -> escape_chr(S);
           $< -> escape_chr(S);
           $> -> escape_chr(S);
           $# -> escape_chr(S);
           $% -> escape_chr(S);
           $" -> escape_chr(S);
           ${ -> escape_chr(S);
           $} -> escape_chr(S);
           $| -> escape_chr(S);
           $\\ -> escape_chr(S);
           $^ -> escape_chr(S);
           $[ -> escape_chr(S);
           $] -> escape_chr(S);
           $` -> escape_chr(S);
           _ when S > 16#20, S < 16#7f ->
               S;
           _ ->
               escape_chr(S)
       end || S <- binary_to_list(Bin)]).

unescape(Bin) ->
    list_to_binary(unescape1(binary_to_list(Bin))).

unescape1([$%, A, B|T]) ->
    case catch erlang:list_to_integer([A,B], 16) of
        {'EXIT', _} ->
            [$%|unescape1([A,B|T])];
        Int ->
            [Int|unescape1(T)]
    end;
unescape1([C|T]) ->
    [C|unescape1(T)];
unescape1([]) ->
    [].

to_hex(X) ->
    Hi = case X div 16 of
             N1 when N1 < 10 -> $0 + N1;
             N1 -> $W + N1
         end,
    Lo = case X rem 16 of
             N2 when N2 < 10 -> $0 + N2;
             N2 -> $W + N2
         end,
    <<Hi, Lo>>.

%% TODO: implement URI comparison according to the RFC
match(U1, U2) ->
    U1 == U2.

%%%===================================================================
%%% Internal functions
%%%===================================================================
decode_start_line(Data) ->
    case split(Data, wsp, 1) of
        [<<"SIP/", Maj, ".", Min>>, Rest] ->
            case split(Rest, wsp, 1) of
                [Code, Reason] ->
                    case to_integer(Code, 100, 699) of
                        {ok, N} ->
                            {response, {Maj-48, Min-48}, N, Reason};
                        _ ->
                            {error, bad_start_line_status}
                    end;
                _ ->
                    {error, bad_start_line}
            end;
        [Method, Rest] ->
            case split(Rest, wsp, 1) of
                [Head, <<"SIP/", Maj, ".", Min>>] ->
                    case decode_uri(Head) of
                        error ->
                            {error, bad_start_line_uri};
                        URI ->
                            {request, {Maj-48, Min-48}, Method, URI}
                    end;
                _ ->
                    {error, bad_start_line}
            end;
        _ ->
            {error, bad_start_line}
    end.

decode_hdrs(Data, {Method, Acc}) ->
    case erlang:decode_packet(httph_bin, Data, []) of
        {ok, {http_header, _, Name, _, Value}, Rest} ->
            LName = if is_atom(Name) ->
                            Name;
                       true ->
                            to_lower(Name)
                    end,
            case catch decode_hdr(LName, Value) of
                {'EXIT', _} ->
                    HdrName = if is_atom(Name) ->
                                      atom_to_binary1(Name);
                                 true ->
                                      Name
                              end,
                    decode_hdrs(Rest, {Method, [{HdrName, Value}|Acc]});
                {cseq, CSeq, M} ->
                    decode_hdrs(Rest, {M, [{'cseq', CSeq}|Acc]});
                Result ->
                    decode_hdrs(Rest, {Method, [Result|Acc]})
            end;
        {ok, http_eoh, Body} ->
            {ok, Method, prepare_hdrs(Acc), Body};
        {more, _} ->
            {more, Data, {Method, Acc}};
        _ ->
            {error, bad_header}
    end.

decode_hdr('Accept', Val) ->
    {'accept', decode_params_list(Val)};
decode_hdr(<<"accept-contact">>, Val) ->
    {'accept-contact', decode_params_list(Val)};
decode_hdr('Accept-Encoding', Val) ->
    {'accept-encoding', decode_params_list(Val)};
decode_hdr('Accept-Language', Val) ->
    {'accept-language', decode_params_list(Val)};
decode_hdr(<<"accept-resource-priority">>, Val) ->
    {'accept-resource-priority', split(Val, $,)};
decode_hdr(<<"alert-info">>, Val) ->
    %%TODO
    {'alert-info', Val};
decode_hdr('Allow', Val) ->
    {'allow', split(Val, $,)};
decode_hdr(<<"allow-events">>, Val) ->
    {'allow-events', decode_params_list(Val)};
decode_hdr(<<"answer-mode">>, Val) ->
    {'answer-mode', decode_type_params(Val)};
decode_hdr(<<"authentication-info">>, Val) ->
    {'authentication-info', decode_params(Val, $,)};
decode_hdr('Authorization', Val) ->
    {'authorization', decode_auth(Val)};
decode_hdr(<<"call-id">>, Val) ->
    {'call-id', Val};
decode_hdr(<<"call-info">>, Val) ->
    %%TODO
    {'call-info', Val};
decode_hdr(<<"contact">>, <<"*">>) ->
    {'contact', <<"*">>};
decode_hdr(<<"contact">>, Val) ->
    {'contact', [_|_] = decode_uri_field(Val)};
decode_hdr(<<"content-disposition">>, Val) ->
    {'content-disposition', decode_type_params(Val)};
decode_hdr('Content-Encoding', Val) ->
    {'content-encoding', split(Val, $,)};
decode_hdr('Content-Language', Val) ->
    {'content-language', split(Val, $,)};
decode_hdr('Content-Length', Val) ->
    {ok, N} = to_integer(Val, 0, unlimited),
    {'content-length', N};
decode_hdr('Content-Type', Val) ->
    {'content-type', decode_type_params(Val)};
decode_hdr(<<"cseq">>, Val) ->
    [CSeq, Method] = split(Val, wsp, 1),
    {ok, N} = to_integer(CSeq, 0, unlimited),
    {'cseq', N, Method};
decode_hdr('Date', Val) ->
    {_, _} = Date = httpd_util:convert_request_date(binary_to_list(Val)),
    {'date', Date};
decode_hdr(<<"error-info">>, Val) ->
    %%TODO
    {'error-info', Val};
decode_hdr(<<"event">>, Val) ->
    {'event', decode_type_params(Val)};
decode_hdr('Expires', Val) ->
    {ok, N} = to_integer(Val, 0, unlimited),
    {'expires', N};
decode_hdr(<<"flow-timer">>, Val) ->
    {ok, N} = to_integer(Val, 0, unlimited),
    {'flow-timer', N};
decode_hdr('From', Val) ->
    [URI|_] = decode_uri_field(Val),
    {'from', URI};
decode_hdr(<<"history-info">>, Val) ->
    {'history-info', [_|_] = decode_uri_field(Val)};
decode_hdr(<<"identity">>, Val) ->
    {'identity', Val};
decode_hdr(<<"identity-info">>, Val) ->
    %%TODO
    {'identity-info', Val};
decode_hdr(<<"info-package">>, Val) ->
    {'info-package', decode_params_list(Val)};
decode_hdr(<<"in-reply-to">>, Val) ->
    {'in-reply-to', split(Val, $,)};
decode_hdr(<<"join">>, Val) ->
    {'join', decode_params_list(Val)};
decode_hdr(<<"max-breadth">>, Val) ->
    {ok, N} = to_integer(Val, 0, unlimited),
    {'max-breadth', N};
decode_hdr('Max-Forwards', Val) ->
    {ok, N} = to_integer(Val, 0, unlimited),
    {'max-forwards', N};
decode_hdr(<<"mime-version">>, Val) ->
    <<Maj, $., Min>> = Val,
    {'mime-version', {Maj-48, Min-48}};
decode_hdr(<<"min-expires">>, Val) ->
    {ok, N} = to_integer(Val, 0, unlimited),
    {'min-expires', N};
decode_hdr(<<"min-se">>, Val) ->
    {Delta, Params} = decode_type_params(Val),
    {ok, N} = to_integer(Delta, 0, unlimited),
    {'min-se', {N, Params}};
decode_hdr(<<"organization">>, Val) ->
    {'organization', Val};
%% decode_hdr(<<"p-access-network-info">>, Val) ->
%%     {'p-access-network-info', decode_type_params(Val)};
%% decode_hdr(<<"p-answer-state">>, Val) ->
%%     {'p-answer-state', decode_type_params(Val)};
%% decode_hdr(<<"p-asserted-identity">>, Val) ->
%%     {'p-asserted-identity', [_|_] = decode_uri_field(Val)};
%% decode_hdr(<<"p-asserted-service">>, Val) ->
%%     {'p-asserted-service', split(Val, $,)};
%% decode_hdr(<<"p-associated-uri">>, Val) ->
%%     {'p-associated-uri', [_|_] = decode_uri_field(Val)};
%% decode_hdr(<<"p-called-party-id">>, Val) ->
%%     [URI|_] = decode_uri_field(Val),
%%     {'p-called-party-id', URI};
%% decode_hdr(<<"p-charging-function-addresses">>, Val) ->
%%     {'p-charging-function-addresses', decode_params(Val)};
%% decode_hdr(<<"p-charging-vector">>, Val) ->
%%     {'p-charging-vector', decode_params(Val)};
%% decode_hdr(<<"p-dcs-billing-info">>, Val) ->
%%     {'p-dcs-billing-info', decode_type_params(Val)};
%% decode_hdr(<<"p-dcs-laes">>, Val) ->
%%     {'p-dcs-laes', decode_type_params(Val)};
%% decode_hdr(<<"p-dcs-osps">>, Val) ->
%%     {'p-dcs-osps', Val};
%% decode_hdr(<<"p-dcs-redirect">>, Val) ->
%%     [URI|_] = decode_uri_field(Val),
%%     {'p-dcs-redirect', URI};
%% decode_hdr(<<"p-dcs-trace-party-id">>, Val) ->
%%     [URI|_] = decode_uri_field(Val),
%%     {'p-dcs-trace-party-id', URI};
%% decode_hdr(<<"p-early-media">>, Val) ->
%%     {'p-early-media', split(Val, $,)};
%% decode_hdr(<<"p-media-authorization">>, Val) ->
%%     {'p-media-authorization', split(Val, $,)};
%% decode_hdr(<<"p-preferred-identity">>, Val) ->
%%     {'p-preferred-identity', [_|_] = decode_uri_field(Val)};
%% decode_hdr(<<"p-preferred-service">>, Val) ->
%%     {'p-preferred-service', split(Val, $,)};
%% decode_hdr(<<"p-profile-key">>, Val) ->
%%     [URI|_] = decode_uri_field(Val),
%%     {'p-profile-key', URI};
%% decode_hdr(<<"p-refused-uri-list">>, Val) ->
%%     {'p-refused-uri-list', [_|_] = decode_uri_field(Val)};
%% decode_hdr(<<"p-served-user">>, Val) ->
%%     [URI|_] = decode_uri_field(Val),
%%     {'p-served-user', URI};
%% decode_hdr(<<"p-user-database">>, Val) ->
%%     %% TODO
%%     {'p-user-database', Val};
%% decode_hdr(<<"p-visited-network-id">>, Val) ->
%%     {'p-visited-network-id', split(Val, $,)};
decode_hdr(<<"path">>, Val) ->
    {'path', [_|_] = decode_uri_field(Val)};
decode_hdr(<<"permission-missing">>, Val) ->
    {'permission-missing', [_|_] = decode_uri_field(Val)};
decode_hdr(<<"priority">>, Val) ->
    {'priority', Val};
decode_hdr(<<"privacy">>, Val) ->
    {'privacy', split(Val, $;)};
decode_hdr(<<"priv-answer-mode">>, Val) ->
    {'priv-answer-mode', split(Val, $,)};
decode_hdr('Proxy-Authenticate', Val) ->
    {'proxy-authenticate', decode_auth(Val)};
decode_hdr('Proxy-Authorization', Val) ->
    {'proxy-authorization', decode_auth(Val)};
decode_hdr(<<"proxy-require">>, Val) ->
    {'proxy-require', split(Val, $,)};
decode_hdr(<<"rack">>, Val) ->
    [Num, CSeq, Method] = split(Val),
    {ok, NumInt} = to_integer(Num, 0, unlimited),
    {ok, CSeqInt} = to_integer(CSeq, 0, unlimited),
    {'rack', {NumInt, CSeqInt, Method}};
decode_hdr(<<"reason">>, Val) ->
    {'reason', decode_params_list(Val)};
decode_hdr(<<"record-route">>, Val) ->
    {'record-route', [_|_] = decode_uri_field(Val)};
decode_hdr(<<"recv-info">>, Val) ->
    {'recv-info', decode_params_list(Val)};
decode_hdr(<<"refer-sub">>, Val) ->
    {Type, Params} = decode_type_params(Val),
    case Type of
        <<"false">> -> {'refer-sub', {false, Params}};
        <<"true">> -> {'refer-sub', {true, Params}}
    end;
decode_hdr(<<"refer-to">>, Val) ->
    [URI] = decode_uri_field(Val),
    {'refer-to', URI};
decode_hdr(<<"referred-by">>, Val) ->
    [URI] = decode_uri_field(Val),
    {'referred-by', URI};
decode_hdr(<<"reject-contact">>, Val) ->
    {'reject-contact', decode_params_list(Val)};
decode_hdr(<<"replaces">>, Val) ->
    {'replaces', decode_type_params(Val)};
decode_hdr(<<"reply-to">>, Val) ->
    [URI] = decode_uri_field(Val),
    {'reply-to', URI};
decode_hdr(<<"request-disposition">>, Val) ->
    {'request-disposition', split(Val, $,)};
decode_hdr(<<"require">>, Val) ->
    {'require', split(Val, $,)};
decode_hdr(<<"resource-priority">>, Val) ->
    {'resource-priority', split(Val, $,)};
decode_hdr('Retry-After', Val) ->
    {Delta, Params} = decode_type_params(Val),
    {ok, N} = to_integer(Delta, 0, unlimited),
    {'retry-after', {N, Params}};
decode_hdr(<<"route">>, Val) ->
    {'route', [_|_] = decode_uri_field(Val)};
decode_hdr(<<"rseq">>, Val) ->
    {ok, N} = to_integer(Val, 0, unlimited),
    {'rseq', N};
decode_hdr(<<"security-client">>, Val) ->
    {'security-client', decode_params_list(Val)};
decode_hdr(<<"security-server">>, Val) ->
    {'security-server', decode_params_list(Val)};
decode_hdr(<<"security-verify">>, Val) ->
    {'security-verify', decode_params_list(Val)};
decode_hdr('Server', Val) ->
    {'server', Val};
decode_hdr(<<"service-route">>, Val) ->
    {'service-route', [_|_] = decode_uri_field(Val)};
decode_hdr(<<"session-expires">>, Val) ->
    {Delta, Params} = decode_type_params(Val),
    {ok, N} = to_integer(Delta, 0, unlimited),
    {'session-expires', {N, Params}};
decode_hdr(<<"sip-etag">>, Val) ->
    {'sip-etag', Val};
decode_hdr(<<"sip-if-match">>, Val) ->
    {'sip-if-match', Val};
decode_hdr(<<"subject">>, Val) ->
    {'subject', Val};
decode_hdr(<<"subscription-state">>, Val) ->
    {'subscription-state', decode_type_params(Val)};
decode_hdr(<<"supported">>, Val) ->
    {'supported', split(Val, $,)};
decode_hdr(<<"suppress-if-match">>, Val) ->
    {'suppress-if-match', Val};
decode_hdr(<<"target-dialog">>, Val) ->
    {'target-dialog', decode_type_params(Val)};
decode_hdr(<<"timestamp">>, Val) ->
    {N, M} = case split(Val, wsp) of
                 [N1, N2] -> {N1, N2};
                 [N1] -> {N1, <<"0">>}
             end,
    {ok, Num1} = to_float(N, 0, unlimited),
    {ok, Num2} = to_float(M, 0, unlimited),
    {'timestamp', {Num1, Num2}};
decode_hdr(<<"to">>, Val) ->
    [URI] = decode_uri_field(Val),
    {'to', URI};
decode_hdr(<<"trigger-consent">>, Val) ->
    %%TODO
    {'trigger-consent', Val};
decode_hdr(<<"unsupported">>, Val) ->
    {'unsupported', split(Val, $,)};
decode_hdr('User-Agent', Val) ->
    {'user-agent', Val};
decode_hdr('Via', Val) ->
    {'via', decode_via(Val)};
decode_hdr('Warning', Val) ->
    R = lists:map(
          fun(S) ->
                  [Code, Agent, Txt] = split(S, wsp, 2),
                  {ok, N} = to_integer(Code, 100, 699),
                  {N, Agent, Txt}
          end, split(Val, $,)),
    {'warning', R};
decode_hdr('Www-Authenticate', Val) ->
    {'www-authenticate', decode_auth(Val)};
decode_hdr(<<"a">>, Val) ->
    decode_hdr(<<"accept-contact">>, Val);
decode_hdr(<<"u">>, Val) ->
    decode_hdr(<<"allow-events">>, Val);
decode_hdr(<<"i">>, Val) ->
    decode_hdr(<<"call-id">>, Val);
decode_hdr(<<"m">>, Val) ->
    decode_hdr(<<"contact">>, Val);
decode_hdr(<<"e">>, Val) ->
    decode_hdr('Content-Encoding', Val);
decode_hdr(<<"l">>, Val) ->
    decode_hdr('Content-Length', Val);
decode_hdr(<<"c">>, Val) ->
    decode_hdr('Content-Type', Val);
decode_hdr(<<"o">>, Val) ->
    decode_hdr(<<"event">>, Val);
decode_hdr(<<"f">>, Val) ->
    decode_hdr('From', Val);
decode_hdr(<<"y">>, Val) ->
    decode_hdr(<<"identity">>, Val);
decode_hdr(<<"n">>, Val) ->
    decode_hdr(<<"identity-info">>, Val);
decode_hdr(<<"r">>, Val) ->
    decode_hdr(<<"refer-to">>, Val);
decode_hdr(<<"b">>, Val) ->
    decode_hdr(<<"referred-by">>, Val);
decode_hdr(<<"j">>, Val) ->
    decode_hdr(<<"reject-contact">>, Val);
decode_hdr(<<"d">>, Val) ->
    decode_hdr(<<"request-disposition">>, Val);
decode_hdr(<<"x">>, Val) ->
    decode_hdr(<<"session-expires">>, Val);
decode_hdr(<<"s">>, Val) ->
    decode_hdr(<<"subject">>, Val);
decode_hdr(<<"k">>, Val) ->
    decode_hdr(<<"supported">>, Val);
decode_hdr(<<"t">>, Val) ->
    decode_hdr(<<"to">>, Val);
decode_hdr(<<"v">>, Val) ->
    decode_hdr('Via', Val).

encode_hdrs(Hdrs, Method) ->
    lists:map(
      fun({K, V}) ->
              [encode_hdr(K, V, Method), $\r, $\n]
      end, Hdrs).

encode_hdr('accept', Val, _) ->
    [<<"Accept: ">>, encode_params_list(Val)];
encode_hdr('accept-contact', Val, _) ->
    [<<"Accept-Contact: ">>, encode_params_list(Val)];
encode_hdr('accept-encoding', Val, _) ->
    [<<"Accept-Encoding: ">>, encode_params_list(Val)];
encode_hdr('accept-language', Val, _) ->
    [<<"Accept-Language: ">>, encode_params_list(Val)];
encode_hdr('accept-resource-priority', Val, _) ->
    [<<"Accept-Resource-Priority: ">>, join(Val, ", ")];
encode_hdr('alert-info', Val, _) ->
    %%TODO
    [<<"Alert-Info: ">>, Val];
encode_hdr('allow', Val, _) ->
    [<<"Allow: ">>, join(Val, ", ")];
encode_hdr('allow-events', Val, _) ->
    [<<"Allow-Events: ">>, encode_params_list(Val)];
encode_hdr('answer-mode', Val, _) ->
    [<<"Answer-Mode: ">>, encode_type_params(Val)];
encode_hdr('authentication-info', Val, _) ->
    [<<"Authentication-Info: ">>, join_params(Val, ", ")];
encode_hdr('authorization', {Head, Tail}, _) ->
    [<<"Authorization: ">>, Head, $ , join_params(Tail, ", ")];
encode_hdr('call-id', Val, _) ->
    [<<"Call-ID: ">>, Val];
encode_hdr('call-info', Val, _) ->
    %%TODO
    [<<"Call-Info: ">>, Val];
encode_hdr('contact', <<"*">>, _) ->
    [<<"Contact: *">>];
encode_hdr('contact', Val, _) ->
    [<<"Contact: ">>, join([encode_uri_field(U) || U <- Val], ", ")];
encode_hdr('content-disposition', Val, _) ->
    [<<"Content-Disposition: ">>, encode_type_params(Val)];
encode_hdr('content-encoding', Val, _) ->
    [<<"Content-Encoding: ">>, join(Val, ", ")];
encode_hdr('content-language', Val, _) ->
    [<<"Content-Language: ">>, join(Val, ", ")];
encode_hdr('content-length', Val, _) ->
    [<<"Content-Length: ">>, integer_to_list(Val)];
encode_hdr('content-type', Val, _) ->
    [<<"Content-Type: ">>, encode_type_params(Val)];
encode_hdr('cseq', Val, Method) ->
    [<<"CSeq: ">>, integer_to_list(Val), $ , Method];
encode_hdr('date', {{YYYY,MM,DD},{Hour,Min,Sec}}, _) ->
    DayNumber = calendar:day_of_the_week({YYYY,MM,DD}),
    Val = io_lib:format("~s, ~2.2.0w ~3.s ~4.4.0w ~2.2.0w:~2.2.0w:~2.2.0w GMT",
                        [day(DayNumber),DD,month(MM),YYYY,Hour,Min,Sec]),
    [<<"Date: ">>, Val];
encode_hdr('error-info', Val, _) ->
    %%TODO
    [<<"Error-Info: ">>, Val];
encode_hdr('event', Val, _) ->
    [<<"Event: ">>, encode_type_params(Val)];
encode_hdr('expires', Val, _) ->
    [<<"Expires: ">>, integer_to_list(Val)];
encode_hdr('flow-timer', Val, _) ->
    [<<"Flow-Timer: ">>, integer_to_list(Val)];
encode_hdr('from', Val, _) ->
    [<<"From: ">>, encode_uri_field(Val)];
encode_hdr('history-info', Val, _) ->
    [<<"History-Info: ">>, join([encode_uri_field(U) || U <- Val], ", ")];
encode_hdr('identity', Val, _) ->
    [<<"Identity: ">>, Val];
encode_hdr('identity-info', Val, _) ->
    %%TODO
    [<<"Identity-Info: ">>, Val];
encode_hdr('info-package', Val, _) ->
    [<<"Info-Package: ">>, encode_params_list(Val)];
encode_hdr('in-reply-to', Val, _) ->
    [<<"In-Reply-To: ">>, join(Val, ", ")];
encode_hdr('join', Val, _) ->
    [<<"Join: ">>, encode_params_list(Val)];
encode_hdr('max-breadth', Val, _) ->
    [<<"Max-Breadth: ">>, integer_to_list(Val)];
encode_hdr('max-forwards', Val, _) ->
    [<<"Max-Forwards: ">>, integer_to_list(Val)];
encode_hdr('mime-version', {Maj, Min}, _) ->
    [<<"MIME-Version: ">>, Maj+48, $., Min+48];
encode_hdr('min-expires', Val, _) ->
    [<<"Min-Expires: ">>, integer_to_list(Val)];
encode_hdr('min-se', {Delta, Params}, _) ->
    [<<"Min-SE: ">>, encode_type_params({integer_to_list(Delta), Params})];
encode_hdr('organization', Val, _) ->
    [<<"Organization: ">>, Val];
encode_hdr('path', Val, _) ->
    [<<"Path: ">>, join([encode_uri_field(U) || U <- Val], ", ")];
encode_hdr('permission-missing', Val, _) ->
    [<<"Permission-Missing: ">>, join([encode_uri_field(U) || U <- Val], ", ")];
encode_hdr('priority', Val, _) ->
    [<<"Priority: ">>, Val];
encode_hdr('privacy', Val, _) ->
    [<<"Privacy: ">>, join(Val, "; ")];
encode_hdr('priv-answer-mode', Val, _) ->
    [<<"Priv-Answer-Mode: ">>, join(Val, ", ")];
encode_hdr('proxy-authenticate', {Head, Tail}, _) ->
    [<<"Proxy-Authenticate: ">>, Head, $ , join_params(Tail, ", ")];
encode_hdr('proxy-authorization', {Head, Tail}, _) ->
    [<<"Proxy-Authorization: ">>, Head, $ , join_params(Tail, ", ")];
encode_hdr('proxy-require', Val, _) ->
    [<<"Proxy-Require: ">>, join(Val, ", ")];
encode_hdr('rack', {Num, CSeq, Method}, _) ->
    [<<"RAck: ">>, integer_to_list(Num), $ , integer_to_list(CSeq), $ , Method];
encode_hdr('reason', Val, _) ->
    [<<"Reason: ">>, encode_params_list(Val)];
encode_hdr('record-route', Val, _) ->
    [<<"Record-Route: ">>, join([encode_uri_field(U) || U <- Val], ", ")];
encode_hdr('recv-info', Val, _) ->
    [<<"Recv-Info: ">>, encode_params_list(Val)];
encode_hdr('refer-sub', {Head, Tail}, _) ->
    [<<"Refer-Sub: ">>, encode_type_params({atom_to_list(Head), Tail})];
encode_hdr('refer-to', Val, _) ->
    [<<"Refer-To: ">>, encode_uri_field(Val)];
encode_hdr('referred-by', Val, _) ->
    [<<"Referred-By: ">>, encode_uri_field(Val)];
encode_hdr('reject-contact', Val, _) ->
    [<<"Reject-Contact: ">>, encode_params_list(Val)];
encode_hdr('replaces', Val, _) ->
    [<<"Replaces: ">>, encode_type_params(Val)];
encode_hdr('reply-to', Val, _) ->
    [<<"Reply-To: ">>, encode_uri_field(Val)];
encode_hdr('request-disposition', Val, _) ->
    [<<"Request-Disposition: ">>, join(Val, ", ")];
encode_hdr('require', Val, _) ->
    [<<"Require: ">>, join(Val, ", ")];
encode_hdr('resource-priority', Val, _) ->
    [<<"Resource-Priority: ">>, join(Val, ", ")];
encode_hdr('retry-after', {Delta, Params}, _) ->
    [<<"Retry-After: ">>, encode_type_params({integer_to_list(Delta), Params})];
encode_hdr('route', Val, _) ->
    [<<"Route: ">>, join([encode_uri_field(U) || U <- Val], ", ")];
encode_hdr('rseq', Val, _) ->
    [<<"RSeq: ">>, integer_to_list(Val)];
encode_hdr('security-client', Val, _) ->
    [<<"Security-Client: ">>, encode_params_list(Val)];
encode_hdr('security-server', Val, _) ->
    [<<"Security-Server: ">>, encode_params_list(Val)];
encode_hdr('security-verify', Val, _) ->
    [<<"Security-Verify: ">>, encode_params_list(Val)];
encode_hdr('server', Val, _) ->
    [<<"Server: ">>, Val];
encode_hdr('service-route', Val, _) ->
    [<<"Service-Route: ">>, join([encode_uri_field(U) || U <- Val], ", ")];
encode_hdr('session-expires', {Delta, Params}, _) ->
    [<<"Session-Expires: ">>, encode_type_params({integer_to_list(Delta), Params})];
encode_hdr('sip-etag', Val, _) ->
    [<<"SIP-ETag: ">>, Val];
encode_hdr('sip-if-match', Val, _) ->
    [<<"SIP-If-Match: ">>, Val];
encode_hdr('subject', Val, _) ->
    [<<"Subject: ">>, Val];
encode_hdr('subscription-state', Val, _) ->
    [<<"Subscription-State: ">>, encode_type_params(Val)];
encode_hdr('supported', Val, _) ->
    [<<"Supported: ">>, join(Val, ", ")];
encode_hdr('suppress-if-match', Val, _) ->
    [<<"Suppress-If-Match: ">>, Val];
encode_hdr('target-dialog', Val, _) ->
    [<<"Target-Dialog: ">>, encode_type_params(Val)];
encode_hdr('timestamp', {N1, N2}, _) ->
    [<<"Timestamp: ">>, number_to_list(N1), $ , number_to_list(N2)];
encode_hdr('to', Val, _) ->
    [<<"To: ">>, encode_uri_field(Val)];
encode_hdr('trigger-consent', Val, _) ->
    %%TODO
    [<<"Trigger-Consent: ">>, Val];
encode_hdr('unsupported', Val, _) ->
    [<<"Unsupported: ">>, join(Val, ", ")];
encode_hdr('user-agent', Val, _) ->
    [<<"User-Agent: ">>, Val];
encode_hdr('via', Val, _) ->
    join([[<<"Via: ">>, encode_via(V)] || V <- Val], "\r\n");
encode_hdr('warning', Val, _) ->
    L = [[integer_to_list(C), $ , A, $ , T] || {C, A, T} <- Val],
    [<<"Warning: ">>, join(L, ", ")];
encode_hdr('www-authenticate', {Head, Tail}, _) ->
    [<<"WWW-Authenticate: ">>, Head, $ , join_params(Tail, ", ")];
encode_hdr(Key, Val, _) when is_binary(Key), is_binary(Val) ->
    [Key, ": ", Val];
encode_hdr(Key, Val, _) ->
    erlang:error({bad_header, Key, Val}).

prepare_hdrs(Hdrs) ->
    lists:reverse(Hdrs).

join_params(Params, Sep) ->
    join(lists:map(
           fun({Key, Val}) when Key /= <<>>, Val /= <<>> ->
                   [Key, $=, Val];
              ({Key, _}) when Key /= <<>> ->
                   [Key];
              (_) ->
                   <<>>
           end, Params), Sep).

encode_params(Params) ->
    lists:map(
      fun({Key, Val}) when Key /= <<>>, Val /= <<>> ->
              [$;, Key, $=, Val];
         ({Key, _}) when Key /= <<>> ->
              [$;, Key];
         (_) ->
              <<>>
      end, Params).

decode_param_hdrs(Data) ->
    decode_params(Data, $&).

decode_params(Data) ->
    decode_params(Data, $;).

decode_params(Data, Sep) ->
    lists:flatmap(
      fun(<<>>) ->
              [];
         (S) ->
              case split(S, $=) of
                  [Key, Val] ->
                      [{strip_wsp(Key, right), strip_wsp(Val, left)}];
                  _ ->
                      [{S, <<>>}]
              end
      end, split(Data, Sep)).

decode_params_list(Data) ->
    lists:flatmap(
      fun(<<>>) ->
              [];
         (S) ->
              [decode_type_params(S)]
      end,
      split(Data, $,)).

decode_type_params(Data) ->
    case split(Data, $;, 1) of
        [Type, Params] ->
            {strip_wsp(Type, right), decode_params(Params)};
        [Type] ->
            {strip_wsp(Type, right), []}
    end.

encode_params_list(L) ->
    join([encode_type_params(TP) || TP <- L], ", ").

encode_type_params({Type, Params}) ->
    [Type, encode_params(Params)].

decode_auth(Data) ->
    [Head, Tail] = split(Data, wsp, 1),
    {strip_wsp(Head, right), decode_params(strip_wsp(Tail, left), $,)}.

strip_wsp(Data) ->
    strip_wsp(Data, both).

strip_wsp(Data, left) ->
    strip_wsp_left(Data);
strip_wsp(Data, right) ->
    strip_wsp_right(Data);
strip_wsp(Data, both) ->
    strip_wsp_left(strip_wsp_right(Data)).

strip_wsp_left(<<$\r, Rest/binary>>) ->
    strip_wsp_left(Rest);
strip_wsp_left(<<$\n, Rest/binary>>) ->
    strip_wsp_left(Rest);
strip_wsp_left(<<$\t, Rest/binary>>) ->
    strip_wsp_left(Rest);
strip_wsp_left(<<$ , Rest/binary>>) ->
    strip_wsp_left(Rest);
strip_wsp_left(Str) ->
    Str.

strip_wsp_right(Data) ->
    reverse(strip_wsp_left(reverse(Data))).

split(Bin) ->
    split(Bin, wsp).

split(Bin, Chr) ->
    split(Bin, Chr, -1).

split(Bin, Chr, N) ->
    lists:reverse(split(Bin, <<>>, [], Chr, out, N, undefined)).

split(Bin, <<>>, Acc, Chr, _, 0, _) ->
    add_to_acc(Bin, Acc, Chr);
split(<<C, Rest/binary>>, Buf, Acc, Chr, in, N, PrevChr) ->
    if C == $", PrevChr /= $\\ ->
            %% Leaving quoted string
            split(Rest, <<Buf/binary, $">>, Acc, Chr, out, N, C);
       true ->
            %% Ignore any other characters
            split(Rest, <<Buf/binary, C>>, Acc, Chr, in, N, C)
    end;
split(<<C, Rest/binary>>, Buf, Acc, Chr, out, N, _) ->
    if C == $" ->
            %% Entering quoted string
            split(Rest, <<Buf/binary, $">>, Acc, Chr, in, N, C);
       C == Chr ->
            split(Rest, <<>>, add_to_acc(Buf, Acc, Chr), Chr, out, N-1, C);
       (Chr == wsp) and
       ((C == $\r) or (C == $\n) or (C == $\t) or (C == $ )) ->
            split(Rest, <<>>, add_to_acc(Buf, Acc, Chr), Chr, out, N-1, C);
       true ->
            split(Rest, <<Buf/binary, C>>, Acc, Chr, out, N, C)
    end;
split(<<>>, _, Acc, _, in, _, _) ->
    Acc;
split(<<>>, Buf, Acc, Chr, _, _, _) ->
    add_to_acc(Buf, Acc, Chr).

add_to_acc(Buf, Acc, Chr) ->
    case strip_wsp(Buf) of
        <<>> when Chr == wsp ->
            Acc;
        NewBuf ->
            [NewBuf|Acc]
    end.

reverse(Bin) ->
    list_to_binary(lists:reverse(binary_to_list(Bin))).

to_lower(Bin) ->
    list_to_binary(string:to_lower(binary_to_list(Bin))).

to_upper(Bin) ->
    list_to_binary(string:to_upper(binary_to_list(Bin))).

strcasecmp(Bin1, Bin2) ->
    to_lower(Bin1) == to_lower(Bin2).

to_integer(Bin, Min, Max) ->
    case catch list_to_integer(binary_to_list(Bin)) of
        N when N >= Min, N =< Max ->
            {ok, N};
        _ ->
            error
    end.

to_float(Bin, Min, Max) ->
    S = binary_to_list(Bin),
    case catch list_to_integer(S) of
        N when N >= Min, N =< Max ->
            {ok, N};
        {'EXIT', _} ->
            case catch erlang:list_to_float(S) of
                N when N >= Min, N =< Max ->
                    {ok, N};
                _ ->
                    error
            end;
        _ ->
            error
    end.

atom_to_binary1(Atom) ->
    list_to_binary(string:to_lower(atom_to_list(Atom))).

number_to_list(N) ->
    if is_float(N) ->
            io_lib:format("~.g", [N]);
       is_integer(N) ->
            integer_to_list(N)
    end.

str(Bin, Sep) ->
    case string:str(binary_to_list(Bin), binary_to_list(Sep)) of
        0 ->
            nomatch;
        N ->
            N-1
    end.

join([], _Sep) ->
    [];
join([H|T], Sep) ->
    [H, [[Sep, X] || X <- T]].

escape_chr(Chr) ->
    [$%, to_hex(Chr)].

%% day

day(1) -> "Mon";
day(2) -> "Tue";
day(3) -> "Wed";
day(4) -> "Thu";
day(5) -> "Fri";
day(6) -> "Sat"; 
day(7) -> "Sun".

%% month

month(1) -> "Jan";
month(2) -> "Feb";
month(3) -> "Mar";
month(4) -> "Apr";
month(5) -> "May";
month(6) -> "Jun";
month(7) -> "Jul";
month(8) -> "Aug";
month(9) -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".

-define(MSG1,
    <<"OPTIONS sip:xram@zinid.ru:5090 SIP/2.0\r\n"
      "Via: SIP/2.0/UDP 192.168.1.1:9;branch=z9hG4bK.697350a0\r\n"
      "From: sip:sipsak@192.168.1.1:9;tag=721d6a76\r\n"
      "To: sip:xram@zinid.ru:5090\r\n"
      "Call-ID: 1914530422@192.168.1.1\r\n"
      "CSeq: 413528 OPTIONS\r\n"
      "Contact: sip:sipsak@192.168.1.1:9\r\n"
      "Content-Length: 0\r\n"
      "User-Agent: sipsak 0.9.6\r\n"
      "Max-Forwards: 70\r\n\r\n">>).

-define(MSG,
    <<"INVITE sips:B@example.com SIP/2.0\r\n"
      "Accept: application/dialog-info+xml\r\n"
      "Accept-Contact: *;methods=\"BYE\";class=\"business\";q=1.0\r\n"
      "Accept-Encoding: gzip\r\n"
      "Accept-Language: da, en-gb;q=0.8, en;q=0.7\r\n"
      "Accept-Resource-Priority: dsn.flash-override, dsn.flash\r\n"
      "Alert-Info: <http://www.example.com/sounds/moo.wav>\r\n"
      "Allow: INVITE, ACK, OPTIONS, CANCEL, BYE\r\n"
      "Allow-Events: spirits-INDPs\r\n"
      "Answer-Mode: Manual\r\n"
      "Authentication-Info: nextnonce=\"47364c23432d2e131a5fb210812c\"\r\n"
      "Authorization: Digest username=\"bob\", realm=\"atlanta.example.com\","
      " nonce=\"ea9c8e88df84f1cec4341ae6cbe5a359\", opaque=\"\","
      " uri=\"sips:ss2.biloxi.example.com\","
      " response=\"dfe56131d1958046689d83306477ecc\"\r\n"
      "Call-ID: a84b4c76e66710\r\n"
      "Call-Info: <http://wwww.example.com/alice/photo.jpg> ;purpose=icon,"
      " <http://www.example.com/alice/> ;purpose=info\r\n"
      "Contact: <sip:test@test.org>\r\n"
      "Content-Disposition: session;handling=optional\r\n"
      "Content-Encoding: gzip\r\n"
      "Content-Language: fr\r\n"
      "Content-Length: 8\r\n"
      "Content-Type: multipart/signed; gzip=true\r\n"
      "CSeq: 314159 INVITE\r\n"
      "Date: Thu, 21 Feb 2002 13:02:03 GMT\r\n"
      "Error-Info: <sips:screen-failure-term-ann@annoucement.example.com>\r\n"
      "Event: refer\r\n"
      "Expires: 7200\r\n"
      "Flow-Timer: 126\r\n"
      "From: Anonymous <sip:anonymous@atlanta.com>;tag=1928301774\r\n"
      "History-Info:<sip:UserA@ims.example.com?Reason=SIP%3B\\"
      " cause%3D302>;index=1;foo=bar\r\n"
      "Identity: \r\n"
      " \"sv5CTo05KqpSmtHt3dcEiO/1CWTSZtnG3iV+1nmurLXV/HmtyNS7Ltrg9dlxkWzo\r\n"
      " eU7d7OV8HweTTDobV3itTmgPwCFjaEmMyEI3d7SyN21yNDo2ER/Ovgtw0Lu5csIp\r\n"
      " pPqOg1uXndzHbG7mR6Rl9BnUhHufVRbp51Mn3w0gfUs=\"\r\n"
      "Identity-Info: <https://biloxi.example.org/biloxi.cer>;alg=rsa-sha1\r\n"
      "Info-Package: package\r\n"
      "In-Reply-To: 70710@saturn.bell-tel.com, 17320@saturn.bell-tel.com\r\n"
      "Join: 12345600@atlanta.example.com;from-tag=1234567;to-tag=23431\r\n"
      "Max-Breadth: 69\r\n"
      "Max-Forwards: 70\r\n"
      "MIME-Version: 1.0\r\n"
      "Min-Expires: 60\r\n"
      "Min-SE: 3600;lr=true\r\n"
      "Organization: Boxes by Bob\r\n"
      %% "P-Access-Network-Info: blah\r\n"
      %% "P-Answer-State: blah\r\n"
      "P-Asserted-Identity: \"Cullen Jennings\" <sip:fluffy@cisco.com>\r\n"
      %% "P-Asserted-Service: blah\r\n"
      %% "P-Associated-URI: blah\r\n"
      "P-Called-Party-ID: sip:user1-business@example.com\r\n"
      "P-Charging-Function-Addresses: ccf=192.1.1.1; ccf=192.1.1.2\r\n"
      "P-Charging-Vector: icid-value=1234bc9876e;\r\n"
      " icid-generated-at=192.0.6.8; orig-ioi=home1.net\r\n"
      %% "P-DCS-Trace-Party-ID: blah\r\n"
      %% "P-DCS-OSPS: blah\r\n"
      %% "P-DCS-Billing-Info: blah\r\n"
      %% "P-DCS-LAES: blah\r\n"
      %% "P-DCS-Redirect: blah\r\n"
      %% "P-Early-Media: blah\r\n"
      %% "P-Media-Authorization: blah\r\n"
      "P-Preferred-Identity: \"Cullen Jennings\" <sip:fluffy@cisco.com>\r\n"
      %% "P-Preferred-Service: blah\r\n"
      "P-Profile-Key: <sip:chatroom-!.*!@example.com>\r\n"
      %% "P-Refused-URI-List: blah\r\n"
      %% "P-Served-User: blah\r\n"
      "P-User-Database: <aaa://host.example.com;transport=tcp>\r\n"
      "P-Visited-Network-ID: other.net, \"Visited network number 1\"\r\n"
      "Path: <sip:P3.EXAMPLEHOME.COM;lr>,<sip:P1.EXAMPLEVISITED.COM;lr>\r\n"
      "Permission-Missing: sip:C@example.com\r\n"
      "Priority: emergency\r\n"
      "Privacy: id\r\n"
      "Priv-Answer-Mode: Auto\r\n"
      "Proxy-Authenticate: Digest realm=\"atlanta.example.com\", qop=\"auth\",\r\n"
      " nonce=\"f84f1cec41e6cbe5aea9c8e88d359\",\r\n"
      " opaque=\"\", stale=FALSE, algorithm=MD5\r\n"
      "Proxy-Authorization: Digest username=\"alice\",\r\n"
      " realm=\"atlanta.example.com\",\r\n"
      " nonce=\"wf84f1ceczx41ae6cbe5aea9c8e88d359\", opaque=\"\",\r\n"
      " uri=\"sip:bob@biloxi.example.com\",\r\n"
      " response=\"42ce3cef44b22f50c6a6071bc8\"\r\n"
      "Proxy-Require: sec-agree\r\n"
      "RAck: 776656 1 INVITE\r\n"
      "Reason: SIP ;cause=200 ;text=\"Call completed elsewhere\"\r\n"
      "Record-Route: xram <sip:home.local;transport=tcp>\r\n"
      "Recv-Info: \r\n"
      "Refer-Sub: false\r\n"
      "Refer-To: <sips:a8342043f@atlanta.example.com?Replaces=\r\n"
      " 12345601%40atlanta.example.com%3Bfrom-tag%3D314159%3Bto-tag%3D1234567>\r\n"
      "Referred-By: <sip:referrer@referrer.example>\r\n"
      " ;cid=\"20398823.2UWQFN309shb3@referrer.example\"\r\n"
      "Reject-Contact: *;actor=\"msg-taker\";video\r\n"
      "Replaces: 425928@bobster.example.org;to-tag=7743;from-tag=6472\r\n"
      "Reply-To: Bob <sip:bob@biloxi.com>\r\n"
      "Request-Disposition: proxy, recurse, parallel\r\n"
      "Require: 100rel\r\n"
      "Resource-Priority: wps.3, dsn.flash\r\n"
      "Retry-After: 18000;duration=3600\r\n"
      "Route: <sip:ss1.atlanta.example.com;lr>,\r\n"
      " <sip:ss2.biloxi.example.com;lr>\r\n"
      "RSeq: 988789\r\n"
      "Security-Client: digest;q=0.1,ipsec-ike\r\n"
      "Security-Server: ipsec-ike;q=0.1\r\n"
      "Security-Verify: tls;q=0.2\r\n"
      "Server: HomeServer v2\r\n"
      "Service-Route: <sip:P2.HOME.EXAMPLE.COM;lr>,\r\n"
      " <sip:HSP.HOME.EXAMPLE.COM;lr>\r\n"
      "Session-Expires: 4000;refresher=uac\r\n"
      "SIP-ETag: dx200xyz\r\n"
      "SIP-If-Match: dx200xyz\r\n"
      "Subject: A tornado is heading our way!\r\n"
      "Subscription-State: active;expires=60\r\n"
      "Supported: replaces, 100rel\r\n"
      "Suppress-If-Match: *\r\n"
      "Target-Dialog: fa77as7dad8-sd98ajzz@host.example.com\r\n"
      " ;local-tag=kkaz-;remote-tag=6544\r\n"
      "Timestamp: 54 0.35\r\n"
      "To: Bob <sip:bob@biloxi.example.com>;tag=8321234356\r\n"
      "Trigger-Consent: sip:123@relay.example.com\r\n"
      " ;target-uri=\"sip:friends@relay.example.com\"\r\n"
      "Unsupported: 100rel\r\n"
      "User-Agent: Softphone Beta1.5\r\n"
      "Via: SIP/2.0/UDP atlanta.com:5060;branch=z9hG4bKnashds8\r\n"
      "Warning: 301 isi.edu \"Incompatible network address type 'E.164'\"\r\n"
      "WWW-Authenticate: Digest realm=\"atlanta.example.com\", qop=\"auth\",\r\n"
      " nonce=\"84f1c1ae6cbe5ua9c8e88dfa3ecm3459\",\r\n"
      " opaque=\"\", stale=FALSE, algorithm=MD5\r\n"
      "\r\nSIP body">>).

test() ->
    io:format("~p~n", [decode(?MSG)]).

test_loop() ->
    N = 10000,
    eprof:start(),
    _P = spawn(?MODULE, test_loop, [self(), ?MSG, N]),
    %%eprof:start_profiling([_P]),
    receive {ok, T} -> ok end,
    eprof:stop_profiling(),
    eprof:analyze(),
    io:format("~n== Estimate: ~p~n", [T div N]).

test_loop(P, Msg, N) ->
    test_loop(P, Msg, N, now()).

test_loop(P, _, 0, T) ->
    P ! {ok, timer:now_diff(now(), T)};
test_loop(P, Msg, N, T) ->
    decode(Msg),
    test_loop(P, Msg, N-1, T).
