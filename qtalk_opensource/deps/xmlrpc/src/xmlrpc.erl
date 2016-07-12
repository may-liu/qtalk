%% Hacked by Romuald du Song
%%
%% Copyright (C) 2003 Joakim Grebenö <jocke@gleipnir.com>.
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met: 
%%
%% 1. Redistributions of source code must retain the above copyright
%%    notice, this list of conditions and the following disclaimer. 
%% 2. Redistributions in binary form must reproduce the above
%%    copyright notice, this list of conditions and the following
%%    disclaimer in the documentation and/or other materials provided
%%    with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
%% OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
%% GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-module(xmlrpc).
-author('jocke@gleipnir.com').
-export([call/3, call/4, call/5, call/6, call/7, call/8, call2/7]).
-export([start_link/1, start_link/5, start_link/6, stop/1]).

%% Only for tests
-export([parse_response/3, open_socket/3]).

-include("log.hrl").

-include("xmlrpc.hrl").

%% Type definitions

-type socket() :: term().

-type ip_address() :: inet:ip_address().

-type host() :: string() | ip_address().

-type iso8601date() :: {date, string()}.

-type base64() :: {base64, string()}.

-type uri() :: string().

-type xmlrpc_value() :: integer() | float() | string() | boolean()
    | iso8601date() | base64() | xmlrpc_struct() | xmlrpc_array().

-type xmlrpc_struct() :: {struct, [{Key::atom(), Value::xmlrpc_value()}]}.

-type xmlrpc_array() :: {array, [xmlrpc_value()]}.

-type response_payload() :: {response, [xmlrpc_value()]}
    | {response, [xmlrpc_value()], #header{}}
    | {response, {fault, Code::integer(),
                  Message::string()}}.

-type call_result() :: {ok, response_payload()}
    | {ok, response_payload(), #header{}}
    | {ok, socket(), response_payload()}
    | {ok, socket(), response_payload(), #header{}}
    | {error, Reason::term()}
    | {error, socket(), Reason::term()}.

-type options() :: [{string(), term()}].

%% Exported: call/{3,4,5,6,7,8}

-spec call(Socket, URI, Payload) -> call_result()
 when Socket :: socket(),
    URI :: uri(),
    Payload :: {call, Method::atom(), Arguments::[xmlrpc_value()]}.

call(Socket, URI, Payload) ->
    call2(Socket, URI, Payload, false, 60000, "", [{ssl, false}, {header, false}]).

-spec call(Host, Port, URI, Payload, Options) -> call_result()
 when Host :: host(),
    Port :: integer(),
    URI :: uri(),
    Payload :: {call, Method::atom(), Arguments::[xmlrpc_value()]},
    Options :: options();
(Socket, URI, Payload, KeepAlive, Timeout) -> call_result()
 when Socket :: socket(),
    URI :: uri(),
    Payload :: {call, Method::atom(), Arguments::[xmlrpc_value()]},
    KeepAlive :: boolean(),
    Timeout :: integer().

call(Host, Port, URI, Payload, Options) when is_number(Port) ->
    call(Host, Port, URI, Payload, false, 60000, "", Options);

call(Socket, URI, Payload, KeepAlive, Timeout) ->
    call2(Socket, URI, Payload, KeepAlive, Timeout, "", [{ssl, false}, {header, false}]).

-spec call(Host, Port, URI, Payload) -> call_result()
 when Host :: host(),
    Port :: integer(),
    URI :: uri(),
    Payload :: {call, Method::atom(), Arguments::[xmlrpc_value()]};
(Socket, URI, Payload, Options) -> call_result()
 when Socket :: socket(),
    URI :: uri(),
    Payload :: {call, Method::atom(), Arguments::[xmlrpc_value()]},
    Options :: options().

call(Host, Port, URI, Payload) when is_number(Port) ->
    call(Host, Port, URI, Payload, false, 60000, "", [{ssl, false}, {header, false}]);

call(Socket, URI, Payload, Options) ->
    call2(Socket, URI, Payload, false, 60000, "", Options).

-spec call(Host, Port, URI, Payload, KeepAlive, Timeout) -> call_result()
 when Host :: host(),
    Port :: integer(),
    URI :: uri(),
    Payload :: {call, Method::atom(), Arguments::[xmlrpc_value()]},
    KeepAlive :: boolean(),
    Timeout :: integer();
(Socket, URI, Payload, KeepAlive, Timeout, Options) -> call_result()
 when Socket :: socket(),
    URI :: uri(),
    Payload :: {call, Method::atom(), Arguments::[xmlrpc_value()]},
    KeepAlive :: boolean(),
    Timeout :: integer(),
    Options :: options().

call(Host, Port, URI, Payload, KeepAlive, Timeout) when is_number(Port) ->
    call(Host, Port, URI, Payload, KeepAlive, Timeout, "", [{ssl, false}, {header, false}]);

call(Socket, URI, Payload, KeepAlive, Timeout, Options) ->
    call2(Socket, URI, Payload, KeepAlive, Timeout, "", Options).

-spec call(Host, Port, URI, Payload, KeepAlive, Timeout, Options) -> call_result()
 when Host :: host(),
    Port :: integer(),
    URI :: uri(),
    Payload :: {call, Method::atom(), Arguments::[xmlrpc_value()]},
    KeepAlive :: boolean(),
    Timeout :: integer(),
    Options :: options().

call(Host, Port, URI, Payload, KeepAlive, Timeout, Options) when is_number(Port) ->
    call(Host, Port, URI, Payload, KeepAlive, Timeout, "", Options);

call(Socket, URI, Payload, KeepAlive, Timeout, ExtraHeader, Options) ->
  call2(Socket, URI, Payload, KeepAlive, Timeout, ExtraHeader, Options).

-spec call(Host, Port, URI, Payload, KeepAlive, Timeout, ExtraHeaders, Options) -> call_result()
 when Host :: host(),
    Port :: integer(),
    URI :: uri(),
    Payload :: {call, Method::atom(), Arguments::[xmlrpc_value()]},
    KeepAlive :: boolean(),
    Timeout :: integer(),
    ExtraHeaders :: string(),
    Options :: options().

call(Host, Port, URI, Payload, KeepAlive, Timeout, ExtraHeaders, Options) when is_number(Port)  ->
    case open_socket(Host, Port, Options) of
        {ok, Socket} -> call2(Socket, URI, Payload, KeepAlive, Timeout, ExtraHeaders, Options);
        {error, Reason} when KeepAlive == false -> {error, Reason};
        {error, Reason} -> {error, undefined, Reason}
    end.


open_socket(Host, Port, Options) ->
    case fetch_comm_module(Options) of
        ssl ->
            %% Start ssl application
            application:start(ssl),
            %% Always seed
            ssl:seed("wheredoyouthinkitcanbefound"),
            %% new ssl implementation does not seem to work as of R13B01
            %%{ok, SslSocket} = ssl:connect(Host, Port, [{ssl_imp, new}, {active, false}, {verify, verify_none}]),
            ssl:connect(Host, Port, [{verify, 0}, {active, false}]);
        _ ->
            gen_tcp:connect(Host, Port, [{active, false}])
    end.


call2(Socket, URI, Payload, KeepAlive, Timeout, ExtraHeader, Options) ->
    ?DEBUG_LOG({payload_call, Payload}),
    case xmlrpc_encode:payload(Payload) of
        {ok, EncodedPayload} ->
            ?DEBUG_LOG({encoded_call, EncodedPayload}),
            case send(Socket, URI, KeepAlive, EncodedPayload, ExtraHeader, Options) of
                ok ->
                    case parse_response(Socket, Timeout, Options) of
                        {ok, Header} ->
                            handle_payload(Socket, KeepAlive, Timeout, Options, Header);
                        {error, Reason} when KeepAlive == false ->
                            comm_close(Options, Socket),
                            {error, Reason};
                        {error, Reason} -> {error, Socket, Reason}
                    end;
                {error, Reason} when KeepAlive == false ->
                    comm_close(Options, Socket),
                    {error, Reason};
                {error, Reason} -> {error, Socket, Reason}
            end;
        {error, Reason} when KeepAlive == false ->
            comm_close(Options, Socket),
            {error, Reason};
        {error, Reason} -> {error, Socket, Reason}
    end.

send(Socket, URI, false, Payload, ExtraHeader, SslOption) ->
    send(Socket, URI, lists:flatten(["Connection: close\r\n" | ExtraHeader]), Payload, SslOption);
send(Socket, URI, true, Payload, ExtraHeader, SslOption) ->
    send(Socket, URI, ExtraHeader, Payload, SslOption).

send(Socket, URI, Header, Payload, SslOption) ->
    Request =
              ["POST ", URI, " HTTP/1.1\r\n",
               "Content-Length: ", integer_to_list(lists:flatlength(Payload)), "\r\n",
               "User-Agent: Erlang XML-RPC Client 1.13\r\n",
               "Content-Type: text/xml\r\n",
               Header, "\r\n",
               Payload],
    M = fetch_comm_module(SslOption),
    apply(M, send, [Socket, Request]).

parse_response(Socket, Timeout, SslOption) ->
    M = fetch_comm_module(SslOption),
    S = fetch_sets_module(SslOption),
    apply(S, setopts, [Socket, [{packet, line}]]),
    case apply(M, recv, [Socket, 0, Timeout]) of
        {ok, "HTTP/1.1 200 OK\r\n"} -> parse_header(Socket, Timeout, SslOption);
        {ok, StatusLine} -> {error, StatusLine};
        {error, Reason} -> {error, Reason}
    end.

fetch_comm_module(Options) ->
    case lists:keysearch(ssl, 1, Options) of
        {value, {ssl, true}} -> ssl;
        _ -> gen_tcp
    end.

has_header_option(Options) ->
    case lists:keysearch(header, 1, Options) of
        {value, {_, true}} -> true;
        _ -> false
    end.

fetch_sets_module(Options) ->
    case lists:keysearch(ssl, 1, Options) of
        {value, {ssl, true}} -> ssl;
        _ -> inet
    end.

comm_close(Options, Socket) ->
    M = fetch_comm_module(Options),
    apply(M, close, [ Socket ]).

parse_header(Socket, Timeout, SslOption) -> parse_header(Socket, Timeout, SslOption, #header{}).

parse_header(Socket, Timeout, SslOption, Header) ->
    M = fetch_comm_module(SslOption),
    case apply(M, recv, [Socket, 0, Timeout]) of
        {ok, "\r\n"} when Header#header.content_length == undefined ->
            {error, missing_content_length};
        {ok, "\r\n"} -> {ok, Header};
        {ok, HeaderField} ->
            case string:tokens(string:to_lower(HeaderField), " \r\n") of
                ["content-length:", ContentLength] ->
                    case catch list_to_integer(ContentLength) of
                        badarg ->
                            {error, {invalid_content_length, ContentLength}};
                        Value ->
                            parse_header(Socket, Timeout, SslOption,
                                         Header#header{content_length =
                                                       Value})
                    end;
                ["connection:", "close"] ->
                    parse_header(Socket, Timeout, SslOption,
                                 Header#header{connection = close});
                ["x-forwarded-for:", XForwardedFor] ->
                    parse_header(Socket, Timeout, SslOption,
                                 Header#header{xforwardedfor = XForwardedFor});
                ["authorization:", Authorization] ->
                    parse_header(Socket, Timeout, SslOption,
                                 Header#header{authorization = Authorization});
                ["cookie:", Cookie] ->
                    Cookies = [ Cookie | Header#header.cookies ],
                    parse_header(Socket, Timeout, SslOption,
                                 Header#header{cookies = Cookies});
                _ ->
                    parse_header(Socket, Timeout, SslOption, Header)
            end;
        {error, Reason} -> {error, Reason}
    end.

handle_payload(Socket, KeepAlive, Timeout, Options, Header) ->
    case get_payload(Socket, Timeout, Options, Header#header.content_length) of
        {ok, Payload} ->
            ?DEBUG_LOG({encoded_response, Payload}),
            case xmlrpc_decode:payload(Payload) of
                {ok, {response, DecodedPayload}} when KeepAlive == false ->
                    ?DEBUG_LOG({decoded_response, DecodedPayload}),
                    comm_close(Options, Socket),
                    case has_header_option(Options) of
                        true ->
                            {ok, {response, DecodedPayload, Header}};
                        _ ->
                            {ok, {response, DecodedPayload}}
                    end;
                {ok, {response, DecodedPayload}} when KeepAlive == true,
                        Header#header.connection == close ->
                    ?DEBUG_LOG({decoded_response, DecodedPayload}),
                    comm_close(Options, Socket),
                    case has_header_option(Options) of
                        true ->
                            {ok, Socket, {response, DecodedPayload, Header}};
                        _ ->
                            {ok, Socket, {response, DecodedPayload}}
                    end;
                {ok, {response, DecodedPayload}} ->
                    ?DEBUG_LOG({decoded_response, DecodedPayload}),
                    case has_header_option(Options) of
                        true ->
                            {ok, Socket, {response, DecodedPayload, Header}};
                        _ ->
                            {ok, Socket, {response, DecodedPayload}}
                    end;
                {error, Reason} when KeepAlive == false ->
                    comm_close(Options, Socket),
                    {error, Reason};
                {error, Reason} when KeepAlive == true,
                        Header#header.connection == close ->
                    comm_close(Options, Socket),
                    {error, Socket, Reason};
                {error, Reason} ->
                    {error, Socket, Reason}
            end;
        {error, Reason} when KeepAlive == false ->
            gen_tcp:close(Socket),
            {error, Reason};
        {error, Reason} when KeepAlive == true,
                Header#header.connection == close ->
            comm_close(Options, Socket),
            {error, Socket, Reason};
        {error, Reason} -> {error, Socket, Reason}
    end.

get_payload(Socket, Timeout, SslOption, ContentLength) ->
    M = fetch_comm_module(SslOption),
    apply(fetch_sets_module(SslOption), setopts, [Socket, [{packet, raw}]]),
    apply(M, recv, [Socket, ContentLength, Timeout]).

%% Exported: start_link/{1,5,6}

start_link(Handler) -> start_link(4567, 1000, 60000, Handler, undefined).

start_link(Port, MaxSessions, Timeout, Handler, State) ->
    start_link(all, Port, MaxSessions, Timeout, Handler, State).

start_link(IP, Port, MaxSessions, Timeout, Handler, State) ->
    OptionList = [{active, false}, {reuseaddr, true}] ++ ip(IP),
    SessionHandler = {xmlrpc_http, handler, [Timeout, Handler, State]},
    tcp_serv:start_link([Port, MaxSessions, OptionList, SessionHandler]).

ip(all) -> [];
ip(IP) when is_tuple(IP) -> [{ip, IP}].

%% Exported: stop/1

stop(Pid) -> tcp_serv:stop(Pid).
