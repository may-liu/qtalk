%% Copyright (C) 2009 Romuald du Song <rdusong _AT_ gmail _DOT_ com>.
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

-module(xmlrpc_tests).
-export([handler/2,start/0]).
-import(xmlrpc, [parse_response/3, open_socket/3]).

-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

start_stop_test_() ->
    {"The server can be started, stopped",
     ?setup(fun is_alive/1)}.

variables_types_test_() ->
    [{"A simple int value is be a valid parameter type",
     ?setup(fun handle_int_call/1)},
     {"A simple float value is a valid parameter type",
      ?setup(fun handle_float_call/1)},
     {"A complex struct value is a valid parameter type",
      ?setup(fun handle_struct_call/1)}
     ].

various_call_test_() ->
    [{"A timeout call time can be made",
      ?setup(fun handle_timeout_call/1)},
     {"A call with a additional header can be made",
      ?setup(fun handle_extraheader_call/1)},
     {"A call with a wrong header format should result in error",
      ?setup(fun handle_wrong_header_call/1)}].

header_variants_test_() ->
    [{"A HTTP header can be specified in lowcase",
     ?setup(fun handle_lowcase_header/1)},
    {"The content type charset is ignored",
     ?setup(fun handle_content_type_iso8859/1)}].

is_alive(Pid) ->
    [?_assert(erlang:is_process_alive(Pid))].

handle_int_call(_Pid) ->
    ShouldBe = {ok,{response,[{array, [42]}]}},
    ?_assertEqual(ShouldBe, xmlrpc:call(localhost, 4567, "/",
                                         {call, echo, [42]})).

handle_float_call(_Pid) ->
    ShouldBe = {ok,{response,[{array, [42.000]}]}},
    [?_assertEqual(ShouldBe, xmlrpc:call(localhost, 4567, "/",
                          {call, echo, [42.0]}))].

handle_struct_call(_Pid) ->
    [?_assert(xmlrpc:call({127, 0, 0, 1}, 4567, "/",
                        {call, echo, [2.6,
                                      {array, [5, "foo"]},
                                      {struct,
                                       [{baz, 1},
                                        {bar, {base64, "aXMgdGhpcyBiaW5hcnkgPw=="}}]
                                      }]}) =:=
            {ok,{response,[{array,[2.60000,
                                   {array,[5,"foo"]},
                                   {struct,
                                    [{baz,1},
                                     {bar,{base64,"aXMgdGhpcyBiaW5hcnkgPw=="}}]
                                   }]}]}})].

handle_timeout_call(_Pid) ->
    ShouldBe = {ok,{response,[{array, [42]}]}},
    Response = xmlrpc:call(localhost, 4567, "/",
                        {call, echo, [42]}, false, 10000),
    ?_assertEqual(ShouldBe, Response).

handle_extraheader_call(_Pid) ->
    Options = [{ssl, false}],
    Cookie = "Cookie: DUMMY=yes\r\n",
    Timeout = 10000,
    Response = xmlrpc:call(localhost, 4567, "/",
                           {call, echo, [421]}, false, Timeout,
                           Cookie, Options),
    ?_assertEqual({ok,{response,[{array, [421]}]}}, Response).

handle_wrong_header_call(_Pid) ->
    Options = [{ssl, false}],
    Cookie = "Cookie: DUMMY=This is not a valid header",
    Timeout = 5000,
    Response = xmlrpc:call(localhost, 4567, "/",
                           {call, echo, [421]}, false, Timeout,
                           Cookie, Options),
    ?_assertEqual({error, timeout}, Response).

handle_lowcase_header(_Pid) ->
    Payload = ["<?xml version=\"1.0\"?>",
               "<methodCall>",
               "<methodName>echo</methodName>",
               "<params>",
               "<param><value><string>43</string></value></param>",
               "</params>",
               "</methodCall>"],
    Request = [ "POST / HTTP/1.1\r\n",
               "user-agent: Dart/1.8 (dart:io)\r\n",
               "content-type: text/xml; charset=utf-8\r\n",
               "content-length: ",integer_to_list(lists:flatlength(Payload)),"\r\n",
               "X-Forwarded-For: 192.46.45.211\r\n",
               "Connection: close\r\n",
               "\r\n",
               Payload],
    ParseResult = send_request(Request),
    ?_assertMatch({ok, _Header}, ParseResult).

handle_content_type_iso8859(_Pid) ->
    Payload = ["<?xml version=\"1.0\"?>",
               "<methodCall>",
               "<methodName>echo</methodName>",
               "<params>",
               "<param><value><string>écho</string></value></param>",
               "</params>",
               "</methodCall>"],
    Request = [ "POST / HTTP/1.1\r\n",
               "user-agent: Dart/1.8 (dart:io)\r\n",
               "content-type: text/xml; charset=iso-8859-1\r\n",
               "content-length: ",integer_to_list(lists:flatlength(Payload)),"\r\n",
               "Connection: close\r\n",
               "\r\n",
               Payload],
    ParseResult = send_request(Request),
    ?_assertMatch({ok, _Header}, ParseResult).

send_request(Request) ->
    Options = [{ssl, false}],
    case open_socket(localhost, 4567, Options) of
        {ok, Socket} ->
            gen_tcp:send(Socket, Request),
            case parse_response(Socket, 1000, Options) of
                {ok, Header} -> { ok, Header };
                {error, Reason } -> { error, Reason }
            end;
        {error, Reason} -> {error, Reason}
    end.

%% setup functions

%% start local XMLRPC echo server for tests
start() ->
    {ok, Pid} = xmlrpc:start_link({?MODULE, handler}),
    Pid.

stop(Pid) ->
    xmlrpc:stop(Pid).

handler(_, {call, echo, Params}) ->
    {false, {response, [{array, Params}]}};
handler(_, Payload) ->
    FaultString = lists:flatten(io_lib:format("Unknown call: ~p", [Payload])),
    {false, {response, {fault, -1, FaultString}}}.

