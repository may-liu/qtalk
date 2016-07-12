%%%----------------------------------------------------------------------
%%% File    : p1_tls.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Interface to openssl
%%% Created : 24 Jul 2004 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% p1_tls, Copyright (C) 2002-2015   ProcessOne
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

-module(p1_tls).

-author('alexey@process-one.net').

-compile({no_auto_import, [{integer_to_binary, 1}]}).

-behaviour(gen_server).

-export([start_link/0, tcp_to_tls/2,
	 tls_to_tcp/1, send/2, recv/2, recv/3, recv_data/2,
	 setopts/2, sockname/1, peername/1,
	 controlling_process/2, close/1, get_peer_certificate/1,
	 get_verify_result/1, get_cert_verify_string/2, test/0]).

%% Internal exports, call-back functions.
-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, code_change/3, terminate/2]).

-define(SET_CERTIFICATE_FILE_ACCEPT, 1).

-define(SET_CERTIFICATE_FILE_CONNECT, 2).

-define(SET_ENCRYPTED_INPUT, 3).

-define(SET_DECRYPTED_OUTPUT, 4).

-define(GET_ENCRYPTED_OUTPUT, 5).

-define(GET_DECRYPTED_INPUT, 6).

-define(GET_PEER_CERTIFICATE, 7).

-define(GET_VERIFY_RESULT, 8).

-define(VERIFY_NONE, 16#10000).

-define(COMPRESSION_NONE, 16#100000).

-define(PRINT(Format, Args), io:format(Format, Args)).

-record(tlssock, {tcpsock :: inet:socket(),
                  tlsport :: port()}).

-type tls_socket() :: #tlssock{}.

-type cert() :: any(). %% TODO

-export_type([tls_socket/0]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [],
			  []).

init([]) ->
    case load_driver() of
        ok ->
            {ok, []};
        {error, Why} ->
            {stop, Why}
    end.

%%% --------------------------------------------------------
%%% The call-back functions.
%%% --------------------------------------------------------

handle_call(_, _, State) -> {noreply, State}.

handle_cast(_, State) -> {noreply, State}.

handle_info({'EXIT', Port, Reason}, Port) ->
    {stop, {port_died, Reason}, Port};
handle_info({'EXIT', _Pid, _Reason}, Port) ->
    {noreply, Port};
handle_info(_, State) -> {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) ->
    ok.

-spec tcp_to_tls(inet:socket(),
                 [{atom(), any()}]) -> {'error','no_certfile' | binary()} |
                                       {ok, tls_socket()}.

tcp_to_tls(TCPSocket, Options) ->
    case lists:keysearch(certfile, 1, Options) of
      {value, {certfile, CertFile}} ->
	  load_driver(),
	  Port = open_port({spawn, "p1_tls_drv"}, [binary]),
	  Flags1 = case lists:member(verify_none, Options) of
                       true -> ?VERIFY_NONE;
                       false -> 0
                   end,
          Flags2 = case lists:member(compression_none, Options) of
                       true -> ?COMPRESSION_NONE;
                       false -> 0
                   end,
          Flags = Flags1 bor Flags2,
	  Command = case lists:member(connect, Options) of
		      true -> ?SET_CERTIFICATE_FILE_CONNECT;
		      false -> ?SET_CERTIFICATE_FILE_ACCEPT
		    end,
          Ciphers =
                case lists:keysearch(ciphers, 1, Options) of
                    {value, {ciphers, C}} ->
                        iolist_to_binary(C);
                    false ->
                        <<>>
                end,
          ProtocolOpts = case lists:keysearch(protocol_options, 1, Options) of
                                 {value, {protocol_options, P}} ->
                                     iolist_to_binary(P);
                                 false ->
                                     <<>>
                             end,
          DHFile = case lists:keysearch(dhfile, 1, Options) of
                       {value, {dhfile, D}} ->
                           iolist_to_binary(D);
                       false ->
                           <<>>
                   end,
          CertFile1 = iolist_to_binary(CertFile),
	  case catch port_control(Port, Command bor Flags,
				  <<CertFile1/binary, 0, Ciphers/binary,
				    0, ProtocolOpts/binary, 0, DHFile/binary,
				    0>>)
	      of
	    {'EXIT', {badarg, _}} -> {error, einval};
	    <<0>> ->
		{ok, #tlssock{tcpsock = TCPSocket, tlsport = Port}};
	    <<1, Error/binary>> -> {error, (Error)}
	  end;
      false -> {error, no_certfile}
    end.

-spec tls_to_tcp(tls_socket()) -> inet:socket().

tls_to_tcp(#tlssock{tcpsock = TCPSocket,
		    tlsport = Port}) ->
    port_close(Port), TCPSocket.

recv(Socket, Length) -> recv(Socket, Length, infinity).

-spec recv(tls_socket(), non_neg_integer(),
           timeout()) -> {error, inet:posix()} |
                         {error, binary()} |
                         {ok, binary()}.

recv(#tlssock{tcpsock = TCPSocket} =
	 TLSSock,
     Length, Timeout) ->
    case recv_data(TLSSock, <<>>, Length) of
        {ok, <<>>} ->
            case gen_tcp:recv(TCPSocket, 0, Timeout) of
                {ok, Packet} -> recv_data(TLSSock, Packet, Length);
                {error, _Reason} = Error -> Error
            end;
        Res -> Res
    end.

-spec recv_data(tls_socket(), binary()) -> {error, inet:posix() | binary()} |
                                           {ok, binary()}.

recv_data(TLSSock, Packet) ->
    recv_data(TLSSock, Packet, 0).

-spec recv_data(tls_socket(), binary(),
                non_neg_integer()) -> {error, inet:posix() | binary()} |
                                      {ok, binary()}.

recv_data(TLSSock, Packet, Length) ->
    case catch recv_data1(TLSSock, Packet, Length) of
      {'EXIT', Reason} -> {error, Reason};
      Res -> Res
    end.

recv_data1(#tlssock{tcpsock = TCPSocket,
		    tlsport = Port},
	   Packet, Length) ->
    case catch port_control(Port, ?SET_ENCRYPTED_INPUT, Packet) of
      {'EXIT', {badarg, _}} -> {error, einval};
      <<0>> ->
	  case catch port_control(Port, ?GET_DECRYPTED_INPUT,
				  <<Length:32>>)
	      of
	    {'EXIT', {badarg, _}} -> {error, einval};
	    <<0, In/binary>> -> {ok, In};
	    <<2, In/binary>> ->
		case catch port_control(Port, ?GET_ENCRYPTED_OUTPUT, []) of
		  {'EXIT', {badarg, _}} -> {error, einval};
		  <<0, Out/binary>> ->
		      case gen_tcp:send(TCPSocket, Out) of
			ok -> {ok, In};
			Error -> Error
		      end;
		  <<1, Error/binary>> -> {error, (Error)}
		end;
	    <<1, Error/binary>> -> {error, (Error)}
	  end;
      <<1, Error/binary>> -> {error, (Error)}
    end.

-spec send(tls_socket(), binary()) -> ok | {error, inet:posix() |
                                            binary() | timeout}.

send(#tlssock{tcpsock = TCPSocket, tlsport = Port},
     Packet) ->
    case catch port_control(Port, ?SET_DECRYPTED_OUTPUT, Packet)
	of
      {'EXIT', {badarg, _}} -> {error, einval};
      <<0>> ->
	  case catch port_control(Port, ?GET_ENCRYPTED_OUTPUT, []) of
	    {'EXIT', {badarg, _}} -> {error, einval};
	    <<0, Out/binary>> -> gen_tcp:send(TCPSocket, Out);
	    <<1, Error/binary>> -> {error, (Error)}
	  end;
      <<1, Error/binary>> -> {error, Error}
    end.

-spec setopts(tls_socket(), list()) -> ok | {error, inet:posix()}.

setopts(#tlssock{tcpsock = TCPSocket}, Opts) ->
    inet:setopts(TCPSocket, Opts).

-spec sockname(tls_socket()) -> {ok, {inet:ip_address(), inet:port_number()}} |
                                {error, inet:posix()}.

sockname(#tlssock{tcpsock = TCPSocket}) ->
    inet:sockname(TCPSocket).

peername(#tlssock{tcpsock = TCPSocket}) ->
    inet:peername(TCPSocket).

controlling_process(#tlssock{tcpsock = TCPSocket},
		    Pid) ->
    gen_tcp:controlling_process(TCPSocket, Pid).

close(#tlssock{tcpsock = TCPSocket, tlsport = Port}) ->
    gen_tcp:close(TCPSocket), port_close(Port).

-spec get_peer_certificate(tls_socket()) -> error | {ok, cert()}.

get_peer_certificate(#tlssock{tlsport = Port}) ->
    case catch port_control(Port, ?GET_PEER_CERTIFICATE, []) of
      {'EXIT', {badarg, _}} -> error;
      <<0, BCert/binary>> ->
	  case catch public_key:pkix_decode_cert(BCert, plain)
	      of
	    {ok, Cert} -> {ok, Cert};
	    {'Certificate', _, _, _} = Cert -> {ok, Cert};
	    _ -> error
	  end;
      <<1>> -> error
    end.

-spec get_verify_result(tls_socket()) -> byte().

get_verify_result(#tlssock{tlsport = Port}) ->
    <<Res>> = port_control(Port, ?GET_VERIFY_RESULT, []),
    Res.

test() ->
    load_driver(),
    Port = open_port({spawn, "p1_tls_drv"}, [binary]),
    ?PRINT("open_port: ~p~n", [Port]),
    PCRes = port_control(Port, ?SET_CERTIFICATE_FILE_ACCEPT,
			 <<"./ssl.pem", 0>>),
    ?PRINT("port_control: ~p~n", [PCRes]),
    {ok, ListenSocket} = gen_tcp:listen(1234,
					[binary, {packet, 0}, {active, true},
					 {reuseaddr, true}, {nodelay, true}]),
    ?PRINT("listen: ~p~n", [ListenSocket]),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    ?PRINT("accept: ~p~n", [Socket]),
    loop(Port, Socket).

loop(Port, Socket) ->
    receive
      {tcp, Socket, Data} ->
	  Res = port_control(Port, ?SET_ENCRYPTED_INPUT, Data),
	  ?PRINT("SET_ENCRYPTED_INPUT: ~p~n", [Res]),
	  DIRes = port_control(Port, ?GET_DECRYPTED_INPUT, Data),
	  ?PRINT("GET_DECRYPTED_INPUT: ~p~n", [DIRes]),
	  case DIRes of
	    <<0, In/binary>> -> ?PRINT("input: ~s~n", [(In)]);
	    <<1, DIError/binary>> ->
		?PRINT("GET_DECRYPTED_INPUT error: ~p~n", [(DIError)])
	  end,
	  EORes = port_control(Port, ?GET_ENCRYPTED_OUTPUT, Data),
	  ?PRINT("GET_ENCRYPTED_OUTPUT: ~p~n", [EORes]),
	  case EORes of
	    <<0, Out/binary>> -> gen_tcp:send(Socket, Out);
	    <<1, EOError/binary>> ->
		?PRINT("GET_ENCRYPTED_OUTPUT error: ~p~n", [(EOError)])
	  end,
	  loop(Port, Socket);
      Msg ->
	  ?PRINT("receive: ~p~n", [Msg]), loop(Port, Socket)
    end.

-spec get_cert_verify_string(number(), cert()) -> binary().

get_cert_verify_string(CertVerifyRes, Cert) ->
    case catch cert_is_self_signed(Cert) of
      {'EXIT', _} -> <<"unknown verification error">>;
      IsSelfsigned ->
	  case {CertVerifyRes, IsSelfsigned} of
	    {21, true} -> <<"self-signed certificate">>;
	    _ -> cert_verify_code(CertVerifyRes)
	  end
    end.

cert_is_self_signed(Cert) ->
    BCert = public_key:pkix_encode('Certificate', Cert, plain),
    public_key:pkix_is_self_signed(BCert).

cert_verify_code(0) -> <<"ok">>;
cert_verify_code(2) ->
    <<"unable to get issuer certificate">>;
cert_verify_code(3) ->
    <<"unable to get certificate CRL">>;
cert_verify_code(4) ->
    <<"unable to decrypt certificate's signature">>;
cert_verify_code(5) ->
    <<"unable to decrypt CRL's signature">>;
cert_verify_code(6) ->
    <<"unable to decode issuer public key">>;
cert_verify_code(7) ->
    <<"certificate signature failure">>;
cert_verify_code(8) -> <<"CRL signature failure">>;
cert_verify_code(9) ->
    <<"certificate is not yet valid">>;
cert_verify_code(10) -> <<"certificate has expired">>;
cert_verify_code(11) -> <<"CRL is not yet valid">>;
cert_verify_code(12) -> <<"CRL has expired">>;
cert_verify_code(13) ->
    <<"format error in certificate's notBefore "
      "field">>;
cert_verify_code(14) ->
    <<"format error in certificate's notAfter "
      "field">>;
cert_verify_code(15) ->
    <<"format error in CRL's lastUpdate field">>;
cert_verify_code(16) ->
    <<"format error in CRL's nextUpdate field">>;
cert_verify_code(17) -> <<"out of memory">>;
cert_verify_code(18) -> <<"self signed certificate">>;
cert_verify_code(19) ->
    <<"self signed certificate in certificate "
      "chain">>;
cert_verify_code(20) ->
    <<"unable to get local issuer certificate">>;
cert_verify_code(21) ->
    <<"unable to verify the first certificate">>;
cert_verify_code(22) ->
    <<"certificate chain too long">>;
cert_verify_code(23) -> <<"certificate revoked">>;
cert_verify_code(24) -> <<"invalid CA certificate">>;
cert_verify_code(25) ->
    <<"path length constraint exceeded">>;
cert_verify_code(26) ->
    <<"unsupported certificate purpose">>;
cert_verify_code(27) -> <<"certificate not trusted">>;
cert_verify_code(28) -> <<"certificate rejected">>;
cert_verify_code(29) -> <<"subject issuer mismatch">>;
cert_verify_code(30) ->
    <<"authority and subject key identifier "
      "mismatch">>;
cert_verify_code(31) ->
    <<"authority and issuer serial number mismatch">>;
cert_verify_code(32) ->
    <<"key usage does not include certificate "
      "signing">>;
cert_verify_code(50) ->
    <<"application verification failure">>;
cert_verify_code(X) ->
    <<"Unknown OpenSSL error code: ", (integer_to_binary(X))/binary>>.

integer_to_binary(I) ->
    list_to_binary(integer_to_list(I)).

get_so_path() ->
    EbinDir = filename:dirname(code:which(?MODULE)),
    AppDir = filename:dirname(EbinDir),
    filename:join([AppDir, "priv", "lib"]).

load_driver() ->
    case erl_ddll:load_driver(get_so_path(), p1_tls_drv) of
        ok ->
            ok;
        {error, already_loaded} ->
            ok;
        {error, ErrorDesc} = Err ->
            error_logger:error_msg("failed to load TLS driver: ~s~n",
                                   [erl_ddll:format_error(ErrorDesc)]),
            Err
    end.
