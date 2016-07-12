%%%----------------------------------------------------------------------
%%% File    : esip_socket.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Purpose : 
%%% Created : 6 Jan 2011 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
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

-module(esip_socket).

-define(GEN_SERVER, p1_server).
-behaviour(?GEN_SERVER).

%% API
-export([start_link/0, start_link/1, start_link/2, start/0, start/2,
         connect/1, connect/2, send/2, socket_type/0, udp_recv/5, udp_init/2,
	 start_pool/0, get_pool_size/0, tcp_init/2, sockname/1, close/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("esip.hrl").
-include("esip_lib.hrl").
-include("stun.hrl").

-define(TCP_SEND_TIMEOUT, 15000).
-define(CONNECT_TIMEOUT, 20000).

-type addr() :: {inet:ip_address(), inet:port_number()}.

-record(state, {type = udp :: udp | tcp | tls,
		addr       :: addr(),
		peer       :: addr(),
		sock       :: port(),
		buf = <<>> :: binary(),
		max_size   :: non_neg_integer(),
		msg        :: #sip{},
		wait_size  :: non_neg_integer(),
		certfile   :: iodata()}).

%%%===================================================================
%%% API
%%%===================================================================
%% @doc Start TCP client to connect
start_link() ->
    ?GEN_SERVER:start_link(?MODULE, [], [{max_queue, 5000}]).

%% @doc Start UDP worker
start_link(I) ->
    ?GEN_SERVER:start_link({local, get_proc(I)}, ?MODULE, [],
			   [{max_queue, 5000}]).

%% @doc Start TCP acceptor
start_link(Sock, Opts) ->
    ?GEN_SERVER:start_link(?MODULE, [Sock, Opts], [{max_queue, 5000}]).

%% @doc Start TCP client to connect, the callback
start() ->
    supervisor:start_child(esip_tcp_sup, []).

socket_type() ->
    raw.

start({gen_tcp, Sock}, Opts) ->
    supervisor:start_child(esip_tcp_sup, [Sock, Opts]).

%% @doc TCP connect
connect(Addrs) ->
    connect(Addrs, []).

%% @doc first is UDP connect, second is TCP
connect([AddrPort|_Addrs], #sip_socket{} = Sock) ->
    {ok, Sock#sip_socket{peer = AddrPort}};
connect(Addrs, Opts) when is_list(Opts) ->
    case start() of
        {ok, Pid} ->
            ?GEN_SERVER:call(Pid, {connect, Addrs, Opts}, 60000);
        Err ->
            Err
    end.

send(#sip_socket{pid = Pid} = SIPSocket, Data) when node(Pid) /= node() ->
    Msg = {send, SIPSocket, Data},
    case erlang:send(Pid, Msg, [noconnect, nosuspend]) of
        nosuspend ->
            {error, closed};
        noconnect ->
            {error, closed};
        _ ->
            ok
    end;
send(#sip_socket{type = tls, sock = Sock}, Data) ->
    p1_tls:send(Sock, Data);
send(#sip_socket{type = tcp, sock = Sock}, Data) ->
    gen_tcp:send(Sock, Data);
send(#sip_socket{type = udp, sock = Sock, peer = {Addr, Port}}, Data) ->
    NewAddr = case Addr of
                  {A, B, C, D} ->
                      case inet:sockname(Sock) of
                          {ok, {{_, _, _, _, _, _, _, _}, _}} ->
                              {0, 0, 0, 0, 0, 16#ffff,
                               (A bsl 8) bor B, (C bsl 8) bor D};
                          _ ->
                              Addr
                      end;
                  {0, 0, 0, 0, 0, 16#ffff, X, Y} ->
                      case inet:sockname(Sock) of
                          {ok, {{_, _, _, _}, _}} ->
                              <<A, B, C, D>> = <<X:16, Y:16>>,
                              {A, B, C, D};
                          _ ->
                              Addr
                      end;
                  _ ->
                      Addr
              end,
    gen_udp:send(Sock, NewAddr, Port, Data).

close(#sip_socket{pid = Pid} = SIPSocket) when node(Pid) /= node() ->
    case erlang:send(Pid, {close, SIPSocket}, [noconnect, nosuspend]) of
        nosuspend -> {error, closed};
        noconnect -> {error, closed};
        _ -> ok
    end;
close(#sip_socket{type = tls, sock = Sock}) ->
    p1_tls:close(Sock);
close(#sip_socket{type = tcp, sock = Sock}) ->
    gen_tcp:close(Sock);
close(#sip_socket{type = udp, sock = Sock}) ->
    gen_udp:close(Sock).

tcp_init(ListenSock, Opts) ->
    {ok, {IP, Port}} = inet:sockname(ListenSock),
    ViaHost = get_via_host(IP),
    Transport = case proplists:get_bool(tls, Opts) of
		    false -> tcp;
		    true -> tls
		end,
    esip_transport:register_route(Transport, ViaHost, Port),
    Opts.

udp_init(Sock, Opts) ->
    {ok, {IP, Port}} = inet:sockname(Sock),
    ViaHost = get_via_host(IP),
    esip_transport:register_route(udp, ViaHost, Port),
    lists:foreach(
      fun(I) ->
	      Pid = get_proc(I),
	      Pid ! {init, Sock, self()}
      end, lists:seq(1, get_pool_size())),
    Opts.

udp_recv(Sock, Addr, Port, Data, Opts) ->
    Pid = get_proc_by_hash({Addr, Port}),
    Pid ! {udp, Sock, Addr, Port, Data},
    Opts.

start_pool() ->
    try
	lists:foreach(
	  fun(I) ->
		  Spec = {get_proc(I),
			  {?MODULE, start_link, [I]},
			  permanent, brutal_kill, worker,
			  [?MODULE]},
		  {ok, _} = supervisor:start_child(esip_udp_sup, Spec)
	  end, lists:seq(1, get_pool_size()))
    catch error:{badmatch, {error, _} = Err} ->
	    ?ERROR_MSG("failed to start UDP pool: ~p", [Err]),
	    Err
    end.

sockname(#sip_socket{type = tls, sock = Sock}) ->
    p1_tls:sockname(Sock);
sockname(#sip_socket{sock = Sock}) ->
    inet:sockname(Sock).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    MaxSize = esip:get_config_value(max_msg_size),
    {ok, #state{max_size = MaxSize}};
init([Sock, Opts]) ->
    case inet:peername(Sock) of
	{ok, PeerAddr} ->
	    case inet:sockname(Sock) of
		{ok, MyAddr} ->
		    Transport = get_transport(Opts),
		    CertFile = get_certfile(Opts),
		    case maybe_starttls(Sock, Transport, CertFile,
					{PeerAddr, MyAddr}, server) of
			{ok, NewSock} ->
			    inet:setopts(Sock, [{active, once}]),
			    MaxSize = esip:get_config_value(max_msg_size),
			    {ok, #state{max_size = MaxSize,
					sock = NewSock,
					peer = PeerAddr,
					addr = MyAddr,
					certfile = CertFile,
					type = Transport}};
			{error, Err} ->
			    {stop, Err}
		    end;
		{error, _Err} ->
		    {stop, normal}
	    end;
	{error, _Err} ->
	    {stop, normal}
    end.

handle_call({connect, Addrs, Opts}, _From, State) ->
    Type = get_transport(Opts),
    CertFile = get_certfile(Opts),
    case do_connect(Addrs, ?CONNECT_TIMEOUT div (length(Addrs) + 1)) of
        {ok, MyAddr, Peer, Sock} ->
	    case maybe_starttls(Sock, Type, CertFile, {MyAddr, Peer}, client) of
		{ok, NewSock} ->
		    inet:setopts(Sock, [{active, once}]),
		    NewState = State#state{type = Type, sock = NewSock,
					   certfile = CertFile,
					   addr = MyAddr, peer = Peer},
		    SIPSock = make_sip_socket(NewState),
		    esip_transport:register_socket(Peer, Type, SIPSock, CertFile),
		    {reply, {ok, SIPSock}, NewState};
		{error, _} = Err ->
		    {stop, normal, Err, State}
	    end;
	{error, _} = Err ->
	    {stop, normal, Err, State}
    end;
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({udp, UDPSock, IP, Port, Data}, State) ->
    SIPSock = #sip_socket{type = udp, sock = UDPSock,
                          addr = State#state.addr,
			  pid = self(),
			  peer = {IP, Port}},
    esip:callback(data_in, [Data, SIPSock]),
    datagram_transport_recv(SIPSock, Data),
    {noreply, State};
handle_info({init, UDPSock, Owner}, State) ->
    {ok, MyAddr} = inet:sockname(UDPSock),
    SIPSock = #sip_socket{type = udp, sock = UDPSock,
			  addr = MyAddr, pid = Owner},
    esip_transport:register_udp_listener(SIPSock),
    {noreply, State#state{addr = MyAddr}};
handle_info({tcp, Sock, Data}, #state{type = tcp} = State) ->
    inet:setopts(Sock, [{active, once}]),
    esip:callback(data_in, [Data, make_sip_socket(State)]),
    process_data(State, Data);
handle_info({tcp, _Sock, TLSData}, #state{type = tls} = State) ->
    p1_tls:setopts(State#state.sock, [{active, once}]),
    case p1_tls:recv_data(State#state.sock, TLSData) of
	{ok, Data} ->
	    esip:callback(data_in, [Data, make_sip_socket(State)]),
	    process_data(State, Data);
	_Err ->
	    {stop, normal, State}
    end;
handle_info({send, SIPSocket, Data}, State) ->
    send(SIPSocket, Data),
    {noreply, State};
handle_info({close, SIPSocket}, State) ->
    close(SIPSocket),
    {noreply, State};
handle_info({tcp_closed, _Sock}, State) ->
    {stop, normal, State};
handle_info({tcp_error, _Sock, _Reason}, State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{peer = Peer, type = Type,
			  certfile = CertFile} = State) ->
    SIPSock = make_sip_socket(State),
    esip_transport:unregister_socket(Peer, Type, SIPSock, CertFile).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_connect([{Addr, Port}|Addrs], ConnectTimeout) ->
    case gen_tcp:connect(Addr, Port, connect_opts(), ConnectTimeout) of
        {ok, Sock} ->
            case inet:sockname(Sock) of
                {error, _} = Err ->
                    fail_or_proceed(Err, Addrs, ConnectTimeout);
                {ok, MyAddr} ->
                    {ok, MyAddr, {Addr, Port}, Sock}
            end;
        Err ->
            fail_or_proceed(Err, Addrs, ConnectTimeout)
    end.

fail_or_proceed(Err, [], _ConnectTimeout) ->
    Err;
fail_or_proceed(_Err, Addrs, ConnectTimeout) ->
    do_connect(Addrs, ConnectTimeout).

make_sip_socket(#state{type = Type, addr = MyAddr, peer = Peer, sock = Sock}) ->
    #sip_socket{type = Type, addr = MyAddr,
		sock = Sock, peer = Peer, pid = self()}.

process_data(#state{buf = Buf, max_size = MaxSize,
                    msg = undefined} = State, Data) ->
    case process_crlf(<<Buf/binary, Data/binary>>, State) of
	{ok, NewBuf} ->
	    case catch esip_codec:decode(NewBuf, stream) of
		{ok, Msg, Tail} ->
		    case esip:get_hdr('content-length', Msg#sip.hdrs) of
			N when is_integer(N), N >= 0, N =< MaxSize ->
			    process_data(State#state{buf = <<>>,
						     msg = Msg,
						     wait_size = N}, Tail);
			_ ->
			    {stop, normal, State}
		    end;
		more when size(NewBuf) < MaxSize ->
		    {noreply, State#state{buf = NewBuf}};
		_ ->
		    {stop, normal, State}
	    end;
	{error, _} ->
	    {stop, normal, State}
    end;
process_data(#state{buf = Buf, max_size = MaxSize,
                    msg = Msg, wait_size = WaitSize} = State, Data) ->
    NewBuf = <<Buf/binary, Data/binary>>,
    case NewBuf of
        <<Body:WaitSize/binary, Tail/binary>> ->
            NewState = State#state{buf = Tail, msg = undefined},
            stream_transport_recv(NewState, Msg#sip{body = Body}),
            process_data(NewState, <<>>);
        _ when size(NewBuf) < MaxSize ->
            {noreply, State#state{buf = NewBuf}};
        _ ->
            {stop, normal, State}
    end.

stream_transport_recv(State, Msg) ->
    SIPSock = make_sip_socket(State),
    case catch esip_transport:recv(SIPSock, Msg) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("transport layer failed:~n"
                       "** Packet: ~p~n** Reason: ~p",
                       [Msg, Reason]);
        _ ->
            ok
    end.

process_crlf(<<"\r\n\r\n", Data/binary>>, State) ->
    SIPSock = make_sip_socket(State),
    SendRes = case esip:callback(message_in, [ping, SIPSock]) of
		  drop ->
		      ok;
		  pang ->
		      {error, closed};
		  _ ->
		      DataOut = <<"\r\n">>,
		      esip:callback(data_out, [DataOut, SIPSock]),
		      send(SIPSock, DataOut)
	      end,
    case SendRes of
	ok ->
	    process_crlf(Data, State);
	{error, Why} ->
	    {error, Why}
    end;
process_crlf(Data, _State) ->
    {ok, Data}.

datagram_transport_recv(SIPSock, <<_:32, ?STUN_MAGIC:32, _/binary>> = Data) ->
    case stun_codec:decode(Data, datagram) of
	{ok, Msg} ->
	    case esip:callback(message_in, [ping, SIPSock]) of
		drop ->
		    ok;
		pang ->
		    Resp = prepare_stun_response(Msg),
		    ErrMsg = Resp#stun{class = error,
				       'ERROR-CODE' = {400, <<"Flow Failed">>}},
		    send_stun_response(SIPSock, ErrMsg);
		_ ->
		    Resp = prepare_stun_response(Msg),
		    send_stun_response(SIPSock,
				       Resp#stun{'XOR-MAPPED-ADDRESS' =
						     SIPSock#sip_socket.peer})
	    end;
	_ ->
	    ok
    end;
datagram_transport_recv(SIPSock, Data) ->
    case catch esip_codec:decode(Data) of
        {ok, #sip{hdrs = Hdrs, body = Body} = Msg} ->
            case esip:get_hdr('content-length', Hdrs) of
                N when is_integer(N), N >= 0 ->
                    case Body of
                        <<_:N/binary>> ->
                            do_transport_recv(SIPSock, Msg);
                        <<Body1:N/binary, _/binary>> ->
                            do_transport_recv(SIPSock, Msg#sip{body = Body1});
                        _ ->
                            ok
                    end;
                _ ->
                    do_transport_recv(SIPSock, Msg)
            end;
	Err ->
	    Err
    end.

do_transport_recv(SIPSock, Msg) ->
    case catch esip_transport:recv(SIPSock, Msg) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("transport layer failed:~n"
                       "** Packet: ~p~n** Reason: ~p",
                       [Msg, Reason]);
        _ ->
            ok
    end.

connect_opts() ->
    [binary,
     {active, false},
     {packet, 0},
     {send_timeout, ?TCP_SEND_TIMEOUT},
     {send_timeout_close, true}].

get_pool_size() ->
    100.

get_proc_by_hash(Source) ->
    N = erlang:phash2(Source, get_pool_size()) + 1,
    get_proc(N).

get_proc(N) ->
    list_to_atom("esip_udp_" ++ integer_to_list(N)).

get_via_host(IP) ->
    case lists:sum(tuple_to_list(IP)) of
	0 ->
	    undefined;
	_ ->
	    iolist_to_binary(inet_parse:ntoa(IP))
    end.

get_transport(Opts) ->
    case proplists:get_bool(tls, Opts) of
	false -> tcp;
	true -> tls
    end.

get_certfile(Opts) ->
    case catch iolist_to_binary(proplists:get_value(certfile, Opts)) of
	Filename when is_binary(Filename), Filename /= <<"">> ->
	    Filename;
	_ ->
	    undefined
    end.

maybe_starttls(_Sock, tls, undefined, FromTo, server) ->
    {{FromIP, FromPort}, {ToIP, ToPort}} = FromTo,
    ?ERROR_MSG("failed to start TLS connection ~s:~p -> ~s:~p: "
	       "option 'certfile' is not set",
	       [inet_parse:ntoa(FromIP), FromPort,
		inet_parse:ntoa(ToIP), ToPort]),
    {error, eprotonosupport};
maybe_starttls(Sock, tls, CertFile, _FromTo, Role) ->
    Opts1 = case Role of
		client -> [connect];
		server -> []
	    end,
    Opts2 = case CertFile of
		undefined -> Opts1;
		_ -> [{certfile, CertFile}|Opts1]
	    end,
    case p1_tls:tcp_to_tls(Sock, Opts2) of
	{ok, NewSock} when Role == client ->
	    case p1_tls:recv_data(NewSock, <<"">>) of
		{ok, <<"">>} ->
		    {ok, NewSock};
		{error, _} = Err ->
		    Err
	    end;
	{ok, NewSock} when Role == server ->
	    {ok, NewSock};
	{error, _} = Err ->
	    Err
    end;
maybe_starttls(Sock, _Transport, _CertFile, _FromTo, _Role) ->
    {ok, Sock}.

prepare_stun_response(Msg) ->
    Software = esip:get_config_value(software),
    #stun{method = Msg#stun.method,
	  magic = Msg#stun.magic,
	  trid = Msg#stun.trid,
	  'SOFTWARE' = Software}.

send_stun_response(SIPSock, Msg) ->
    DataOut = stun_codec:encode(Msg),
    esip:callback(data_out, [DataOut, SIPSock]),
    send(SIPSock, DataOut).
