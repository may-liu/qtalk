-module(ejb_http_server_sys_stat).

-include("logger.hrl").

-export([
        port_stat/0,
        msg_queue_stat/0,
        memory_stat/0
    ]).

-define(MB, (1024 * 1024)).

port_stat() ->
    UdpNum = length(recon:udp()),
    TcpNum = length(recon:tcp()),
    SctpNum = length(recon:sctp()),
    FileNum = length(recon:files()),
    PortNum = erlang:system_info(port_count),
    ProcessNum = erlang:system_info(process_count),

    [{<<"udp_num">>, UdpNum}, {<<"tcp_num">>, TcpNum}, {<<"sctp_num">>, SctpNum}, {<<"file_num">>, FileNum}, {<<"port_num">>, PortNum}, {<<"process_count">>, ProcessNum}].

msg_queue_stat() ->
    [{Pid, MsgNum, Attr}] = recon:proc_count(message_queue_len, 1),

    ?DEBUG("the process(~p) message_queue num is ~p, and the attr is ~p~n", [Pid, MsgNum, Attr]),
    [{<<"max_msg_queue">>, MsgNum}].

memory_stat() ->
    UsedNum = recon_alloc:memory(used) / ?MB,
    AllocatedNum = recon_alloc:memory(allocated) / ?MB,

    [{<<"memory_used">>, UsedNum}, {<<"memory_allocated">>, AllocatedNum}].
