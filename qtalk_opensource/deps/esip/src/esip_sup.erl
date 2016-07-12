%%%----------------------------------------------------------------------
%%% File    : esip_sup.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Purpose : SIP supervisor
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

-module(esip_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
init([]) ->
    ESIP = {esip, {esip, start_link, []},
	    permanent, 2000, worker, [esip]},
    Listener = {esip_listener, {esip_listener, start_link, []},
		permanent, 2000, worker, [esip_listener]},
    Dialog =
        {esip_dialog, {esip_dialog, start_link, []},
	 permanent, 2000, worker, [esip_dialog]},
    Transaction =
	{esip_transaction, {esip_transaction, start_link, []},
	 permanent, 2000, worker, [esip_transaction]},
    Transport =
        {esip_transport, {esip_transport, start_link, []},
	 permanent, 2000, worker, [esip_transport]},
    ServerTransactionSup =
	{esip_server_transaction_sup,
	 {esip_tmp_sup, start_link,
	  [esip_server_transaction_sup, esip_server_transaction]},
	 permanent,
	 infinity,
	 supervisor,
	 [esip_tmp_sup]},
    ClientTransactionSup =
	{esip_client_transaction_sup,
	 {esip_tmp_sup, start_link,
	  [esip_client_transaction_sup, esip_client_transaction]},
	 permanent,
	 infinity,
	 supervisor,
	 [esip_tmp_sup]},
    TCPConnectionSup =
        {esip_tcp_sup,
         {esip_tmp_sup, start_link,
          [esip_tcp_sup, esip_socket]},
         permanent,
         infinity,
         supervisor,
         [esip_tmp_sup]},
    UDPConnectionSup =
        {esip_udp_sup,
         {esip_udp_sup, start_link, []},
         permanent,
         infinity,
         supervisor,
         [esip_udp_sup]},
    {ok,{{one_for_one,10,1},
	 [ESIP,
	  Listener,
	  Dialog,
	  ServerTransactionSup,
	  ClientTransactionSup,
	  Transaction,
          Transport,
          TCPConnectionSup,
          UDPConnectionSup]}}.

%%====================================================================
%% Internal functions
%%====================================================================
