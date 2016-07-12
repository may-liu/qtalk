%%%----------------------------------------------------------------------
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

-record(sip, {type,
              version = {2,0},
              method,
              hdrs = [],
              body = <<>>,
              uri,
              status,
              reason}).

-record(uri, {scheme = <<"sip">>,
              user = <<>>,
              password = <<>>,
              host = <<>>,
              port = undefined,
              params = [],
              hdrs = []}).

-record(via, {proto = <<"SIP">>,
              version = {2,0},
              transport,
              host,
              port = undefined,
              params = []}).

-record(dialog_id, {'call-id', remote_tag, local_tag}).

-record(sip_socket, {type :: udp | tcp | tls,
		     sock :: inet:socket() | p1_tls:tls_socket(),
		     addr :: {inet:ip_address(), inet:port_number()},
		     peer :: {inet:ip_address(), inet:port_number()},
		     pid  :: pid()}).
