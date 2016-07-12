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

-record(trid, {owner, type}).

-record(dialog, {secure,
                 route_set,
                 remote_target,
                 remote_seq_num,
                 local_seq_num,
                 'call-id',
                 local_tag,
                 remote_tag,
                 remote_uri,
                 local_uri,
                 state}).

-define(ERROR_MSG(Format, Args),
	error_logger:error_msg("(~p:~p:~p) " ++ Format,
			       [self(), ?MODULE, ?LINE | Args])).

-define(INFO_MSG(Format, Args),
	error_logger:info_msg("(~p:~p:~p) " ++ Format,
			       [self(), ?MODULE, ?LINE | Args])).
