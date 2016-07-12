%%%----------------------------------------------------------------------
%%% File    : esip_tmp_sup.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Purpose : A pattern for simple_one_for_one supervisor
%%% Created : 15 Jul 2009 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
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

-module(esip_tmp_sup).

%% API
-export([start_link/2, init/1, start_child/4]).

%%====================================================================
%% API
%%====================================================================
start_link(Name, Module) ->
    supervisor:start_link({local, Name}, ?MODULE, Module).

-ifndef(NO_TMP_SUP).
start_child(Supervisor, _Module, _Behaviour, Opts) ->
    supervisor:start_child(Supervisor, Opts).
-else.
start_child(_Supervisor, Module, Behaviour, Opts) ->
    Behaviour:start(Module, Opts, []).
-endif.

init(Module) ->
    {ok, {{simple_one_for_one, 10, 1},
	  [{undefined, {Module, start_link, []},
	    temporary, brutal_kill, worker, [Module]}]}}.

%%====================================================================
%% Internal functions
%%====================================================================
