%%%----------------------------------------------------------------------
%%% File    : iconv.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Interface to libiconv
%%% Created : 16 Feb 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% eiconv, Copyright (C) 2002-2015   ProcessOne
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

-module(iconv).

-author('alexey@process-one.net').

-export([load_nif/0, load_nif/1, convert/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%===================================================================
%%% API functions
%%%===================================================================
load_nif() ->
    load_nif(get_so_path()).

load_nif(LibDir) ->
    SOPath = filename:join(LibDir, "iconv"),
    case catch erlang:load_nif(SOPath, 0) of
        ok ->
            ok;
        Err ->
            error_logger:warning_msg("unable to load iconv NIF: ~p~n", [Err]),
            Err
    end.

-spec convert(iodata(), iodata(), iodata()) -> binary().

convert(_From, _To, _String) ->
    erlang:nif_error(nif_not_loaded).

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_so_path() ->
    EbinDir = filename:dirname(code:which(?MODULE)),
    AppDir = filename:dirname(EbinDir),
    filename:join([AppDir, "priv", "lib"]).

%%%===================================================================
%%% Unit tests
%%%===================================================================
-ifdef(TEST).

load_nif_test() ->
    ?assertEqual(ok, load_nif(filename:join(["..", "priv", "lib"]))).

utf8_to_koi8r_test() ->
    ?assertEqual(
       <<212,197,211,212>>,
       iconv:convert("utf-8", "koi8-r", <<209,130,208,181,209,129,209,130>>)).

koi8r_to_cp1251_test() ->
    ?assertEqual(
       <<242,229,241,242>>,
       iconv:convert("koi8-r", "cp1251", <<212,197,211,212>>)).

wrong_encoding_test() ->
    ?assertEqual(
       <<1,2,3,4,5>>,
       iconv:convert("wrong_encoding_from",
                     "wrong_encoding_to",
                     <<1,2,3,4,5>>)).

-endif.
