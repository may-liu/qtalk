%%%----------------------------------------------------------------------
%%% File    : p1_sha.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose :
%%% Created : 20 Dec 2002 by Alexey Shchepin <alexey@process-one.net>
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

-module(p1_sha).

-author('alexey@process-one.net').

-export([load_nif/0, load_nif/1,
         sha/1, sha1/1, sha224/1, sha256/1,
         sha384/1, sha512/1, to_hexlist/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%===================================================================
%%% API functions
%%%===================================================================
load_nif() ->
    load_nif(get_so_path()).

load_nif(LibDir) ->
    SOPath = filename:join(LibDir, "p1_sha"),
    case catch erlang:load_nif(SOPath, 0) of
        ok ->
            ok;
        Err ->
            error_logger:warning_msg("unable to load sha NIF: ~p~n", [Err]),
            Err
    end.

-spec to_hexlist(iodata()) -> binary().
-spec sha(iodata()) -> binary().
-spec sha1(iodata()) -> binary().
-spec sha224(iodata()) -> binary().
-spec sha256(iodata()) -> binary().
-spec sha384(iodata()) -> binary().
-spec sha512(iodata()) -> binary().

to_hexlist(_Text) ->
    erlang:nif_error(nif_not_loaded).

sha(Text) ->
    to_hexlist(sha1(Text)).

sha1(_Text) ->
    erlang:nif_error(nif_not_loaded).

sha224(_Text) ->
    erlang:nif_error(nif_not_loaded).

sha256(_Text) ->
    erlang:nif_error(nif_not_loaded).

sha384(_Text) ->
    erlang:nif_error(nif_not_loaded).

sha512(_Text) ->
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

sha1_test() ->
    ?assertEqual(
       <<169,74,143,229,204,177,155,166,28,76,8,115,211,145,233,
         135,152,47,187,211>>,
       sha1("test")).

sha224_test() ->
    ?assertEqual(
       <<144,163,237,158,50,178,170,244,198,28,65,14,185,37,66,
         97,25,225,169,220,83,212,40,106,222,153,168,9>>,
       sha224("test")).

sha256_test() ->
    ?assertEqual(
       <<159,134,208,129,136,76,125,101,154,47,234,160,197,90,208,
         21,163,191,79,27,43,11,130,44,209,93,108,21,176,240,10,8>>,
       sha256("test")).

sha384_test() ->
    ?assertEqual(
       <<118,132,18,50,15,123,10,165,129,47,206,66,141,196,112,107,
         60,174,80,224,42,100,202,161,106,120,34,73,191,232,239,196,
         183,239,28,203,18,98,85,209,150,4,125,254,223,23,160,169>>,
       sha384("test")).

sha512_test() ->
    ?assertEqual(
       <<238,38,176,221,74,247,231,73,170,26,142,227,193,10,233,146,
         63,97,137,128,119,46,71,63,136,25,165,212,148,14,13,178,122,
         193,133,248,160,225,213,248,79,136,188,136,127,214,123,20,55,
         50,195,4,204,95,169,173,142,111,87,245,0,40,168,255>>,
       sha512("test")).

to_hexlist_test() ->
    ?assertEqual(
       <<"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122"
         "232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f404142434445"
         "464748494a4b4c4d4e4f505152535455565758595a5b5c5d5e5f606162636465666768"
         "696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f808182838485868788898a8b"
         "8c8d8e8f909192939495969798999a9b9c9d9e9fa0a1a2a3a4a5a6a7a8a9aaabacadae"
         "afb0b1b2b3b4b5b6b7b8b9babbbcbdbebfc0c1c2c3c4c5c6c7c8c9cacbcccdcecfd0d1"
         "d2d3d4d5d6d7d8d9dadbdcdddedfe0e1e2e3e4e5e6e7e8e9eaebecedeeeff0f1f2f3f4"
         "f5f6f7f8f9fafbfcfdfeff">>,
       to_hexlist(lists:seq(0, 255))).

-endif.
