/*
 * ejabberd, Copyright (C) 2002-2015   ProcessOne
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
 * 02111-1307 USA
 *
 */

#include <erl_nif.h>
#include <string.h>
#include <iconv.h>

#define OK 0
#define ERR_MEMORY_FAIL 1

static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    return 0;
}

static int do_convert(ErlNifEnv* env, char *from, char *to,
		      ErlNifBinary *string, ErlNifBinary *rstring)
{
    char *stmp = (char *) string->data;
    char *rtmp = (char *) rstring->data;
    size_t outleft = rstring->size;
    size_t inleft = string->size;
    int invalid_utf8_as_latin1 = 0;
    iconv_t cd;

    /* Special mode: parse as UTF-8 if possible; otherwise assume it's
      Latin-1.  Makes no difference when encoding. */
    if (strcmp(from, "utf-8+latin-1") == 0) {
	from[5] = '\0';
	invalid_utf8_as_latin1 = 1;
    }
    if (strcmp(to, "utf-8+latin-1") == 0) {
	to[5] = '\0';
    }
    cd = iconv_open(to, from);

    if (cd == (iconv_t) -1) {
	if (enif_realloc_binary(rstring, string->size)) {
	    memcpy(rstring->data, string->data, string->size);
	    return OK;
	} else {
	    return ERR_MEMORY_FAIL;
	}
    }

    while (inleft > 0) {
	if (iconv(cd, &stmp, &inleft, &rtmp, &outleft) == (size_t) -1) {
	    if (invalid_utf8_as_latin1 && (*stmp & 0x80) && outleft >= 2) {
		/* Encode one byte of (assumed) Latin-1 into two bytes of UTF-8 */
		*rtmp++ = 0xc0 | ((*stmp & 0xc0) >> 6);
		*rtmp++ = 0x80 | (*stmp & 0x3f);
		outleft -= 2;
	    }
	    stmp++;
	    inleft--;
	}
    }

    iconv_close(cd);

    if (enif_realloc_binary(rstring, rtmp - (char *) rstring->data)) {
	return OK;
    } else {
	return ERR_MEMORY_FAIL;
    } 
}

static ERL_NIF_TERM convert(ErlNifEnv* env, int argc,
			    const ERL_NIF_TERM argv[])
{

    ErlNifBinary from_bin, to_bin, string, rstring;
    char *from, *to;
    int rescode;

    if (argc == 3) {
	if (enif_inspect_iolist_as_binary(env, argv[0], &from_bin) &&
	    enif_inspect_iolist_as_binary(env, argv[1], &to_bin) &&
	    enif_inspect_iolist_as_binary(env, argv[2], &string))
	    {
		from = enif_alloc(from_bin.size + 1);
		to = enif_alloc(to_bin.size + 1);
		if (from && to && enif_alloc_binary(4*string.size, &rstring)) {
		    memcpy(from, from_bin.data, from_bin.size);
		    memcpy(to, to_bin.data, to_bin.size);
		    from[from_bin.size] = '\0';
		    to[to_bin.size] = '\0';
		    rescode = do_convert(env, from, to, &string, &rstring);
		    enif_free(from);
		    enif_free(to);
		    if (rescode == OK) {
			return enif_make_binary(env, &rstring);
		    } else {
			enif_release_binary(&rstring);
		    }
		}
	    }
    }
    
    return enif_make_badarg(env);
}

static ErlNifFunc nif_funcs[] =
    {
	{"convert", 3, convert}
    };

ERL_NIF_INIT(iconv, nif_funcs, load, NULL, NULL, NULL)
