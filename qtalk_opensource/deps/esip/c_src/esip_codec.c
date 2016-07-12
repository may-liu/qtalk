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
#include <stdio.h>
#include <ctype.h>

#ifdef NIF_OLD
#define ENIF_ALLOC(SIZE) enif_alloc(env, SIZE)
#define ENIF_FREE(PTR) enif_free(env, PTR)
#define ENIF_REALLOC(PTR, SIZE) enif_realloc(env, PTR, SIZE)
#define ENIF_ALLOC_BINARY(SIZE, BIN) enif_alloc_binary(env, SIZE, BIN)
#define ENIF_COMPARE(TERM1, TERM2) enif_compare(env, TERM1, TERM2)
#else
#define ENIF_ALLOC(SIZE) enif_alloc(SIZE)
#define ENIF_FREE(PTR) enif_free(PTR)
#define ENIF_REALLOC(PTR, SIZE) enif_realloc(PTR, SIZE)
#define ENIF_ALLOC_BINARY(SIZE, BIN) enif_alloc_binary(SIZE, BIN)
#define ENIF_COMPARE(TERM1, TERM2) enif_compare(TERM1, TERM2)
#endif

#define BUF_LIMIT 64
#define WSP 256
#define OUT 0
#define IN 1

static ERL_NIF_TERM atom_wsp;
static ERL_NIF_TERM atom_true;
static ERL_NIF_TERM atom_false;

struct buf {
  int limit;
  int len;
  unsigned char *b;
};

struct list {
  ERL_NIF_TERM term;
  struct list *next;
};

static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
  atom_wsp = enif_make_atom(env, "wsp");
  atom_true = enif_make_atom(env, "true");
  atom_false = enif_make_atom(env, "false");
  return 0;
}

static struct buf *init_buf(ErlNifEnv* env)
{
  struct buf *rbuf = ENIF_ALLOC(sizeof(struct buf));
  rbuf->limit = BUF_LIMIT;
  rbuf->len = 0;
  rbuf->b = ENIF_ALLOC(rbuf->limit);
  return rbuf;
}

static void destroy_buf(ErlNifEnv* env, struct buf *rbuf)
{
  if (rbuf) {
    if (rbuf->b) {
      ENIF_FREE(rbuf->b);
    };
    ENIF_FREE(rbuf);
  };
}

inline void resize_buf(ErlNifEnv* env, struct buf *rbuf, int len_to_add)
{
  int new_len = rbuf->len + len_to_add;

  if (new_len >= rbuf->limit) {
    rbuf->limit = ((new_len / BUF_LIMIT) + 1) * BUF_LIMIT;
    rbuf->b = ENIF_REALLOC(rbuf->b, rbuf->limit);
  };
}

static void buf_add_char(ErlNifEnv* env, struct buf *rbuf, unsigned char c)
{
  resize_buf(env, rbuf, 1);
  (rbuf->b)[rbuf->len] = c;
  rbuf->len += 1;
}

static void buf_add_str(ErlNifEnv* env, struct buf *rbuf, char *data, int len)
{
  resize_buf(env, rbuf, len);
  memcpy(rbuf->b + rbuf->len, data, len);
  rbuf->len += len;
}

static ERL_NIF_TERM to_lower(ErlNifEnv* env, int argc,
			     const ERL_NIF_TERM argv[])
{
  ErlNifBinary input, output;
  int i;
  
  if (argc == 1) {
    if (enif_inspect_iolist_as_binary(env, argv[0], &input)) {
      if (ENIF_ALLOC_BINARY(input.size, &output)) {
	if (input.size > 0) {
	  for (i=0; i<input.size; i++) {
	    output.data[i] = tolower(input.data[i]);
	  };
	};
	return enif_make_binary(env, &output);
      };
    };
  };
  
  return enif_make_badarg(env);
}

static ERL_NIF_TERM to_upper(ErlNifEnv* env, int argc,
			     const ERL_NIF_TERM argv[])
{
  ErlNifBinary input, output;
  int i;
  
  if (argc == 1) {
    if (enif_inspect_iolist_as_binary(env, argv[0], &input)) {
      if (ENIF_ALLOC_BINARY(input.size, &output)) {
	if (input.size > 0) {
	  for (i=0; i<input.size; i++) {
	    output.data[i] = toupper(input.data[i]);
	  };
	};
	return enif_make_binary(env, &output);
      };
    };
  };
  
  return enif_make_badarg(env);
}

static ERL_NIF_TERM reverse(ErlNifEnv* env, int argc,
			    const ERL_NIF_TERM argv[])
{
  ErlNifBinary input, output;
  int i;
  
  if (argc == 1) {
    if (enif_inspect_iolist_as_binary(env, argv[0], &input)) {
      if (ENIF_ALLOC_BINARY(input.size, &output)) {
	if (input.size > 0) {
	  for (i=0; i<input.size; i++) {
	    output.data[input.size-i-1] = input.data[i];
	  };
	};
	return enif_make_binary(env, &output);
      };
    };
  };

  return enif_make_badarg(env);
}

static ERL_NIF_TERM strip_wsp_left(ErlNifEnv* env, int argc,
				   const ERL_NIF_TERM argv[])
{
  ErlNifBinary input, output;
  int i = 0;
  unsigned char c;
  
  if (argc == 1) {
    if (enif_inspect_iolist_as_binary(env, argv[0], &input)) {
      if (input.size > 0) {
	while (i<input.size) {
	  c = input.data[i];
	  if (!isspace(c)) break;
	  i++;
	};
	if (ENIF_ALLOC_BINARY(input.size - i, &output)) {
	  memcpy(output.data, input.data + i, input.size - i);
	  return enif_make_binary(env, &output);
	};
      } else {
	return enif_make_binary(env, &input);
      };
    };
  };

  return enif_make_badarg(env);
}

static ERL_NIF_TERM strip_wsp_right(ErlNifEnv* env, int argc,
				    const ERL_NIF_TERM argv[])
{
  ErlNifBinary input, output;
  int i;
  unsigned char c;
  
  if (argc == 1) {
    if (enif_inspect_iolist_as_binary(env, argv[0], &input)) {
      if (input.size > 0) {
	i = input.size - 1;
	while (i>=0) {
	  c = input.data[i];
	  if (!isspace(c)) break;
	  i--;
	};
	if (ENIF_ALLOC_BINARY(i+1, &output)) {
	  memcpy(output.data, input.data, i+1);
	  return enif_make_binary(env, &output);
	};
      } else {
	return enif_make_binary(env, &input);
      };
    };
  };
  
  return enif_make_badarg(env);
}

static ERL_NIF_TERM strip_wsp(ErlNifEnv* env, int argc,
			      const ERL_NIF_TERM argv[])
{
  ErlNifBinary input, output;
  int start = 0, end;
  unsigned char c;

  if (argc == 1) {
    if (enif_inspect_iolist_as_binary(env, argv[0], &input)) {
      while (start < input.size) {
	c = input.data[start];
	if (!isspace(c)) break;
	start++;
      };
      end = input.size - 1;
      while (end >= start) {
	c = input.data[end];
	if (!isspace(c)) break;
	end--;
      };
      if (ENIF_ALLOC_BINARY(end - start + 1, &output)) {
	memcpy(output.data, input.data + start, end - start + 1);
	return enif_make_binary(env, &output);
      };
    };
  };

  return enif_make_badarg(env);
}

static ERL_NIF_TERM str(ErlNifEnv* env, int argc,
			const ERL_NIF_TERM argv[])
{
  ErlNifBinary input, sep;
  unsigned i = 0, j;

  if (argc == 2) {
    if (enif_inspect_iolist_as_binary(env, argv[0], &input) &&
	enif_inspect_iolist_as_binary(env, argv[1], &sep)) {
      if (sep.size > 0) {
	while (i<input.size && input.size-i >= sep.size) {
	  j = 0;
	  while (j<sep.size && input.data[i+j] == sep.data[j]) j++;
	  if (j == sep.size) return enif_make_int(env, i);
	  i++;
	};
	return enif_make_atom(env, "nomatch");
      };
      return enif_make_int(env, 0);
    };
  };
  
  return enif_make_badarg(env);
}

static ERL_NIF_TERM strcasecmp_erl(ErlNifEnv* env, int argc,
				   const ERL_NIF_TERM argv[])
{
    ErlNifBinary b1, b2;

    if (argc == 2) {
	if (enif_inspect_iolist_as_binary(env, argv[0], &b1) &&
	    enif_inspect_iolist_as_binary(env, argv[1], &b2)) {
	    if (b1.size == b2.size) {
		if (!strncasecmp((char *)b1.data, (char *)b2.data, b1.size))
		    return atom_true;
		else
		    return atom_false;
	    } else
		return atom_false;
	}
    }

    return enif_make_badarg(env);
}

inline struct list *add_to_acc(ErlNifEnv* env, struct buf *buf,
			       struct list *acc, unsigned chr)
{
  int start = 0, end;
  unsigned char c;
  ErlNifBinary output;
  
  while (start < buf->len) {
    c = buf->b[start];
    if (!isspace(c)) break;
    start++;
  };
  end = buf->len - 1;
  while (end >= start) {
    c = buf->b[end];
    if (!isspace(c)) break;
    end--;
  };

  if (end < start && chr == WSP) {
    destroy_buf(env, buf);
    return acc;
  } else {
    struct list *new_acc = ENIF_ALLOC(sizeof(struct list));
    ENIF_ALLOC_BINARY(end - start + 1, &output);
    memcpy(output.data, buf->b + start, end - start + 1);
    destroy_buf(env, buf);
    new_acc->next = acc;
    new_acc->term = enif_make_binary(env, &output);
    return new_acc;
  };
}

static ERL_NIF_TERM do_split(ErlNifEnv* env, ErlNifBinary *input,
			     unsigned pos, unsigned chr,
			     struct buf *buf, struct list *acc,
			     unsigned state, unsigned prev_chr,
			     signed iter)
{
  struct list *new_acc;
  unsigned c;
  ERL_NIF_TERM result;

  if (pos < input->size && iter != 0) {
    c = input->data[pos];
    if (state == IN) {
      /* We are in quoted string here */
      buf_add_char(env, buf, c);
      if (c == '"' && prev_chr != '\\') {
	/* Leaving quoted string */
	return do_split(env, input, pos+1, chr, buf, acc, OUT, c, iter);
      } else {
	/* Ignore any other characters */
	return do_split(env, input, pos+1, chr, buf, acc, IN, c, iter);
      };
    } else {
      if (c == '"') {
	/* Entering quoted string */
	buf_add_char(env, buf, c);
	return do_split(env, input, pos+1, chr, buf, acc, IN, c, iter);
      };
      if (c == chr || (chr == WSP && (isspace(c)))) {
	acc = add_to_acc(env, buf, acc, chr);
	return do_split(env, input, pos+1, chr, init_buf(env), acc, OUT, c, iter-1);
      };
      buf_add_char(env, buf, c);
      return do_split(env, input, pos+1, chr, buf, acc, OUT, c, iter);
    };
  } else {
    if (state == IN) {
      destroy_buf(env, buf);
    } else {
      if (iter == 0) {
	buf_add_str(env, buf, (char *)input->data + pos, input->size - pos);
      };
      acc = add_to_acc(env, buf, acc, chr);
    };
    result = enif_make_list(env, 0, NULL);
    while (acc) {
      result = enif_make_list_cell(env, acc->term, result);
      new_acc = acc->next;
      ENIF_FREE(acc);
      acc = new_acc;
    };
    return result;
  };
}

static ERL_NIF_TERM split(ErlNifEnv* env, int argc,
			  const ERL_NIF_TERM argv[])
{
  ErlNifBinary input;
  unsigned chr;
  signed iter;

  if (argc == 3) {
    if (enif_inspect_iolist_as_binary(env, argv[0], &input) &&
	enif_get_int(env, argv[2], &iter)) {
      if (enif_get_uint(env, argv[1], &chr)) {
	if (chr >= 0 && chr <= 255) {
	  return do_split(env, &input, 0, chr, init_buf(env), NULL, OUT, 0, iter);
	};
      } else {
	if (!ENIF_COMPARE(argv[1], atom_wsp)) {
	  chr = WSP;
	  return do_split(env, &input, 0, chr, init_buf(env), NULL, OUT, 0, iter);
	};
      };
    };
  };
  
  return enif_make_badarg(env);
}

static ErlNifFunc nif_funcs[] =
  {
    {"to_lower", 1, to_lower},
    {"to_upper", 1, to_upper},
    {"reverse", 1, reverse},
    {"strip_wsp", 1, strip_wsp},
    {"strip_wsp_left", 1, strip_wsp_left},
    {"strip_wsp_right", 1, strip_wsp_right},
    {"str", 2, str},
    {"strcasecmp", 2, strcasecmp_erl},
    {"split", 3, split}
  };

ERL_NIF_INIT(esip_codec, nif_funcs, load, NULL, NULL, NULL)
