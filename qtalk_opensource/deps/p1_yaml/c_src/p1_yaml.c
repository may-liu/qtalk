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

#include <yaml.h>
#include <erl_nif.h>
#include <stdlib.h>

#define OK 0
#define ERR_MEMORY_FAIL 1
#define PLAIN_AS_ATOM 1
#define INTEGER 1
#define FLOAT 2

typedef struct events_t {
    yaml_event_t *event;
    struct events_t *prev;
} events_t;

static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    return 0;
}

static ERL_NIF_TERM make_binary_size(ErlNifEnv* env,
				     const unsigned char *str,
				     size_t size)
{
    ErlNifBinary b;
    enif_alloc_binary(size, &b);

    if (str) memcpy(b.data, str, size);

    return enif_make_binary(env, &b);
}
	
static ERL_NIF_TERM make_binary(ErlNifEnv* env, const unsigned char *str)
{
    size_t size = 0;

    if (str) size = strlen((char *) str);
    return make_binary_size(env, str, size);
}

static int make_num(ErlNifEnv* env, const unsigned char *value,
		    size_t size, long int *i, double *d)
{
    int ret = 0;

    if (size>0) {
	char *buf = enif_alloc(size + 1);
	if (buf) {
	    memcpy(buf, value, size);
	    buf[size] = '\0';
	    char *check;
	    *i = strtol(buf, &check, 10);
	    if (*check == '\0')
		ret = INTEGER;
	    else if (*check == '.') {
		*d = strtod(buf, &check);
		if (*check == '\0')
		    ret = FLOAT;
	    }
	    enif_free(buf);
	}
    }

    return ret;
}

static ERL_NIF_TERM make_scalar(ErlNifEnv* env, yaml_event_t *event, int flags)
{
    int as_atom = PLAIN_AS_ATOM & flags;
    long int i;
    double d;
    int type;
    yaml_scalar_style_t style = event->data.scalar.style;
    ERL_NIF_TERM rterm;

    if (as_atom && style == YAML_SINGLE_QUOTED_SCALAR_STYLE) {
	rterm =  enif_make_atom_len(env,
				    (char *) event->data.scalar.value,
				    event->data.scalar.length);
    } else if (style == YAML_DOUBLE_QUOTED_SCALAR_STYLE) {
	rterm = make_binary_size(env, event->data.scalar.value,
				 event->data.scalar.length);
    } else if ((type = make_num(env, event->data.scalar.value, 
				event->data.scalar.length, &i, &d))) {
	if (type == INTEGER)
	    rterm = enif_make_long(env, i);
	else
	    rterm = enif_make_double(env, d);
    } else if (as_atom && style == YAML_PLAIN_SCALAR_STYLE) {
	rterm = enif_make_atom_len(env,
				   (char *) event->data.scalar.value,
				   event->data.scalar.length);
    } else {
	rterm = make_binary_size(env, event->data.scalar.value,
				 event->data.scalar.length);
    }

    return rterm;
}

static ERL_NIF_TERM make_alias(ErlNifEnv* env, yaml_event_t *event)
{
    return make_binary(env, event->data.alias.anchor);
}

static ERL_NIF_TERM zip(ErlNifEnv* env, ERL_NIF_TERM list)
{
    ERL_NIF_TERM key, val, tmp1, tmp2;

    if (enif_get_list_cell(env, list, &key, &tmp1)) {
	if (enif_get_list_cell(env, tmp1, &val, &tmp2)) {
	    return enif_make_list_cell(env,
				       enif_make_tuple2(env, key, val),
				       zip(env, tmp2));
	} else {
	    return enif_make_list_cell(env, key, enif_make_list(env, 0));
	}
    } else
	return list;
}

static ERL_NIF_TERM make_error(ErlNifEnv* env, yaml_parser_t *parser)
{
    ERL_NIF_TERM err;

    switch (parser->error) {
    case YAML_MEMORY_ERROR:
	err = enif_make_atom(env, "memory_error");
	break;
    case YAML_PARSER_ERROR:
	err = enif_make_tuple4(env,
			       enif_make_atom(env, "parser_error"),
			       make_binary(env, (const unsigned char*) parser->problem),
			       enif_make_uint(env, parser->problem_mark.line),
			       enif_make_uint(env, parser->problem_mark.column));
	break;
    case YAML_SCANNER_ERROR:
	err = enif_make_tuple4(env,
			       enif_make_atom(env, "scanner_error"),
			       make_binary(env, (const unsigned char*) parser->problem),
			       enif_make_uint(env, parser->problem_mark.line),
			       enif_make_uint(env, parser->problem_mark.column));
	break;
    default:
	err = enif_make_atom(env, "unexpected_error");
	break;
    }

    return enif_make_tuple2(env, enif_make_atom(env, "error"), err);
}

static yaml_event_t *hd(events_t **events)
{
    yaml_event_t *event = NULL;
    events_t *tmp;

    if (*events) {
	event = (*events)->event;
	tmp = *events;
	*events = (*events)->prev;
	enif_free(tmp);
    }

    return event;
}

static void free_events(events_t **events)
{
    yaml_event_t *event = NULL;

    if (events) {
	while (*events) {
	    event = hd(events);
	    if (event) {
		yaml_event_delete(event);
		enif_free(event);
	    }
	}
    }
}

static ERL_NIF_TERM process_events(ErlNifEnv* env, events_t **events,
				   yaml_parser_t *parser, int flags)
{
    ERL_NIF_TERM els, el;
    yaml_event_t *event;
    els = enif_make_list(env, 0);

    if (events) {
	while (*events) {
	    event = hd(events);
	    if (event) {
		switch (event->type) {
		case YAML_SEQUENCE_END_EVENT:
		    el = process_events(env, events, parser, flags);
		    els = enif_make_list_cell(env, el, els);
		    break;
		case YAML_MAPPING_END_EVENT:
		    el = process_events(env, events, parser, flags);
		    els = enif_make_list_cell(env, el, els);
		    break;
		case YAML_MAPPING_START_EVENT:
		    yaml_event_delete(event);
		    enif_free(event);
		    return zip(env, els);
		case YAML_SEQUENCE_START_EVENT:
		    yaml_event_delete(event);
		    enif_free(event);
		    return els;
		case YAML_SCALAR_EVENT:
		    el = make_scalar(env, event, flags);
		    els = enif_make_list_cell(env, el, els);
		    break;
		case YAML_ALIAS_EVENT:
		    el = make_alias(env, event);
		    els = enif_make_list_cell(env, el, els);
		    break;
		default:
		    break;
		}
		yaml_event_delete(event);
		enif_free(event);
	    } else {
		break;
	    }
	}
    }

    return els;
}

static ERL_NIF_TERM parse(ErlNifEnv* env, yaml_parser_t *parser,
			  int flags, unsigned char *data, int size)
{
    int result = 0, done = 0;
    yaml_event_t *event = NULL;
    events_t *prev_events = NULL;
    events_t *events = NULL;
    ERL_NIF_TERM rterm;

    yaml_parser_set_input_string(parser, data, size);

    do {
	event = enif_alloc(sizeof(yaml_event_t));
	result = yaml_parser_parse(parser, event);
	if (result) {
	    prev_events = events;
	    events = enif_alloc(sizeof(events_t *));
	    events->event = event;
	    events->prev = prev_events;
	    done = (event->type == YAML_STREAM_END_EVENT);
	} else {
	    enif_free(event);
	    done = 1;
	}
    } while (!done);
    
    if (result) {
	rterm = enif_make_tuple2(env, enif_make_atom(env, "ok"),
				 process_events(env, &events, parser, flags));
    } else {
	rterm = make_error(env, parser);
    }
    
    free_events(&events);
    return rterm;
}

static ERL_NIF_TERM decode(ErlNifEnv* env, int argc,
			   const ERL_NIF_TERM argv[])
{
    ErlNifBinary input;
    ERL_NIF_TERM result;
    unsigned int flags;
    yaml_parser_t parser;

    if (argc == 2) {
	if (enif_inspect_iolist_as_binary(env, argv[0], &input) &&
	    enif_get_uint(env, argv[1], &flags))
	    {
		yaml_parser_initialize(&parser);
		result = parse(env, &parser, flags, input.data, input.size);
		yaml_parser_delete(&parser);
		return result;
	    }
    }

    return enif_make_badarg(env);
}

static ErlNifFunc nif_funcs[] =
    {
	{"nif_decode", 2, decode}
    };

ERL_NIF_INIT(p1_yaml, nif_funcs, load, NULL, NULL, NULL)
