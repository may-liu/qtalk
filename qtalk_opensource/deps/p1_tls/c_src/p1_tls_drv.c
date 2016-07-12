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

#include <stdio.h>
#include <string.h>
#include <erl_driver.h>
#include <openssl/err.h>
#include <openssl/ssl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdint.h>
#include "options.h"

#define BUF_SIZE 1024

typedef struct {
      ErlDrvPort port;
      BIO *bio_read;
      BIO *bio_write;
      SSL *ssl;
      int handshakes;
      char *send_buffer;
      int send_buffer_size;
      int send_buffer_len;
      char *send_buffer2;
      int send_buffer2_size;
      int send_buffer2_len;
} tls_data;

static int ssl_index;

#ifdef _WIN32
typedef unsigned __int32 uint32_t;
#endif

#ifndef SSL_OP_NO_TICKET
#define SSL_OP_NO_TICKET 0
#endif

#define CIPHERS "DEFAULT:!EXPORT:!LOW:!RC4:!SSLv2"

/**
 * Prepare the SSL options flag.
 **/
static int set_option_flag(const char *opt, long *flag)
{
    ssl_option_t *p;
    for (p = ssl_options; p->name; p++) {
        if (!strcmp(opt, p->name)) {
            *flag |= p->code;
            return 1;
        }
    }
    return 0;
}

/*
 * R15B changed several driver callbacks to use ErlDrvSizeT and
 * ErlDrvSSizeT typedefs instead of int.
 * This provides missing typedefs on older OTP versions.
 */
#if ERL_DRV_EXTENDED_MAJOR_VERSION < 2
typedef int ErlDrvSizeT;
typedef int ErlDrvSSizeT;
#endif

/*
 * str_hash is based on the public domain code from
 * http://www.burtleburtle.net/bob/hash/doobs.html
 */
static uint32_t str_hash(char *s)
{
   unsigned char *key = (unsigned char *)s;
   uint32_t hash = 0;
   size_t i;

   for (i = 0; key[i] != 0; i++) {
      hash += key[i];
      hash += (hash << 10);
      hash ^= (hash >> 6);
   }
   hash += (hash << 3);
   hash ^= (hash >> 11);
   hash += (hash << 15);
   return hash;
}

/* Linear hashing */

#define MIN_LEVEL 8
#define MAX_LEVEL 20

struct bucket {
      uint32_t hash;
      char *key;
      time_t key_mtime;
      time_t dh_mtime;
      SSL_CTX *ssl_ctx;
      struct bucket *next;
};

struct hash_table {
      int split;
      int level;
      struct bucket **buckets;
      int size;
};

struct hash_table ht;

static void init_hash_table()
{
   size_t size = 1 << (MIN_LEVEL + 1);
   size_t i;
   ht.buckets = (struct bucket **)driver_alloc(sizeof(struct bucket *) * size);
   ht.split = 0;
   ht.level = MIN_LEVEL;
   for (i = 0; i < size; i++)
      ht.buckets[i] = NULL;
   
}

static void hash_table_insert(char *key, time_t key_mtime, time_t dh_mtime,
			      SSL_CTX *ssl_ctx)
{
   int level, split;
   uint32_t hash = str_hash(key);
   size_t bucket;
   int do_split = 0;
   struct bucket *el;
   struct bucket *new_bucket_el;

   split = ht.split;
   level = ht.level;

   bucket = hash & ((1 << level) - 1);
   if (bucket < split)
      bucket = hash & ((1 << (level + 1)) - 1);

   el = ht.buckets[bucket];
   while (el != NULL) {
      if (el->hash == hash && strcmp(el->key, key) == 0) {
	 el->key_mtime = key_mtime;
	 el->dh_mtime = dh_mtime;
	 if (el->ssl_ctx != NULL)
	    SSL_CTX_free(el->ssl_ctx);
	 el->ssl_ctx = ssl_ctx;
	 break;
      }
      el = el->next;
   }

   if (el == NULL) {
      if (ht.buckets[bucket] != NULL)
	 do_split = !0;

      new_bucket_el = (struct bucket *)driver_alloc(sizeof(struct bucket));
      new_bucket_el->hash = hash;
      new_bucket_el->key = (char *)driver_alloc(strlen(key) + 1);
      strcpy(new_bucket_el->key, key);
      new_bucket_el->key_mtime = key_mtime;
      new_bucket_el->dh_mtime = dh_mtime;
      new_bucket_el->ssl_ctx = ssl_ctx;
      new_bucket_el->next = ht.buckets[bucket];
      ht.buckets[bucket] = new_bucket_el;
   }

   if (do_split) {
      struct bucket **el_ptr = &ht.buckets[split];
      size_t new_bucket = split + (1 << level);
      while (*el_ptr != NULL) {
	 uint32_t hash = (*el_ptr)->hash;
	 if ((hash & ((1 << (level + 1)) - 1)) == new_bucket) {
	    struct bucket *moved_el = *el_ptr;
	    *el_ptr = (*el_ptr)->next;
	    moved_el->next = ht.buckets[new_bucket];
	    ht.buckets[new_bucket] = moved_el;
	 } else
	    el_ptr = &(*el_ptr)->next;
      }
      split++;
      if (split == 1 << level) {
	 size_t size;
	 size_t i;
	 split = 0;
	 level++;
	 size = 1 << (level + 1);
	 ht.split = split;
	 ht.level = level;
	 ht.buckets = (struct bucket **)
	    driver_realloc(ht.buckets, sizeof(struct bucket *) * size);
	 for (i = 1 << level; i < size; i++)
	    ht.buckets[i] = NULL;
      } else
	 ht.split = split;
   }
}

static SSL_CTX *hash_table_lookup(char *key, time_t *key_mtime,
				  time_t *dh_mtime)
{
   int level, split;
   uint32_t hash = str_hash(key);
   size_t bucket;
   struct bucket *el;

   split = ht.split;
   level = ht.level;

   bucket = hash & ((1 << level) - 1);
   if (bucket < split)
      bucket = hash & ((1 << (level + 1)) - 1);

   el = ht.buckets[bucket];
   while (el != NULL) {
      if (el->hash == hash && strcmp(el->key, key) == 0) {
	 *key_mtime = el->key_mtime;
	 *dh_mtime = el->dh_mtime;
	 return el->ssl_ctx;
      }
      el = el->next;
   }

   return NULL;
}


static ErlDrvData tls_drv_start(ErlDrvPort port, char *buff)
{
   tls_data *d = (tls_data *)driver_alloc(sizeof(tls_data));
   d->port = port;
   d->bio_read = NULL;
   d->bio_write = NULL;
   d->ssl = NULL;
   d->handshakes = 0;
   d->send_buffer = NULL;
   d->send_buffer_len = 0;
   d->send_buffer_size = 0;
   d->send_buffer2 = NULL;
   d->send_buffer2_len = 0;
   d->send_buffer2_size = 0;

   set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);

   return (ErlDrvData)d;
}

static void tls_drv_stop(ErlDrvData handle)
{
   tls_data *d = (tls_data *)handle;

   if (d->ssl != NULL)
      SSL_free(d->ssl);
   if (d->send_buffer != NULL)
      driver_free(d->send_buffer);
   if (d->send_buffer2 != NULL)
      driver_free(d->send_buffer2);

   driver_free((char *)handle);
}

static void tls_drv_finish()
{
   int level;
   struct bucket *el;
   int i;

   level = ht.level;
   for (i = 0; i < 1 << (level + 1); i++) {
      el = ht.buckets[i];
      while (el != NULL) {
	 if (el->ssl_ctx != NULL)
	    SSL_CTX_free(el->ssl_ctx);
	 driver_free(el->key);
	 el = el->next;
      }
   }

   driver_free(ht.buckets);
}

static int is_modified(char *file, time_t *known_mtime)
{
   struct stat file_stat;

   if (file == NULL) {
      return 0;
   } else if (stat(file, &file_stat)) {
      *known_mtime = 0;
      return 1;
   } else {
      if (*known_mtime != file_stat.st_mtime)
      {
	 *known_mtime = file_stat.st_mtime;
	 return 1;
      } else
	 return 0;
   }
}

static int verify_callback(int preverify_ok, X509_STORE_CTX *ctx)
{
   return 1;
}

/*
 * ECDHE is enabled only on OpenSSL 1.0.0e and later.
 * See http://www.openssl.org/news/secadv_20110906.txt
 * for details.
 */
#ifndef OPENSSL_NO_ECDH
static void setup_ecdh(SSL_CTX *ctx)
{
   EC_KEY *ecdh;

   if (SSLeay() < 0x1000005fL) {
      return;
   }

   ecdh = EC_KEY_new_by_curve_name(NID_X9_62_prime256v1);
   SSL_CTX_set_options(ctx, SSL_OP_SINGLE_ECDH_USE);
   SSL_CTX_set_tmp_ecdh(ctx, ecdh);

   EC_KEY_free(ecdh);
}
#endif

#ifndef OPENSSL_NO_DH
/*
1024-bit MODP Group with 160-bit prime order subgroup (RFC5114)
-----BEGIN DH PARAMETERS-----
MIIBDAKBgQCxC4+WoIDgHd6S3l6uXVTsUsmfvPsGo8aaap3KUtI7YWBz4oZ1oj0Y
mDjvHi7mUsAT7LSuqQYRIySXXDzUm4O/rMvdfZDEvXCYSI6cIZpzck7/1vrlZEc4
+qMaT/VbzMChUa9fDci0vUW/N982XBpl5oz9p21NpwjfH7K8LkpDcQKBgQCk0cvV
w/00EmdlpELvuZkF+BBN0lisUH/WQGz/FCZtMSZv6h5cQVZLd35pD1UE8hMWAhe0
sBuIal6RVH+eJ0n01/vX07mpLuGQnQ0iY/gKdqaiTAh6CR9THb8KAWm2oorWYqTR
jnOvoy13nVkY0IvIhY9Nzvl8KiSFXm7rIrOy5QICAKA=
-----END DH PARAMETERS-----
 */
static unsigned char dh1024_p[] = {
   0xB1,0x0B,0x8F,0x96,0xA0,0x80,0xE0,0x1D,0xDE,0x92,0xDE,0x5E,
   0xAE,0x5D,0x54,0xEC,0x52,0xC9,0x9F,0xBC,0xFB,0x06,0xA3,0xC6,
   0x9A,0x6A,0x9D,0xCA,0x52,0xD2,0x3B,0x61,0x60,0x73,0xE2,0x86,
   0x75,0xA2,0x3D,0x18,0x98,0x38,0xEF,0x1E,0x2E,0xE6,0x52,0xC0,
   0x13,0xEC,0xB4,0xAE,0xA9,0x06,0x11,0x23,0x24,0x97,0x5C,0x3C,
   0xD4,0x9B,0x83,0xBF,0xAC,0xCB,0xDD,0x7D,0x90,0xC4,0xBD,0x70,
   0x98,0x48,0x8E,0x9C,0x21,0x9A,0x73,0x72,0x4E,0xFF,0xD6,0xFA,
   0xE5,0x64,0x47,0x38,0xFA,0xA3,0x1A,0x4F,0xF5,0x5B,0xCC,0xC0,
   0xA1,0x51,0xAF,0x5F,0x0D,0xC8,0xB4,0xBD,0x45,0xBF,0x37,0xDF,
   0x36,0x5C,0x1A,0x65,0xE6,0x8C,0xFD,0xA7,0x6D,0x4D,0xA7,0x08,
   0xDF,0x1F,0xB2,0xBC,0x2E,0x4A,0x43,0x71,
};
static unsigned char dh1024_g[] = {
   0xA4,0xD1,0xCB,0xD5,0xC3,0xFD,0x34,0x12,0x67,0x65,0xA4,0x42,
   0xEF,0xB9,0x99,0x05,0xF8,0x10,0x4D,0xD2,0x58,0xAC,0x50,0x7F,
   0xD6,0x40,0x6C,0xFF,0x14,0x26,0x6D,0x31,0x26,0x6F,0xEA,0x1E,
   0x5C,0x41,0x56,0x4B,0x77,0x7E,0x69,0x0F,0x55,0x04,0xF2,0x13,
   0x16,0x02,0x17,0xB4,0xB0,0x1B,0x88,0x6A,0x5E,0x91,0x54,0x7F,
   0x9E,0x27,0x49,0xF4,0xD7,0xFB,0xD7,0xD3,0xB9,0xA9,0x2E,0xE1,
   0x90,0x9D,0x0D,0x22,0x63,0xF8,0x0A,0x76,0xA6,0xA2,0x4C,0x08,
   0x7A,0x09,0x1F,0x53,0x1D,0xBF,0x0A,0x01,0x69,0xB6,0xA2,0x8A,
   0xD6,0x62,0xA4,0xD1,0x8E,0x73,0xAF,0xA3,0x2D,0x77,0x9D,0x59,
   0x18,0xD0,0x8B,0xC8,0x85,0x8F,0x4D,0xCE,0xF9,0x7C,0x2A,0x24,
   0x85,0x5E,0x6E,0xEB,0x22,0xB3,0xB2,0xE5,
};

static int setup_dh(SSL_CTX *ctx, char *dh_file)
{
   DH *dh;
   int res;

   if (dh_file != NULL) {
      BIO *bio = BIO_new_file(dh_file, "r");

      if (bio == NULL) {
	 return 0;
      }
      dh = PEM_read_bio_DHparams(bio, NULL, NULL, NULL);
      BIO_free(bio);
      if (dh == NULL) {
	 return 0;
      }
   } else {
      dh = DH_new();
      if (dh == NULL) {
	 return 0;
      }

      dh->p = BN_bin2bn(dh1024_p, sizeof(dh1024_p), NULL);
      dh->g = BN_bin2bn(dh1024_g, sizeof(dh1024_g), NULL);
      if (dh->p == NULL || dh->g == NULL) {
	 DH_free(dh);
	 return 0;
      }
   }

   SSL_CTX_set_options(ctx, SSL_OP_SINGLE_DH_USE);
   res = (int)SSL_CTX_set_tmp_dh(ctx, dh);

   DH_free(dh);
   return res;
}
#endif

static void ssl_info_callback(const SSL *s, int where, int ret)
{
   if (where == SSL_CB_ACCEPT_LOOP) {
      int state = SSL_get_state(s);
      if (state == SSL3_ST_SR_CLNT_HELLO_A ||
	  state == SSL23_ST_SR_CLNT_HELLO_A) {
	 tls_data *d = (tls_data *)SSL_get_ex_data(s, ssl_index);
	 d->handshakes++;
      }
   }
}


#define SET_CERTIFICATE_FILE_ACCEPT 1
#define SET_CERTIFICATE_FILE_CONNECT 2
#define SET_ENCRYPTED_INPUT  3
#define SET_DECRYPTED_OUTPUT 4
#define GET_ENCRYPTED_OUTPUT 5
#define GET_DECRYPTED_INPUT  6
#define GET_PEER_CERTIFICATE 7
#define GET_VERIFY_RESULT    8
#define VERIFY_NONE 0x10000
#define COMPRESSION_NONE 0x100000

#define die_unless(cond, errstr)				\
	 if (!(cond))						\
	 {							\
	    int errstrlen = strlen(errstr);			\
	    unsigned long error_code = ERR_get_error();		\
	    char *error_string = error_code ?			\
	       ERR_error_string(error_code, NULL) :		\
	       NULL;						\
	    int error_string_length = error_string ?		\
	       strlen(error_string) : 0;			\
	    if (error_code)					\
	       rlen = errstrlen + error_string_length + 3;	\
	    else						\
	       rlen = errstrlen + 1;				\
	    b = driver_alloc_binary(rlen);			\
	    b->orig_bytes[0] = 1;				\
	    strncpy(b->orig_bytes + 1, errstr, errstrlen);	\
	    if (error_code) {					\
	       strncpy(b->orig_bytes + 1 + errstrlen,		\
		       ": ", 2);				\
	       strncpy(b->orig_bytes + 3 + errstrlen,		\
		       error_string, error_string_length);	\
	    }							\
	    *rbuf = (char *)b;					\
	    return rlen;					\
	 }

#ifdef _WIN32
/** public domain strtok_r() by Charlie Gordon
 **   from http://groups.google.com/group/comp.lang.c/msg/2ab1ecbb86646684
 */
char* strtok_r(
    char *str,
    const char *delim,
    char **nextp)
{
    char *ret;

    if (str == NULL)
    {
        str = *nextp;
    }

    str += strspn(str, delim);

    if (*str == '\0')
    {
        return NULL;
    }

    ret = str;

    str += strcspn(str, delim);

    if (*str)
    {
        *str++ = '\0';
    }

    *nextp = str;

    return ret;
}
#endif

static ErlDrvSSizeT tls_drv_control(ErlDrvData handle,
			   unsigned int command,
			   char *buf, ErlDrvSizeT len,
			   char **rbuf, ErlDrvSizeT rlen)
{
   tls_data *d = (tls_data *)handle;
   int res;
   int size;
   ErlDrvBinary *b;
   X509 *cert;
   unsigned int flags = command;

   command &= 0xffff;

   ERR_clear_error();
   switch (command)
   {
      case SET_CERTIFICATE_FILE_ACCEPT:
      case SET_CERTIFICATE_FILE_CONNECT: {
	 time_t key_mtime = 0;
	 time_t dh_mtime = 0;
	 char *key_file = buf;
	 size_t key_file_len = strlen(key_file);
	 char *ciphers = key_file + key_file_len + 1;
	 size_t ciphers_len = strlen(ciphers);
	 char *protocol_options = ciphers + ciphers_len + 1;
	 size_t protocol_options_len = strlen(protocol_options);
	 char *dh_file = protocol_options + protocol_options_len + 1;
	 size_t dh_file_len = strlen(dh_file);
	 char *hash_key = (char *)driver_alloc(key_file_len +
					       ciphers_len +
					       protocol_options_len +
					       dh_file_len + 1);
	 long options = 0L;

	 if (protocol_options_len != 0) {
	    char *po = strdup(protocol_options), delim[] = "|";
	    char *popts = po;
	    char *strtok_buf;

	    while ((po = strtok_r(po, delim, &strtok_buf)) != NULL) {
	       set_option_flag(po, &options);
	       po = NULL;
	    }

	    free(popts);
	 }

	 sprintf(hash_key, "%s%s%s%s", key_file, ciphers, protocol_options,
		 dh_file);
	 SSL_CTX *ssl_ctx = hash_table_lookup(hash_key, &key_mtime, &dh_mtime);

	 if (dh_file_len == 0)
	    dh_file = NULL;

	 if (is_modified(key_file, &key_mtime) ||
	     is_modified(dh_file, &dh_mtime) ||
	     ssl_ctx == NULL)
	 {
	    SSL_CTX *ctx;

	    hash_table_insert(hash_key, key_mtime, dh_mtime, NULL);

	    ctx = SSL_CTX_new(SSLv23_method());
	    die_unless(ctx, "SSL_CTX_new failed");

	    res = SSL_CTX_use_certificate_chain_file(ctx, key_file);
	    die_unless(res > 0, "SSL_CTX_use_certificate_file failed");

	    res = SSL_CTX_use_PrivateKey_file(ctx, key_file, SSL_FILETYPE_PEM);
	    die_unless(res > 0, "SSL_CTX_use_PrivateKey_file failed");

	    res = SSL_CTX_check_private_key(ctx);
	    die_unless(res > 0, "SSL_CTX_check_private_key failed");

	    if (ciphers_len == 0)
	       ciphers = CIPHERS;
	    SSL_CTX_set_cipher_list(ctx, ciphers);

#ifndef OPENSSL_NO_ECDH
	    setup_ecdh(ctx);
#endif
#ifndef OPENSSL_NO_DH
	    res = setup_dh(ctx, dh_file);
	    die_unless(res > 0, "Setting DH parameters failed");
#endif

	    SSL_CTX_set_session_cache_mode(ctx, SSL_SESS_CACHE_OFF);
	    SSL_CTX_set_default_verify_paths(ctx);
#ifdef SSL_MODE_RELEASE_BUFFERS
	    SSL_CTX_set_mode(ctx, SSL_MODE_RELEASE_BUFFERS);
#endif
	    /* SSL_CTX_load_verify_locations(ctx, "/etc/ejabberd/ca_certificates.pem", NULL); */
	    /* SSL_CTX_load_verify_locations(ctx, NULL, "/etc/ejabberd/ca_certs/"); */

	    /* This IF is commented to allow verification in all cases: */
	    /* if (command == SET_CERTIFICATE_FILE_ACCEPT) */
	    /* { */
	       SSL_CTX_set_verify(ctx,
				  SSL_VERIFY_PEER|SSL_VERIFY_CLIENT_ONCE,
				  verify_callback);
	    /* } */

	    SSL_CTX_set_info_callback(ctx, &ssl_info_callback);

	    ssl_ctx = ctx;
	    hash_table_insert(hash_key, key_mtime, dh_mtime, ssl_ctx);
	 }

	 driver_free(hash_key);

	 d->ssl = SSL_new(ssl_ctx);
	 die_unless(d->ssl, "SSL_new failed");

	 if (flags & VERIFY_NONE)
	    SSL_set_verify(d->ssl, SSL_VERIFY_NONE, verify_callback);

#ifdef SSL_OP_NO_COMPRESSION
	 if (flags & COMPRESSION_NONE)
	     SSL_set_options(d->ssl, SSL_OP_NO_COMPRESSION);
#endif

	 SSL_set_ex_data(d->ssl, ssl_index, d);

	 d->bio_read = BIO_new(BIO_s_mem());
	 d->bio_write = BIO_new(BIO_s_mem());

	 SSL_set_bio(d->ssl, d->bio_read, d->bio_write);

	 if (command == SET_CERTIFICATE_FILE_ACCEPT) {
	    options |= (SSL_OP_NO_TICKET|SSL_OP_ALL|SSL_OP_NO_SSLv2);

	    SSL_set_options(d->ssl, options);

	    SSL_set_accept_state(d->ssl);
	 } else {
	    options |= (SSL_OP_NO_TICKET|SSL_OP_NO_SSLv2);

	    SSL_set_options(d->ssl, options);

	    SSL_set_connect_state(d->ssl);
	 }
	 break;
      }
      case SET_ENCRYPTED_INPUT:
	 die_unless(d->ssl, "SSL not initialized");
	 BIO_write(d->bio_read, buf, len);
	 break;
      case SET_DECRYPTED_OUTPUT:
	 die_unless(d->ssl, "SSL not initialized");

	 if (len > 0) {
	    if (d->send_buffer != NULL) {
	       if (d->send_buffer2 == NULL) {
		  d->send_buffer2_len = len;
		  d->send_buffer2_size = len;
		  d->send_buffer2 = driver_alloc(d->send_buffer2_size);
		  memcpy(d->send_buffer2, buf, len);
	       } else {
		  if (d->send_buffer2_size <
		      d->send_buffer2_len + len) {
		     while (d->send_buffer2_size <
			    d->send_buffer2_len + len) {
			d->send_buffer2_size *= 2;
		     }
		     d->send_buffer2 = driver_realloc(d->send_buffer2,
						      d->send_buffer2_size);
		  }
		  memcpy(d->send_buffer2 + d->send_buffer2_len,
			 buf, len);
		  d->send_buffer2_len += len;
	       }
	    } else {
	       res = SSL_write(d->ssl, buf, len);
	       if (res <= 0) {
		  res = SSL_get_error(d->ssl, res);
		  if (res == SSL_ERROR_WANT_READ ||
		      res == SSL_ERROR_WANT_WRITE) {
		     d->send_buffer_len = len;
		     d->send_buffer_size = len;
		     d->send_buffer = driver_alloc(d->send_buffer_size);
		     memcpy(d->send_buffer, buf, len);
		  } else {
		     die_unless(0, "SSL_write failed");
		  }
	       }
	    }
	 }
	 break;
      case GET_ENCRYPTED_OUTPUT:
	 die_unless(d->ssl, "SSL not initialized");
	 size = BIO_ctrl_pending(d->bio_write) + 1;
	 b = driver_alloc_binary(size);
	 b->orig_bytes[0] = 0;
	 BIO_read(d->bio_write, b->orig_bytes + 1, size - 1);
	 *rbuf = (char *)b;
	 return size;
      case GET_DECRYPTED_INPUT: {
	 int retcode = 0;
	 if (!SSL_is_init_finished(d->ssl))
	 {
	    retcode = 2;
	    res = SSL_do_handshake(d->ssl);
	    if (res <= 0)
	       die_unless(SSL_get_error(d->ssl, res) == SSL_ERROR_WANT_READ,
			  "SSL_do_handshake failed");
	 }
	 if (SSL_is_init_finished(d->ssl)) {
	    size_t req_size = 0;
	    int i;
	    for (i = 0; i < 2; i++)
	       if (d->send_buffer != NULL) {
		  res = SSL_write(d->ssl, d->send_buffer, d->send_buffer_len);
		  if (res <= 0) {
		     die_unless(0, "SSL_write failed");
		  }
		  retcode = 2;
		  driver_free(d->send_buffer);
		  d->send_buffer = d->send_buffer2;
		  d->send_buffer_len = d->send_buffer2_len;
		  d->send_buffer_size = d->send_buffer2_size;
		  d->send_buffer2 = NULL;
		  d->send_buffer2_len = 0;
		  d->send_buffer2_size = 0;
	       }

	    if (len == 4)
	    {
	       unsigned char *b = (unsigned char *)buf;
	       req_size =
		  (b[0] << 24) | (b[1] << 16) | (b[2] << 8) | b[3];
	    }
	    size = BUF_SIZE + 1;
	    rlen = 1;
	    b = driver_alloc_binary(size);
	    b->orig_bytes[0] = retcode;

	    res = 0;

	    while ((req_size == 0 || rlen < req_size + 1) &&
		   (res = SSL_read(d->ssl,
				   b->orig_bytes + rlen,
				   (req_size == 0 || req_size + 1 >= size) ?
				   size - rlen : req_size + 1 - rlen)) > 0)
	    {
	       //printf("%d bytes of decrypted data read from state machine\r\n",res);
	       rlen += res;
	       if (size - rlen < BUF_SIZE) {
		  size *= 2;
		  b = driver_realloc_binary(b, size);
	       }
	    }

	    if (d->handshakes > 1) {
	       char *error = "client renegotiations forbidden";
	       int error_len = strlen(error);
	       rlen = error_len + 1;
	       b = driver_alloc_binary(rlen);
	       b->orig_bytes[0] = 1;
	       strncpy(b->orig_bytes + 1, error, error_len);
	       *rbuf = (char *)b;
	       return rlen;
	    }

	    if (res < 0)
	    {
	       int err = SSL_get_error(d->ssl, res);

	       if (err == SSL_ERROR_WANT_READ)
	       {
		  //printf("SSL_read wants more data\r\n");
		  //return 0;
	       }
	       // TODO
	    }
	    b = driver_realloc_binary(b, rlen);
	    *rbuf = (char *)b;
	    return rlen;
	 } else {
	    b = driver_alloc_binary(1);
	    b->orig_bytes[0] = 2;
	    *rbuf = (char *)b;
	    return 1;
	 }
	 break;
      }
      case GET_PEER_CERTIFICATE:
	 cert = SSL_get_peer_certificate(d->ssl);
	 if (cert == NULL)
	 {
	    b = driver_alloc_binary(1);
	    b->orig_bytes[0] = 1;
	    *rbuf = (char *)b;
	    return 1;
	 } else {
	    unsigned char *tmp_buf;
	    rlen = i2d_X509(cert, NULL);
	    if (rlen >= 0)
	    {
	       rlen++;
	       b = driver_alloc_binary(rlen);
	       b->orig_bytes[0] = 0;
	       tmp_buf = (unsigned char *)&b->orig_bytes[1];
	       i2d_X509(cert, &tmp_buf);
	       X509_free(cert);
	       *rbuf = (char *)b;
	       return rlen;
	    } else
	       X509_free(cert);
	 }
	 break;
      case GET_VERIFY_RESULT:
	 b = driver_alloc_binary(1);
	 b->orig_bytes[0] = SSL_get_verify_result(d->ssl);
	 *rbuf = (char *)b;
	 return 1;
	 break;
   }

   b = driver_alloc_binary(1);
   b->orig_bytes[0] = 0;
   *rbuf = (char *)b;
   return 1;
}


ErlDrvEntry tls_driver_entry = {
   NULL,			/* F_PTR init, N/A */
   tls_drv_start,		/* L_PTR start, called when port is opened */
   tls_drv_stop,		/* F_PTR stop, called when port is closed */
   NULL,			/* F_PTR output, called when erlang has sent */
   NULL,			/* F_PTR ready_input, called when input descriptor ready */
   NULL,			/* F_PTR ready_output, called when output descriptor ready */
   "p1_tls_drv",		/* char *driver_name, the argument to open_port */
   tls_drv_finish,		/* F_PTR finish, called when unloaded */
   NULL,			/* handle */
   tls_drv_control,		/* F_PTR control, port_command callback */
   NULL,			/* F_PTR timeout, reserved */
   NULL,			/* F_PTR outputv, reserved */
  /* Added in Erlang/OTP R15B: */
  NULL,                 /* ready_async */
  NULL,                 /* flush */
  NULL,                 /* call */
  NULL,                 /* event */
  ERL_DRV_EXTENDED_MARKER,        /* extended_marker */
  ERL_DRV_EXTENDED_MAJOR_VERSION, /* major_version */
  ERL_DRV_EXTENDED_MINOR_VERSION, /* minor_version */
  0,                    /* driver_flags */
  NULL,                 /* handle2 */
  NULL,                 /* process_exit */
  NULL                  /* stop_select */
};

DRIVER_INIT(p1_tls_drv) /* must match name in driver_entry */
{
   CRYPTO_set_mem_functions(driver_alloc, driver_realloc, driver_free);
   OpenSSL_add_ssl_algorithms();
   SSL_load_error_strings();
   init_hash_table();
   ssl_index = SSL_get_ex_new_index(0, "ssl index", NULL, NULL, NULL);
   return &tls_driver_entry;
}


