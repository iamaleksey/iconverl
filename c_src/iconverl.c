/*
 * Copyright (c) 2013 Eric des Courtis <eric.des.courtis@gmail.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

/*
  ┌─────────────────────────────────────────────────────────────┐
  │▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓│
  │▓                                                           ▓│
  │▓  ██  ██▀▀██  ██▀██  ███ ██ ██    ██ ██████ ██▀▀██ ██      ▓│
  │▓  ██ ██    ▀ ██   ██ ██████  ██  ██  ██▄▄   ██▄▄██ ██      ▓│
  │▓  ██ ██      ██   ██ ██ ███   ████   ██▀▀   ██▀██  ██   ▄  ▓│
  │▓  ██  ██▄▄██  ██▄██  ██  ██    ██    ██████ ██  ██ ██▄▄██  ▓│
  │▓                                                           ▓│
  │▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓│
  └─────────────────────────────────────────────────────────────┘
*/

#include "erl_nif.h"

#include <iconv.h>
#include <errno.h>
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#define CHUNK_SIZE 4096

static ErlNifResourceType *iconv_cd_type = NULL;

static ERL_NIF_TERM iconv_iconv_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM iconv_open_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM iconv_reset_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM make_error_tuple_from_errno(ErlNifEnv* env, int errn);
static ERL_NIF_TERM make_error_tuple_from_string(ErlNifEnv* env, const char *error);
static ERL_NIF_TERM make_error_tuple(ErlNifEnv* env, ERL_NIF_TERM error);
static void garbage_collect_iconv_cd(ErlNifEnv *env, void *cd);
static int handle_load(ErlNifEnv *env, void **priv, ERL_NIF_TERM load_info);
static int handle_upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info);

static ErlNifFunc nif_funcs[] = {
    {"open",  2, iconv_open_nif  },
    {"iconv", 2, iconv_iconv_nif },
    {"reset", 1, iconv_reset_nif }
};

static ERL_NIF_TERM iconv_open_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    unsigned int to_len;
    unsigned int from_len;
    char *to;
    char *from;
    iconv_t conv_desc;
    iconv_t *conv_desc_ref;
    int res;

    assert(argc == 2);
    assert(enif_get_list_length(env, argv[0], &to_len));
    assert(enif_get_list_length(env, argv[1], &from_len));

    to = (char *)enif_alloc(++to_len);
    if(to == NULL){
        return make_error_tuple_from_string(env, "to_enif_alloc");
    }

    from = (char *)enif_alloc(++from_len);

    if(from == NULL){
        enif_free(to);
        return make_error_tuple_from_string(env, "from_enif_alloc");
    }

    res = enif_get_string(
        env,
        argv[0],
        to,
        to_len,
        ERL_NIF_LATIN1
    );

    if(res < 0){
        enif_free(to);
        enif_free(from);
        return make_error_tuple_from_string(env, "to_enif_get_string_truncation");
    }

    if(res == 0){
        enif_free(to);
        enif_free(from);
        return make_error_tuple_from_string(env, "to_enif_get_string_cannot_encode");
    }

    res = enif_get_string(
        env,
        argv[1],
        from,
        from_len,
        ERL_NIF_LATIN1
    );

    if(res < 0){
        enif_free(to);
        enif_free(from);
        return make_error_tuple_from_string(env, "from_enif_get_string_truncation");
    }

    if(res == 0){
        enif_free(to);
        enif_free(from);
        return make_error_tuple_from_string(env, "from_enif_get_string_cannot_encode");
    }

    conv_desc = iconv_open(to, from);

    if(conv_desc == (iconv_t) -1){
        enif_free(to);
        enif_free(from);

        switch(errno){
            case EINVAL:
                return make_error_tuple_from_string(env, "unsupported");
            default:
                return make_error_tuple_from_errno(env, errno);
        }
    }

    conv_desc_ref = (iconv_t *)enif_alloc_resource(iconv_cd_type, sizeof(iconv_t));
    *conv_desc_ref = conv_desc;

    return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_resource(env, conv_desc_ref));
}


static ERL_NIF_TERM iconv_iconv_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    iconv_t *cd_ptr = NULL;
    int res;
    size_t ret;
    ErlNifBinary in_bin;
    ErlNifBinary out_bin;
    ERL_NIF_TERM final_bin;
    char *inbuf;
    char *outbuf;
    size_t inbytesleft;
    size_t outbytesleft;

    assert(argc == 2);
    assert(enif_get_resource(env, argv[0], iconv_cd_type, (void **)&cd_ptr));
    assert(cd_ptr != NULL);

    res = enif_inspect_binary(env, argv[1], &in_bin);
    if(res == 0){
        return make_error_tuple_from_string(env, "not_a_byte_aligned_binary");
    }

    if(enif_alloc_binary(CHUNK_SIZE, &out_bin) == 0){
        return make_error_tuple_from_string(env, "enif_alloc_binary");
    }

    inbytesleft = in_bin.size;
    outbytesleft = out_bin.size;
    inbuf = (char *)in_bin.data;
    outbuf = (char *)out_bin.data;

    ret = iconv(*cd_ptr, &inbuf, &inbytesleft, &outbuf, &outbytesleft);
    if(enif_realloc_binary(&out_bin, out_bin.size - outbytesleft) == 0){
        enif_release_binary(&out_bin);
        return make_error_tuple_from_string(env, "enif_realloc_binary");
    }

    final_bin = enif_make_binary(env, &out_bin);

    if(ret == (size_t)-1){
        switch(errno){
            case E2BIG:
                return enif_make_tuple4(
                    env,
                    enif_make_atom(env, "ok"),
                    enif_make_atom(env, "e2big"),
                    enif_make_uint64(env, (ErlNifUInt64)inbytesleft),
                    final_bin
                );
            case EILSEQ:
                return enif_make_tuple4(
                    env,
                    enif_make_atom(env, "ok"),
                    enif_make_atom(env, "eilseq"),
                    enif_make_uint64(env, (ErlNifUInt64)inbytesleft),
                    final_bin
                );
            case EINVAL:
                return enif_make_tuple4(
                    env,
                    enif_make_atom(env, "ok"),
                    enif_make_atom(env, "einval"),
                    enif_make_uint64(env, (ErlNifUInt64)inbytesleft),
                    final_bin
                );
            case EBADF:
                return make_error_tuple_from_string(env, "ebadf");
            default:
                return make_error_tuple_from_errno(env, errno);
        }
    }

    return enif_make_tuple3(
        env,
        enif_make_atom(env, "ok"),
        enif_make_uint64(env, (ErlNifUInt64)inbytesleft),
        final_bin
    );
}

static ERL_NIF_TERM iconv_reset_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    iconv_t *cd_ptr = NULL;
    size_t ret;

    assert(argc == 1);
    assert(enif_get_resource(env, argv[0], iconv_cd_type, (void **)&cd_ptr));
    assert(cd_ptr != NULL);

    ret = iconv(*cd_ptr, NULL, NULL, NULL, NULL);
    if(ret == (size_t) -1) {
        return make_error_tuple_from_errno(env, errno);
    }

    return enif_make_atom(env, "ok");
}



static ERL_NIF_TERM make_error_tuple_from_errno(ErlNifEnv* env, int errn)
{
    return enif_make_tuple2(
        env,
        enif_make_atom(env, "error"),
        enif_make_int(env, errn)
    );
}

static ERL_NIF_TERM make_error_tuple_from_string(ErlNifEnv* env, const char *error)
{
    return make_error_tuple(env, enif_make_atom(env, error));
}

static ERL_NIF_TERM make_error_tuple(ErlNifEnv* env, ERL_NIF_TERM error)
{
    return enif_make_tuple2(env, enif_make_atom(env, "error"), error);
}

static void garbage_collect_iconv_cd(ErlNifEnv *env, void *cd)
{
    int res;

    res = iconv_close(*(iconv_t *)cd);

    if(res == -1){
        fprintf(
            stderr,
            "iconv_close() in file: %s at line: %d failed with errno: %d\n",
            __FILE__,
            __LINE__,
            errno
        );
        abort();
    }

    assert(res == 0);
}

static int handle_load(ErlNifEnv *env, void **priv, ERL_NIF_TERM load_info)
{
    ErlNifResourceType *rt;
    ErlNifResourceFlags rf;

    rt = enif_open_resource_type(
        env,
        NULL,
        "iconv_cd_type",
        garbage_collect_iconv_cd,
        ERL_NIF_RT_CREATE,
        &rf
    );

    if(rt == NULL) return -1;

    iconv_cd_type = rt;

    return 0;
}

static int handle_upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
    return 0;
}

ERL_NIF_INIT(iconverl, nif_funcs, handle_load, NULL, handle_upgrade, NULL)
