/*
 * Copyright (c) 2012 Eric des Courtis <eric.des.courtis@gmail.com>
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

static ERL_NIF_TERM iconv_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM make_error_tuple_from_string_errno(ErlNifEnv* env, const char *error);
static ERL_NIF_TERM make_error_tuple_from_string(ErlNifEnv* env, const char *error);
static ERL_NIF_TERM handle_close_after_error_from_string(ErlNifEnv* env, const char *error, iconv_t conv_desc);
static ERL_NIF_TERM make_iconv_close_error_tuple(ErlNifEnv* env, ERL_NIF_TERM error, const char *close_error);
static ERL_NIF_TERM handle_close_after_error(ErlNifEnv* env, ERL_NIF_TERM error, iconv_t conv_desc);
static ERL_NIF_TERM make_error_tuple(ErlNifEnv* env, ERL_NIF_TERM error);
static int handle_upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info);

static ErlNifFunc nif_funcs[] = {
    {"conv", 3, iconv_nif}
};


static ERL_NIF_TERM iconv_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    size_t ret;
    iconv_t conv_desc;
    char *to;
    char *from;
    int res;
    unsigned int to_len;
    unsigned int from_len;
    ErlNifBinary bin;
    char *bin_buf;
    char *out_bin_buf;
    size_t inbytesleft;
    size_t outbytesleft;
    ErlNifBinary out_bin;
    ErlNifBinary final_bin;
    int input_multiplier = 5;

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
                return make_error_tuple(env, enif_make_int(env, errno));
        }
    }

    res = enif_inspect_binary(env, argv[2], &bin);
    if(res == 0){
        enif_free(to);
        enif_free(from);
        return handle_close_after_error_from_string(env, "not_a_byte_aligned_binary", conv_desc);
    }


    for(;;){
        if(enif_alloc_binary(bin.size * input_multiplier, &out_bin) == 0){
            enif_free(to);
            enif_free(from);
            return handle_close_after_error_from_string(env, "enif_alloc_binary", conv_desc);
        }

        inbytesleft = bin.size;
        outbytesleft = out_bin.size;
        bin_buf = (char *)bin.data;
        out_bin_buf = (char *)out_bin.data;

        ret = iconv(
            conv_desc,
            &bin_buf,
            &inbytesleft,
            &out_bin_buf,
            &outbytesleft
        );

        if(ret == (size_t)-1){
            switch(errno){
                case E2BIG:
                    enif_release_binary(&out_bin);
                    input_multiplier *= 2;
                    continue;
                case EILSEQ:
                    enif_free(to);
                    enif_free(from);
                    return handle_close_after_error_from_string(env, "eilseq", conv_desc);
                case EINVAL:
                    enif_free(to);
                    enif_free(from);
                    return handle_close_after_error_from_string(env, "einval", conv_desc);
                default:
                    enif_free(to);
                    enif_free(from);
                    return handle_close_after_error_from_string(env, "iconv", conv_desc);
            }
        }

        if(enif_alloc_binary(out_bin.size - outbytesleft, &final_bin) == 0){
            enif_release_binary(&out_bin);
            enif_free(to);
            enif_free(from);

            return handle_close_after_error_from_string(env, "enif_alloc_binary", conv_desc);
        }

        memcpy(final_bin.data, out_bin.data, out_bin.size - outbytesleft);
        enif_release_binary(&out_bin);
        enif_free(to);
        enif_free(from);
        res = iconv_close(conv_desc);
        switch(res) {
            case -1:
                return make_error_tuple_from_string_errno(env, "iconv_close");
            case 0:
                break;

            default:
                return make_error_tuple_from_string_errno(env, "iconv_close_weird_return_value");
        }

        break;
    }

    return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_binary(env, &final_bin));

}

static ERL_NIF_TERM make_error_tuple_from_string_errno(ErlNifEnv* env, const char *error)
{
    return make_error_tuple(
        env,
        enif_make_tuple2(
            env,
            enif_make_atom(env, error),
            enif_make_int(env, errno)
        )
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

static ERL_NIF_TERM make_iconv_close_error_tuple(ErlNifEnv* env, ERL_NIF_TERM error, const char *close_error)
{
    return make_error_tuple(
        env,
        enif_make_tuple2(
            env,
            error,
            enif_make_tuple2(
                env,
                enif_make_atom(
                    env,
                    close_error
                ),
                enif_make_int(env, errno)
            )
        )
    );
}

static ERL_NIF_TERM handle_close_after_error_from_string(ErlNifEnv* env, const char *error, iconv_t conv_desc)
{
    return handle_close_after_error(env, enif_make_atom(env, error), conv_desc);
}

static ERL_NIF_TERM handle_close_after_error(ErlNifEnv* env, ERL_NIF_TERM error, iconv_t conv_desc)
{
    int res;

    res = iconv_close(conv_desc);

    switch(res){
    case -1:
        return make_iconv_close_error_tuple(env, error, "iconv_close");
    case 0:
        return make_error_tuple(env, error);
    default:
        break;
    }

    return make_iconv_close_error_tuple(env, error, "iconv_close_weird_return_value");
}

static int handle_upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
    return 0;
}

ERL_NIF_INIT(iconverl, nif_funcs, NULL, NULL, handle_upgrade, NULL);
