#include "erl_nif.h"
#include <iconv.h>
#include <errno.h>
#include <string.h>

static ErlNifResourceType *iconv_cd_type;

typedef struct { iconv_t cd; } iconv_cd;

static ERL_NIF_TERM
erl_iconv_open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary to, from;
    iconv_cd *cd;
    ERL_NIF_TERM result;

    if (!enif_inspect_binary(env, argv[0], &to)) {
        return enif_make_badarg(env);
    }

    if (!enif_inspect_binary(env, argv[1], &from)) {
        return enif_make_badarg(env);
    }

    cd = enif_alloc_resource(env, iconv_cd_type, sizeof(iconv_cd));

    cd->cd = iconv_open(to.data, from.data);

    if (cd->cd == (iconv_t) -1) {
        enif_release_resource(env, cd);
        return enif_make_badarg(env);
    }

    result = enif_make_resource(env, cd);
    enif_release_resource(env, cd);

    return result;
}

static ERL_NIF_TERM
erl_iconv(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary orig_bin, conv_bin;
    size_t inleft, outleft, outsize;
    char *in, *out;
    char *err;
    size_t rc;
    iconv_cd *cd;
    ERL_NIF_TERM ok, error, result;

    if(!enif_get_resource(env, argv[0], iconv_cd_type, (void **) &cd)) {
        return enif_make_badarg(env);
    }

    if (!enif_inspect_binary(env, argv[1], &orig_bin)) {
        return enif_make_badarg(env);
    }

    in = orig_bin.data;
    inleft = orig_bin.size;

    outsize = inleft;
    outleft = outsize;

    if (!enif_alloc_binary(env, outsize, &conv_bin)) {
        error = enif_make_atom(env, "error");
        result = enif_make_atom(env, "enomem");
        return enif_make_tuple(env, 2, error, result);
    }

    out = conv_bin.data;

    iconv(cd->cd, NULL, NULL, NULL, NULL);

    do {
        rc = iconv(cd->cd, &in, &inleft, &out, &outleft);
        if (rc == 0) { // done.
            if (outleft > 0) { // trim.
                enif_realloc_binary(env, &conv_bin, outsize - outleft);
            }
            ok = enif_make_atom(env, "ok");
            result = enif_make_binary(env, &conv_bin);
            return enif_make_tuple(env, 2, ok, result);
        } else if (errno == E2BIG) { // double the binary.
            outleft += outsize;
            outsize *= 2;
            if (!enif_realloc_binary(env, &conv_bin, outsize)) {
                enif_release_binary(env, &conv_bin);
                error = enif_make_atom(env, "error");
                result = enif_make_atom(env, "enomem");
                return enif_make_tuple(env, 2, error, result);
            }
            out = conv_bin.data + (outsize - outleft);
        } else { // another error.
            enif_release_binary(env, &conv_bin);
            if      (errno == EILSEQ) { err = "eilseq";   }
            else if (errno == EINVAL) { err = "einval";   }
            else                      { err = "eunknown"; }
            error = enif_make_atom(env, "error");
            result = enif_make_atom(env, err);
            return enif_make_tuple(env, 2, error, result);
        }
    } while (rc != 0);
}

static ErlNifFunc nif_funcs[] = {
    {"open", 2, erl_iconv_open},
    {"conv", 2, erl_iconv}
};

static void
gc_iconv_cd(ErlNifEnv* env, void* cd) {
    iconv_close(((iconv_cd *) cd)->cd);
}

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info) {
    iconv_cd_type = enif_open_resource_type(env, "iconv_cd_type",
        gc_iconv_cd, ERL_NIF_RT_CREATE, NULL);
    return 0;
}

ERL_NIF_INIT(iconv, nif_funcs, load, NULL, NULL, NULL)
