#include "erl_nif.h"
#include <iconv.h>
#include <errno.h>
#include <string.h>

static iconv_t extract_cd(ErlNifEnv* env, ERL_NIF_TERM binary);

static ERL_NIF_TERM
erl_iconv_open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary to, from;
    iconv_t cd;
    ErlNifBinary cd_binary;

    if (!enif_inspect_binary(env, argv[0], &to)) {
        return enif_make_badarg(env);
    }

    if (!enif_inspect_binary(env, argv[1], &from)) {
        return enif_make_badarg(env);
    }

    cd = iconv_open(to.data, from.data);

    if (cd == (iconv_t) -1) {
        return enif_make_badarg(env);
    }

    enif_alloc_binary(env, sizeof(iconv_t), &cd_binary);
    memcpy(cd_binary.data, &cd, sizeof(iconv_t));

    return enif_make_binary(env, &cd_binary);
}

static ERL_NIF_TERM
erl_iconv_close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    iconv_t cd = extract_cd(env, argv[0]);

    if ((cd == (iconv_t) -1) || (iconv_close(cd) != 0)) {
        return enif_make_badarg(env);
    }

    return enif_make_atom(env, "ok");
}

// TODO: handle alloc/realloc failures.
static ERL_NIF_TERM
erl_iconv(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary orig_bin, conv_bin;
    size_t inleft, outleft, outsize;
    char *in, *out;
    char *err;
    ERL_NIF_TERM ok, error, result;
    size_t rc = -1;

    iconv_t cd = extract_cd(env, argv[0]);

    if (cd == (iconv_t) -1) {
        return enif_make_badarg(env);
    }

    if (!enif_inspect_binary(env, argv[1], &orig_bin)) {
        return enif_make_badarg(env);
    }

    in = orig_bin.data;
    inleft = orig_bin.size;

    outsize = inleft;
    outleft = outsize;

    enif_alloc_binary(env, outsize, &conv_bin);
    out = conv_bin.data;

    iconv(cd, NULL, NULL, NULL, NULL);

    do {
        rc = iconv(cd, &in, &inleft, &out, &outleft);
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
            enif_realloc_binary(env, &conv_bin, outsize);
            out = conv_bin.data + (outsize - outleft);
        } else { // other error.
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

static iconv_t
extract_cd(ErlNifEnv* env, ERL_NIF_TERM binary) {
    ErlNifBinary cd_binary;
    iconv_t cd;

    if (!enif_inspect_binary(env, binary, &cd_binary)) {
        return (iconv_t) -1;
    }

    if (cd_binary.size != sizeof(iconv_t)) {
        return (iconv_t) -1;
    }

    memcpy(&cd, cd_binary.data, sizeof(iconv_t));

    return cd;
}

static ErlNifFunc nif_funcs[] = {
    {"open",  2, erl_iconv_open},
    {"close", 1, erl_iconv_close},
    {"iconv", 2, erl_iconv}
};

ERL_NIF_INIT(iconv, nif_funcs, NULL, NULL, NULL, NULL)
