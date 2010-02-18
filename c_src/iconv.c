#include "erl_nif.h"
#include <iconv.h>
#include <errno.h>
#include <string.h>

static ErlNifResourceType *iconv_cd_type = NULL;

typedef struct { iconv_t cd; } iconv_cd;

static struct {
    ERL_NIF_TERM ok;
    ERL_NIF_TERM error;
    ERL_NIF_TERM enomem;
    ERL_NIF_TERM eilseq;
    ERL_NIF_TERM einval;
    ERL_NIF_TERM eunknown;
} iconv_atoms;

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
    size_t rc;
    iconv_cd *cd;
    ERL_NIF_TERM error, result;

    if (!enif_get_resource(env, argv[0], iconv_cd_type, (void **) &cd)) {
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
        return enif_make_tuple2(env, iconv_atoms.error, iconv_atoms.enomem);
    }

    out = conv_bin.data;

    iconv(cd->cd, NULL, NULL, NULL, NULL);

    do {
        rc = iconv(cd->cd, &in, &inleft, &out, &outleft);
        if (rc == 0) { // done.
            if (outleft > 0) { // trim.
                enif_realloc_binary(env, &conv_bin, outsize - outleft);
            }
            result = enif_make_binary(env, &conv_bin);
            return enif_make_tuple2(env, iconv_atoms.ok, result);
        } else if (errno == E2BIG) { // double the binary.
            outleft += outsize;
            outsize *= 2;
            if (!enif_realloc_binary(env, &conv_bin, outsize)) {
                enif_release_binary(env, &conv_bin);
                return enif_make_tuple2(env, iconv_atoms.error, iconv_atoms.enomem);
            }
            out = conv_bin.data + (outsize - outleft);
        } else { // another error.
            enif_release_binary(env, &conv_bin);
            if      (errno == EILSEQ) { error = iconv_atoms.eilseq;   }
            else if (errno == EINVAL) { error = iconv_atoms.einval;   }
            else                      { error = iconv_atoms.eunknown; }
            return enif_make_tuple2(env, iconv_atoms.error, error);
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

    iconv_atoms.ok       = enif_make_atom(env, "ok");
    iconv_atoms.error    = enif_make_atom(env, "error");
    iconv_atoms.enomem   = enif_make_atom(env, "enomem");
    iconv_atoms.eilseq   = enif_make_atom(env, "eilseq");
    iconv_atoms.einval   = enif_make_atom(env, "einval");
    iconv_atoms.eunknown = enif_make_atom(env, "eunknown");

    return 0;
}

ERL_NIF_INIT(iconv, nif_funcs, load, NULL, NULL, NULL)
