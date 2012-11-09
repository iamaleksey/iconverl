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

#include "erl_nif.h"

#include <iconv.h>
#include <errno.h>
#include <assert.h>
#include <string.h>

static ERL_NIF_TERM iconv_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

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
        return enif_make_tuple2(
            env,
            enif_make_atom(env, "error"),
            enif_make_atom(env, "to_enif_alloc_failed")
        );
    }
    
    from = (char *)enif_alloc(++from_len);

    if(from == NULL){
        enif_free(to);
        return enif_make_tuple2(
            env,
            enif_make_atom(env, "error"),
            enif_make_atom(env, "from_enif_alloc_failed")
        );
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
        return enif_make_tuple2(
            env,
            enif_make_atom(env, "error"),
            enif_make_atom(env, "to_enif_get_string_truncation")
        );       
    }
    
    if(res == 0){
        enif_free(to);
        enif_free(from);
        return enif_make_tuple2(
            env,
            enif_make_atom(env, "error"),
            enif_make_atom(env, "to_enif_get_string_cannot_encode")
        );       
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
        return enif_make_tuple2(
            env,
            enif_make_atom(env, "error"),
            enif_make_atom(env, "from_enif_get_string_truncation")
        );       
    }
    
    if(res == 0){
        enif_free(to);
        enif_free(from);
        return enif_make_tuple2(
            env,
            enif_make_atom(env, "error"),
            enif_make_atom(env, "from_enif_get_string_cannot_encode")
        );       
    }

    conv_desc = iconv_open(to, from);
    
    if(conv_desc == (iconv_t) -1){
        enif_free(to);
        enif_free(from);
        
        switch(errno){
            case EINVAL:
                return enif_make_tuple2(
                    env,
                    enif_make_atom(env, "error"),
                    enif_make_atom(env, "unsupported")
                );
            default:
                return enif_make_tuple2(
                    env,
                    enif_make_atom(env, "error"),
                    enif_make_int(env, errno)
                );                
        }
    }
    
    res = enif_inspect_binary(env, argv[2], &bin);
    if(res == 0){
        enif_free(to);
        enif_free(from);
        res = iconv_close(conv_desc);
        switch(res) {
            case -1:
                return enif_make_tuple2(
                    env,
                    enif_make_atom(env, "error"),
                    enif_make_tuple2(
                        env,
                        enif_make_atom(
                            env, 
                            "not_binary_iconv_close_failed"
                        ),
                        enif_make_int(env, errno)
                    )
                );                  
            case 0:    
                return enif_make_tuple2(
                    env,
                    enif_make_atom(env, "error"),
                    enif_make_atom(env, "not_a_binary")
                );
            default:
                return enif_make_tuple2(
                    env,
                    enif_make_atom(env, "error"),
                    enif_make_tuple2(
                        env,
                        enif_make_atom(
                            env, 
                            "not_binary_iconv_close_weird_return_value"
                        ),
                        enif_make_int(env, errno)
                    )
                );           
        }
    }


    for(;;){
        if(enif_alloc_binary(bin.size * input_multiplier, &out_bin) == 0){
            enif_free(to);
            enif_free(from);
            res = iconv_close(conv_desc);
            switch(res) {
                case -1:
                    return enif_make_tuple2(
                        env,
                        enif_make_atom(env, "error"),
                        enif_make_tuple2(
                            env,
                            enif_make_atom(
                                env, 
                                "alloc_failed_iconv_close_failed"
                            ),
                            enif_make_int(env, errno)
                        )
                    );                  
                case 0:    
                    return enif_make_tuple2(
                        env,
                        enif_make_atom(env, "error"),
                        enif_make_atom(env, "alloc_failed")
                    );
                default:
                    return enif_make_tuple2(
                        env,
                        enif_make_atom(env, "error"),
                        enif_make_tuple2(
                            env,
                            enif_make_atom(
                                env, 
                                "alloc_failed_iconv_close_weird_return_value"
                            ),
                            enif_make_int(env, errno)
                        )
                    );           
            }
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
                    res = iconv_close(conv_desc);
                    switch(res) {
                        case -1:
                            return enif_make_tuple2(
                                env,
                                enif_make_atom(env, "error"),
                                enif_make_tuple3(
                                    env,
                                    enif_make_atom(
                                        env, 
                                        "iconv_failed_iconv_close_failed"
                                    ),
                                    enif_make_atom(
                                        env,
                                        "eilseq"
                                    ),
                                    enif_make_int(env, errno)
                                )
                            );                  
                        case 0:    
                            return enif_make_tuple2(
                                env,
                                enif_make_atom(env, "error"),
                                enif_make_atom(env, "eilseq")
                            );
                        default:
                            return enif_make_tuple2(
                                env,
                                enif_make_atom(env, "error"),
                                enif_make_tuple3(
                                    env,
                                    enif_make_atom(
                                        env, 
                                        "iconv_failed_iconv_close_weird_return_value"
                                    ),
                                    enif_make_atom(
                                        env,
                                        "eilseq"
                                    ),
                                    enif_make_int(env, errno)
                                )
                            );           
                    }
                case EINVAL:
                    enif_free(to);
                    enif_free(from);
                    res = iconv_close(conv_desc);
                    switch(res) {
                        case -1:
                            return enif_make_tuple2(
                                env,
                                enif_make_atom(env, "error"),
                                enif_make_tuple3(
                                    env,
                                    enif_make_atom(
                                        env, 
                                        "iconv_failed_iconv_close_failed"
                                    ),
                                    enif_make_atom(
                                        env,
                                        "einval"
                                    ),
                                    enif_make_int(env, errno)
                                )
                            );                  
                        case 0:    
                            return enif_make_tuple2(
                                env,
                                enif_make_atom(env, "error"),
                                enif_make_atom(env, "einval")
                            );
                        default:
                            return enif_make_tuple2(
                                env,
                                enif_make_atom(env, "error"),
                                enif_make_tuple3(
                                    env,
                                    enif_make_atom(
                                        env, 
                                        "iconv_failed_iconv_close_weird_return_value"
                                    ),
                                    enif_make_atom(
                                        env,
                                        "einval"
                                    ),
                                    enif_make_int(env, errno)
                                )
                            );           
                    }
                default:
                    enif_free(to);
                    enif_free(from);
                    res = iconv_close(conv_desc);
                    switch(res) {
                        case -1:
                            return enif_make_tuple2(
                                env,
                                enif_make_atom(env, "error"),
                                enif_make_tuple2(
                                    env,
                                    enif_make_atom(
                                        env, 
                                        "iconv_failed_iconv_close_failed"
                                    ),
                                    enif_make_int(env, errno)
                                )
                            );                  
                        case 0:    
                            return enif_make_tuple2(
                                env,
                                enif_make_atom(env, "error"),
                                enif_make_atom(env, "iconv_failed")
                            );
                        default:
                            return enif_make_tuple2(
                                env,
                                enif_make_atom(env, "error"),
                                enif_make_tuple2(
                                    env,
                                    enif_make_atom(
                                        env, 
                                        "iconv_failed_iconv_close_weird_return_value"
                                    ),
                                    enif_make_int(env, errno)
                                )
                            );           
                    }
            }
        }
        break;
    }
    
    //printf("%u %u %u\n", out_bin.size, outbytesleft, out_bin.size - outbytesleft);
    
    if(enif_alloc_binary(out_bin.size - outbytesleft, &final_bin) == 0){
        enif_release_binary(&out_bin);
        enif_free(to);
        enif_free(from);
        res = iconv_close(conv_desc);
        switch(res) {
            case -1:
                return enif_make_tuple2(
                    env,
                    enif_make_atom(env, "error"),
                    enif_make_tuple2(
                        env,
                        enif_make_atom(
                            env, 
                            "alloc_binary_iconv_close_failed"
                        ),
                        enif_make_int(env, errno)
                    )
                );
                                 
            case 0:
                return enif_make_tuple2(
                    env,
                    enif_make_atom(env, "error"),
                    enif_make_atom(env, "alloc_binary")
                );

            default:
                return enif_make_tuple2(
                    env,
                    enif_make_atom(env, "error"),
                    enif_make_tuple2(
                        env,
                        enif_make_atom(
                            env, 
                            "alloc_binary_iconv_close_weird_return_value"
                        ),
                        enif_make_int(env, errno)
                    )
                );           
        }
    }

    memcpy(final_bin.data, out_bin.data, out_bin.size - outbytesleft);
    enif_release_binary(&out_bin);
    enif_free(to);
    enif_free(from);
    res = iconv_close(conv_desc);
    switch(res) {
        case -1:
            return enif_make_tuple2(
                env,
                enif_make_atom(env, "error"),
                enif_make_tuple2(
                    env,
                    enif_make_atom(
                        env, 
                        "iconv_close_failed"
                    ),
                    enif_make_int(env, errno)
                )
            );
                             
        case 0:
            break;

        default:
            return enif_make_tuple2(
                env,
                enif_make_atom(env, "error"),
                enif_make_tuple2(
                    env,
                    enif_make_atom(
                        env, 
                        "iconv_close_weird_return_value"
                    ),
                    enif_make_int(env, errno)
                )
            );           
    }

    return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_binary(env, &final_bin));
}


ERL_NIF_INIT(iconverl, nif_funcs, NULL, NULL, NULL, NULL);
