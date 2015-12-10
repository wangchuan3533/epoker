#include "erl_nif.h"
#include "hand.h"

static ERL_NIF_TERM eval(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    unsigned int len, i, value;
    unsigned char cards[7];
    ERL_NIF_TERM list, head, tail, score, mask;
    hand_rank_t result;
    
    if (!enif_get_list_length(env, argv[0], &len)) {
        return enif_make_badarg(env);
    }
    if (len != 7) {
        return enif_make_badarg(env);
    }
    
    for (list = argv[0], i = 0; enif_get_list_cell(env, list, &head, &tail); list = tail) {
        if (!enif_get_uint(env, head, &value)) {
            return enif_make_badarg(env);
        }
        cards[i++] = value;
    }
    
    result = calc_rank(cards);
    score = enif_make_uint(env, result.score);
    mask = enif_make_uint(env, result.mask);
      
    return enif_make_tuple2(env, score, mask);
}

static ErlNifFunc nif_funcs[] =
{
    {"eval", 1, eval}
};

ERL_NIF_INIT(hand, nif_funcs, NULL, NULL, NULL, NULL)
