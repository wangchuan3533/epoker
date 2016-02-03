#include "erl_nif.h"
#include "hand.h"

static ERL_NIF_TERM eval(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    unsigned int value;
    unsigned char cards[7];
    int i, len;

    const ERL_NIF_TERM *array;
    ERL_NIF_TERM score, mask;
    hand_rank_t result;
    
    if (!enif_get_tuple(env, argv[0], &len, &array)) {
        return enif_make_badarg(env);
    }

    for (i = 0; i < len; i++) {
        if (!enif_get_uint(env, array[i], &value)) {
            return enif_make_badarg(env);
        }
        cards[i++] = value;
    }
    
    result = calc_rank(cards, len);
    score = enif_make_uint(env, result.score);
    mask = enif_make_uint(env, result.mask);
      
    return enif_make_tuple2(env, score, mask);
}

static ErlNifFunc nif_funcs[] =
{
    {"eval", 1, eval}
};

ERL_NIF_INIT(hand, nif_funcs, NULL, NULL, NULL, NULL)
