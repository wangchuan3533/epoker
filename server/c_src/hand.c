#include "hand.h"
#include <stdio.h>

static unsigned int masks5[] = {0x1f};
static unsigned int masks6[] = {0x1f, 0x2f, 0x37, 0x3b, 0x3d, 0x3e};
static unsigned int masks7[] = {
    0x1f, 0x2f, 0x37, 0x3b, 0x3d, 0x3e, 0x4f,
    0x57, 0x5b, 0x5d, 0x5e, 0x67, 0x6b, 0x6d,
    0x6e, 0x73, 0x75, 0x76, 0x79, 0x7a, 0x7c,
};

static struct {
    unsigned int *masks;
    unsigned int n_masks;
} masks_arr[] = {
    {NULL, 0},
    {NULL, 0},
    {NULL, 0},
    {NULL, 0},
    {NULL, 0},
    {masks5, sizeof(masks5) / sizeof(unsigned int)},
    {masks6, sizeof(masks6) / sizeof(unsigned int)},
    {masks7, sizeof(masks6) / sizeof(unsigned int)},
};

unsigned int _calc_rank(card_t *cards);

hand_rank_t calc_rank(card_t *hand, unsigned int n)
{
    card_t picked[CARD_NUM];
    hand_rank_t max_rank;
    unsigned int i, j, k, score;
    unsigned int *masks = masks_arr[n].masks, n_masks = masks_arr[n].n_masks;

    max_rank.score = 0;
    max_rank.mask = 0;
    for (i = 0; i < n_masks; i++) {
        for (j = 0, k = 0; j < n; j++) {
            if (masks[i] & (1u << j)) {
                picked[k++] = hand[j];
            }
        }
        
        score = _calc_rank(picked);
        if (score > max_rank.score) {
            max_rank.score = score;
            max_rank.mask = masks[i];
        }
    }
    return max_rank;
}

unsigned int _calc_rank(card_t *hand)
{
    int i, j, numbers[CARD_BASE] = {0}, suits[4] = {0}, statistics[CARD_NUM] = {0};
    int flush = 0, straight = 0;
    score_t score;

    // prepare
    for (i = 0; i < CARD_NUM; i++) {
        numbers[hand[i] % CARD_BASE]++;
        suits[hand[i] / CARD_BASE]++;
    }

    // statistics
    for (i = 0; i < CARD_BASE; i++) {
        statistics[numbers[i]]++;
    }

    // check flush
    for (i = 0; i < 4; i++) {
        if (suits[i] == CARD_NUM) {
            flush = 1;
        }
    }

    // check straight
    if (!(statistics[2] || statistics[3] || statistics[4])) {
        for (i = 0; numbers[i] == 0 && i < CARD_BASE; i++);
        for (j = 0; j < 5 && i + j < CARD_BASE; j++) {
            if (!numbers[i + j]) {
                break;
            }
        }
        if (j == 5) {
            straight = 1;
        } else if (i == 0 && j == 4 && numbers[CARD_BASE - 1] > 0) {
            straight = 1;
            numbers[CARD_BASE - 1] = 0;
        }
    }

    // calculate score
    score = 0;
    for (i = 4; i > 0; i--) {
        if (statistics[i] == 0) {
            continue;
        }
        for (j = CARD_BASE - 1; j >= 0; j--) {
            if (numbers[j] == i) {
                score <<= 4;
                score |= j;
            }
        }
    }

    // calc levels
    if (flush && straight) {
        SET_RANK(score, STRAIGHT_FLUSH);
    } else if (statistics[4]) {
        SET_RANK(score, FOUR_OF_A_KIND);
    } else if (statistics[3] && statistics[2]) {
        SET_RANK(score, FULL_HORSE);
    } else if (flush) {
        SET_RANK(score, FLUSH);
    } else if (straight) {
        SET_RANK(score, STRAIGHT);
    } else if (statistics[3]) {
        SET_RANK(score, THREE_OF_A_KIND);
    } else if (statistics[2] >= 2) {
        SET_RANK(score, TWO_PAIR);
    } else if (statistics[2]) {
        SET_RANK(score, ONE_PAIR);
    } else {
        SET_RANK(score, HIGH_CARD);
    }
    return score;
}
