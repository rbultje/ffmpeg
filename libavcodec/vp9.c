/*
 * VP9 compatible video decoder
 *
 * Copyright (C) 2013 Ronald S. Bultje
 *
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#include "avcodec.h"
#include "get_bits.h"
#include "internal.h"
#include "videodsp.h"
#include "vp56.h"
#include "vp9.h"
#include "vp9data.h"
#include "vp9dsp.h"

enum FilterMode {
    FILTER_8TAP_SMOOTH,
    FILTER_8TAP_REGULAR,
    FILTER_8TAP_SHARP,
    FILTER_BILINEAR,
    FILTER_SWITCHABLE,
};

enum CompPredMode {
    PRED_SINGLEREF,
    PRED_COMPREF,
    PRED_SWITCHABLE,
};

// FIXME maybe invert order here, may make some calculations down under
// a cycle faster or so
enum BlockLevel {
    BL_64X64,
    BL_32X32,
    BL_16X16,
    BL_8X8,
};

typedef struct VP9Context {
    VP9DSPContext dsp;
    VideoDSPContext vdsp;
    GetBitContext gb;
    VP56RangeCoder c;
    AVFrame *f;

    // bitstream header
    unsigned profile:2;
    unsigned keyframe:1;
    unsigned invisible:1;
    unsigned errorres:1;
    unsigned colorspace:3;
    unsigned fullrange:1;
    unsigned intraonly:1;
    unsigned resetctx:2;
    unsigned refreshrefmask:8;
    unsigned highprecisionmvs:1;
    enum FilterMode filtermode:3;
    unsigned allowcompinter:1;
    unsigned fixcompref:2;
    unsigned refreshctx:1;
    unsigned parallelmode:1;
    unsigned framectxid:2;
    uint8_t refidx[3] /* :3 */;
    uint8_t signbias[3] /* :1 */;
    uint8_t varcompref[2] /* :2 */;
    AVFrame *refs[8], *cur_frame;

    struct {
        unsigned level:6;
        int8_t sharpness;
        uint8_t lim_lut[64];
        uint8_t mblim_lut[64];
    } filter;
    struct {
        unsigned enabled:1;
        int8_t mode[2] /* :6+1 */;
        int8_t ref[4] /* :6+1 */;
    } lf_delta;
    uint8_t yac_qi;
    int8_t ydc_qdelta, uvdc_qdelta, uvac_qdelta;
    unsigned lossless:1;
    struct {
        unsigned enabled:1;
        unsigned temporal:1;
        unsigned absolute_vals:1;
        unsigned update_map:1;
        struct {
            unsigned q_enabled:1;
            unsigned lf_enabled:1;
            unsigned ref_enabled:1;
            unsigned skip_enabled:1;
            unsigned ref_val:2;
            int16_t q_val /* :8+1 */;
            int8_t lf_val /* :6+1 */;
            int16_t qmul[2][2];
            uint8_t lflvl[4][2];
        } feat[8];
    } segmentation;
    struct {
        unsigned log2_tile_cols, log2_tile_rows;
        unsigned tile_cols, tile_rows;
        unsigned tile_row_start, tile_row_end, tile_col_start, tile_col_end;
    } tiling;
    unsigned sb_cols, sb_rows, rows, cols;
    struct {
        prob_context p;
        uint8_t coef[4][2][2][6][6][3];
    } prob_ctx[4];
    struct {
        prob_context p;
        uint8_t coef[4][2][2][6][6][11];
        uint8_t seg[7];
        uint8_t segpred[3];
    } prob;
    struct {
        unsigned y_mode[4][10];
        unsigned uv_mode[10][10];
        unsigned filter[4][3];
        unsigned mv_mode[7][4];
        unsigned intra[4][2];
        unsigned comp[5][2];
        unsigned single_ref[5][2][2];
        unsigned comp_ref[5][2];
        unsigned tx32p[2][4];
        unsigned tx16p[2][3];
        unsigned tx8p[2][2];
        unsigned skip[3][2];
        unsigned mv_joint[4];
        struct {
            unsigned sign[2];
            unsigned classes[11];
            unsigned class0[2];
            unsigned bits[11];
            unsigned class0_fp[2][4];
            unsigned fp[4];
            unsigned class0_hp[2];
            unsigned hp[2];
        } mv_comp[2];
        unsigned partition[4][4][4];
        unsigned coef[4][2][2][6][6][3];
        unsigned eob[4][2][2][6][6][2];
    } counts;
    enum TxfmMode txfmmode:3;
    enum CompPredMode comppredmode:2;
    uint8_t left_partition_ctx[8], *above_partition_ctx;
    uint8_t left_skip_ctx[8], *above_skip_ctx;
    uint8_t left_txfm_ctx[8], *above_txfm_ctx;
    uint8_t left_mode_ctx[16], *above_mode_ctx;
    uint8_t left_y_nnz_ctx[16], *above_y_nnz_ctx;
    uint8_t left_uv_nnz_ctx[2][8], *above_uv_nnz_ctx[2];
    uint8_t *intra_pred_data[3];
    DECLARE_ALIGNED(16, int16_t, block)[4096];
    DECLARE_ALIGNED(16, int16_t, uvblock)[2][1024];
} VP9Context;

static int update_size(AVCodecContext *ctx, int w, int h)
{
    VP9Context *s = ctx->priv_data;

    if (s->above_partition_ctx && w == ctx->width && h == ctx->height)
        return 0;

    ctx->width  = w;
    ctx->height = h;
    s->sb_cols  = (w + 63) >> 6;
    s->sb_rows  = (h + 63) >> 6;
    s->cols     = (w + 7) >> 3;
    s->rows     = (h + 7) >> 3;
    av_free(s->above_partition_ctx);
    s->above_partition_ctx = av_malloc(s->sb_cols * 200);
    s->above_skip_ctx = s->above_partition_ctx + s->sb_cols * 8;
    s->above_txfm_ctx = s->above_skip_ctx + s->sb_cols * 8;
    s->above_mode_ctx = s->above_txfm_ctx + s->sb_cols * 8;
    s->above_y_nnz_ctx = s->above_mode_ctx + s->sb_cols * 16;
    s->above_uv_nnz_ctx[0] = s->above_y_nnz_ctx + s->sb_cols * 16;
    s->above_uv_nnz_ctx[1] = s->above_uv_nnz_ctx[0] + s->sb_cols * 8;
    s->intra_pred_data[0] = s->above_uv_nnz_ctx[1] + s->sb_cols * 8;
    s->intra_pred_data[1] = s->intra_pred_data[0] + s->sb_cols * 64;
    s->intra_pred_data[2] = s->intra_pred_data[1] + s->sb_cols * 32;

    return 0;
}

// for some reason the sign bit is at the end, not the start, of a bit sequence
static av_always_inline int get_sbits_inv(GetBitContext *gb, int n)
{
    int v = get_bits(gb, n);
    return get_bits1(gb) ? -v : v;
}

static av_always_inline int inv_recenter_nonneg(int v, int m)
{
    return v > 2 * m ? v : v & 1 ? m - ((v + 1) >> 1) : m + (v >> 1);
}

// differential forward probability updates
static int update_prob(VP56RangeCoder *c, int p)
{
    static const int inv_map_table[254] = {
          7,  20,  33,  46,  59,  72,  85,  98, 111, 124, 137, 150, 163, 176,
        189, 202, 215, 228, 241, 254,   1,   2,   3,   4,   5,   6,   8,   9,
         10,  11,  12,  13,  14,  15,  16,  17,  18,  19,  21,  22,  23,  24,
         25,  26,  27,  28,  29,  30,  31,  32,  34,  35,  36,  37,  38,  39,
         40,  41,  42,  43,  44,  45,  47,  48,  49,  50,  51,  52,  53,  54,
         55,  56,  57,  58,  60,  61,  62,  63,  64,  65,  66,  67,  68,  69,
         70,  71,  73,  74,  75,  76,  77,  78,  79,  80,  81,  82,  83,  84,
         86,  87,  88,  89,  90,  91,  92,  93,  94,  95,  96,  97,  99, 100,
        101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 112, 113, 114, 115,
        116, 117, 118, 119, 120, 121, 122, 123, 125, 126, 127, 128, 129, 130,
        131, 132, 133, 134, 135, 136, 138, 139, 140, 141, 142, 143, 144, 145,
        146, 147, 148, 149, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160,
        161, 162, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175,
        177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 190, 191,
        192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 203, 204, 205, 206,
        207, 208, 209, 210, 211, 212, 213, 214, 216, 217, 218, 219, 220, 221,
        222, 223, 224, 225, 226, 227, 229, 230, 231, 232, 233, 234, 235, 236,
        237, 238, 239, 240, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251,
        252, 253,
    };
    int d;

    if (!vp8_rac_get(c)) {
        d = vp8_rac_get_uint(c, 4) + 0;
    } else if (!vp8_rac_get(c)) {
        d = vp8_rac_get_uint(c, 4) + 16;
    } else if (!vp8_rac_get(c)) {
        d = vp8_rac_get_uint(c, 5) + 32;
    } else {
        d = vp8_rac_get_uint(c, 7);
        if (d >= 65)
            d = (d << 1) - 65 + vp8_rac_get(c);
        d += 64;
    }

    return p <= 128 ? 1 + inv_recenter_nonneg(inv_map_table[d], p - 1) :
                    255 - inv_recenter_nonneg(inv_map_table[d], 255 - p);
}

#define VP9_SYNCCODE 0x498342
static int decode_frame_header(AVCodecContext *ctx,
                               const uint8_t *data, int size)
{
    VP9Context *s = ctx->priv_data;
    int c, i, j, k, l, m, n, w, h, max, size2, res, sharp;
    const uint8_t *data2;

    /* general header */
    init_get_bits(&s->gb, data, size << 3);
    if (get_bits(&s->gb, 2) != 0x2) // frame marker
        return AVERROR_INVALIDDATA;
    s->profile = get_bits1(&s->gb);
    if (get_bits1(&s->gb)) // reserved bit
        return AVERROR_INVALIDDATA;
    if (get_bits1(&s->gb)) {
        int ref = get_bits(&s->gb, 3);
        printf("Directly show frame %d\n", ref);
        return -1;
    }
    s->keyframe  = !get_bits1(&s->gb);
    s->invisible = !get_bits1(&s->gb);
    s->errorres  = get_bits1(&s->gb);
    if (s->keyframe) {
        if (get_bits_long(&s->gb, 24) != VP9_SYNCCODE) // synccode
            return AVERROR_INVALIDDATA;
        s->colorspace = get_bits(&s->gb, 3);
        if (s->colorspace == 7) // RGB = profile 1
            return AVERROR_INVALIDDATA;
        s->fullrange  = get_bits1(&s->gb);
        // for profile 1, here follows the subsampling bits
        s->refreshrefmask = 0xff;
        w = get_bits(&s->gb, 16) + 1;
        h = get_bits(&s->gb, 16) + 1;
        if (get_bits1(&s->gb)) // display size
            skip_bits(&s->gb, 32);
    } else {
        s->intraonly  = s->invisible ? get_bits1(&s->gb) : 0;
        s->resetctx   = s->errorres ? 0 : get_bits(&s->gb, 2);
        if (s->intraonly) {
            if (get_bits_long(&s->gb, 24) != VP9_SYNCCODE) // synccode
                return AVERROR_INVALIDDATA;
            s->refreshrefmask = get_bits(&s->gb, 8);
            w = get_bits(&s->gb, 16) + 1;
            h = get_bits(&s->gb, 16) + 1;
            if (get_bits1(&s->gb)) // display size
                skip_bits(&s->gb, 32);
        } else {
            s->refreshrefmask = get_bits(&s->gb, 8);
            s->refidx[0]    = get_bits(&s->gb, 3);
            s->signbias[0]  = get_bits1(&s->gb);
            s->refidx[1]    = get_bits(&s->gb, 3);
            s->signbias[1]  = get_bits1(&s->gb);
            s->refidx[2]    = get_bits(&s->gb, 3);
            s->signbias[2]  = get_bits1(&s->gb);
            if (!s->refs[s->refidx[0]] || !s->refs[s->refidx[1]] ||
                !s->refs[s->refidx[2]])
                return AVERROR_INVALIDDATA;
            if (get_bits1(&s->gb)) {
                w = s->refs[s->refidx[0]]->width;
                h = s->refs[s->refidx[0]]->height;
            } else if (get_bits1(&s->gb)) {
                w = s->refs[s->refidx[1]]->width;
                h = s->refs[s->refidx[1]]->height;
            } else if (get_bits1(&s->gb)) {
                w = s->refs[s->refidx[2]]->width;
                h = s->refs[s->refidx[2]]->height;
            } else {
                w = get_bits(&s->gb, 16) + 1;
                h = get_bits(&s->gb, 16) + 1;
            }
            if (get_bits1(&s->gb)) // display size
                skip_bits(&s->gb, 32);
            s->highprecisionmvs = get_bits1(&s->gb);
            s->filtermode = get_bits1(&s->gb) ? FILTER_SWITCHABLE :
                                                get_bits(&s->gb, 2);
            s->allowcompinter = s->signbias[0] != s->signbias[1] ||
                                s->signbias[0] != s->signbias[2];
            if (s->allowcompinter) {
                if (s->signbias[0] == s->signbias[1]) {
                    s->fixcompref    = 2;
                    s->varcompref[0] = 0;
                    s->varcompref[1] = 1;
                } else if (s->signbias[0] == s->signbias[2]) {
                    s->fixcompref    = 1;
                    s->varcompref[0] = 0;
                    s->varcompref[1] = 2;
                } else {
                    s->fixcompref    = 0;
                    s->varcompref[0] = 1;
                    s->varcompref[1] = 2;
                }
            }
        }
    }
    s->refreshctx   = s->errorres ? 0 : get_bits1(&s->gb);
    s->parallelmode = s->errorres ? 1 : get_bits1(&s->gb);
    s->framectxid   = c = get_bits(&s->gb, 2);

    /* loopfilter header data */
    s->filter.level = get_bits(&s->gb, 6);
    sharp = get_bits(&s->gb, 3);
    // if sharpness changed, reinit lim/mblim LUTs. if it didn't change, keep
    // the old cache values since they are still valid
    if (s->filter.sharpness != sharp)
        memset(s->filter.lim_lut, 0, sizeof(s->filter.lim_lut));
    s->filter.sharpness = sharp;
    if ((s->lf_delta.enabled = get_bits1(&s->gb))) {
        if (get_bits1(&s->gb)) {
            for (i = 0; i < 4; i++)
                if (get_bits1(&s->gb))
                    s->lf_delta.ref[i] = get_sbits_inv(&s->gb, 6);
            for (i = 0; i < 2; i++)
                if (get_bits1(&s->gb))
                    s->lf_delta.mode[i] = get_sbits_inv(&s->gb, 6);
        }
    } else {
        memset(&s->lf_delta, 0, sizeof(s->lf_delta));
    }

    /* quantization header data */
    s->yac_qi = get_bits(&s->gb, 8);
    if (get_bits1(&s->gb))
        s->ydc_qdelta = get_sbits_inv(&s->gb, 4);
    if (get_bits1(&s->gb))
        s->uvdc_qdelta = get_sbits_inv(&s->gb, 4);
    if (get_bits1(&s->gb))
        s->uvac_qdelta = get_sbits_inv(&s->gb, 4);
    s->lossless = s->yac_qi == 0 && s->ydc_qdelta == 0 &&
                  s->uvdc_qdelta == 0 && s->uvac_qdelta == 0;

    /* segmentation header info */
    if ((s->segmentation.enabled = get_bits1(&s->gb))) {
        if ((s->segmentation.update_map = get_bits1(&s->gb))) {
            for (i = 0; i < 7; i++)
                s->prob.seg[i] = get_bits1(&s->gb) ?
                                 get_bits(&s->gb, 8) : 255;
            if ((s->segmentation.temporal = get_bits1(&s->gb))) {
                for (i = 0; i < 3; i++)
                    s->prob.segpred[i] = get_bits1(&s->gb) ?
                                         get_bits(&s->gb, 8) : 255;
            }
        }

        if (get_bits1(&s->gb)) {
            s->segmentation.absolute_vals = get_bits1(&s->gb);
            for (i = 0; i < 8; i++) {
                if ((s->segmentation.feat[i].q_enabled = get_bits1(&s->gb)))
                    s->segmentation.feat[i].q_val = get_sbits_inv(&s->gb, 8);
                if ((s->segmentation.feat[i].lf_enabled = get_bits1(&s->gb)))
                    s->segmentation.feat[i].lf_val = get_sbits_inv(&s->gb, 6);
                if ((s->segmentation.feat[i].ref_enabled = get_bits1(&s->gb)))
                    s->segmentation.feat[i].ref_val = get_bits(&s->gb, 2);
                s->segmentation.feat[i].skip_enabled = get_bits1(&s->gb);
            }
        }
    } else {
        s->segmentation.feat[0].q_enabled = 0;
        s->segmentation.feat[0].lf_enabled = 0;
        s->segmentation.feat[0].skip_enabled = 0;
        s->segmentation.feat[0].ref_enabled = 0;
    }

    // set qmul[] based on Y/UV, AC/DC and segmentation Q idx deltas
    for (i = 0; i < (s->segmentation.enabled ? 8 : 1); i++) {
        int qyac, qydc, quvac, quvdc, lflvl, sh;

        if (s->segmentation.feat[i].q_enabled) {
            if (s->segmentation.absolute_vals)
                qyac = s->segmentation.feat[i].q_val;
            else
                qyac = s->yac_qi + s->segmentation.feat[i].q_val;
        } else {
            qyac  = s->yac_qi;
        }
        qydc  = av_clip_uintp2(qyac + s->ydc_qdelta, 8);
        quvdc = av_clip_uintp2(qyac + s->uvdc_qdelta, 8);
        quvac = av_clip_uintp2(qyac + s->uvac_qdelta, 8);
        qyac  = av_clip_uintp2(qyac, 8);

        s->segmentation.feat[i].qmul[0][0] = vp9_dc_qlookup[qydc];
        s->segmentation.feat[i].qmul[0][1] = vp9_ac_qlookup[qyac];
        s->segmentation.feat[i].qmul[1][0] = vp9_dc_qlookup[quvdc];
        s->segmentation.feat[i].qmul[1][1] = vp9_ac_qlookup[quvac];

        sh = s->filter.level >= 32;
        if (s->segmentation.feat[i].lf_enabled) {
            if (s->segmentation.absolute_vals)
                lflvl = s->segmentation.feat[i].lf_val;
            else
                lflvl = s->filter.level + s->segmentation.feat[i].lf_val;
        } else {
            lflvl  = s->filter.level;
        }
        s->segmentation.feat[i].lflvl[0][0] = s->segmentation.feat[i].lflvl[1][0] =
            av_clip_uintp2(lflvl + (s->lf_delta.ref[0] << sh), 6);
        // FIXME inter mode loopfilter deltas
    }

    /* tiling info */
    if ((res = update_size(ctx, w, h)) < 0)
        return res;
    for (s->tiling.log2_tile_cols = 0;
         (s->sb_cols >> s->tiling.log2_tile_cols) > 64;
         s->tiling.log2_tile_cols++) ;
    for (max = 0; (s->sb_cols >> max) >= 4; max++) ;
    max = FFMAX(0, max - 1);
    while (max-- > s->tiling.log2_tile_cols) {
        if (get_bits1(&s->gb))
            s->tiling.log2_tile_cols++;
        else
            break;
    }
    s->tiling.log2_tile_rows = get_bits1(&s->gb);
    if (s->tiling.log2_tile_rows)
        s->tiling.log2_tile_rows += get_bits1(&s->gb);
    s->tiling.tile_rows = 1 << s->tiling.log2_tile_rows;
    s->tiling.tile_cols = 1 << s->tiling.log2_tile_cols;

    if (s->keyframe || s->errorres || s->intraonly) {
        s->prob_ctx[0].p = s->prob_ctx[1].p = s->prob_ctx[2].p =
                           s->prob_ctx[3].p = vp9_default_probs;
        memcpy(s->prob_ctx[0].coef, vp9_default_coef_probs,
               sizeof(vp9_default_coef_probs));
        memcpy(s->prob_ctx[1].coef, vp9_default_coef_probs,
               sizeof(vp9_default_coef_probs));
        memcpy(s->prob_ctx[2].coef, vp9_default_coef_probs,
               sizeof(vp9_default_coef_probs));
        memcpy(s->prob_ctx[3].coef, vp9_default_coef_probs,
               sizeof(vp9_default_coef_probs));
    }

    // next 16 bits is size of the rest of the header (arith-coded)
    size2 = get_bits(&s->gb, 16);
    data2 = align_get_bits(&s->gb);
    if (size2 > size - (data2 - data))
        return AVERROR_INVALIDDATA;
    ff_vp56_init_range_decoder(&s->c, data2, size2);
    if (vp56_rac_get_prob_branchy(&s->c, 128)) // marker bit
        return AVERROR_INVALIDDATA;

    // FIXME for keyframes we don't need to zero all of this - most is unused
    memset(&s->counts, 0, sizeof(s->counts));
    // FIXME is it faster to not copy here, but do it down in the fw updates
    // as explicit copies if the fw update is missing (and skip the copy upon
    // fw update)?
    s->prob.p = s->prob_ctx[c].p;

    // txfm updates
    if (s->lossless) {
        s->txfmmode = TX_4X4;
    } else {
        s->txfmmode = vp8_rac_get_uint(&s->c, 2);
        if (s->txfmmode == 3)
            s->txfmmode += vp8_rac_get(&s->c);

        if (s->txfmmode == TX_SWITCHABLE) {
            for (i = 0; i < 2; i++)
                if (vp56_rac_get_prob_branchy(&s->c, 252))
                    s->prob.p.tx8p[i] = update_prob(&s->c, s->prob.p.tx8p[i]);
            for (i = 0; i < 2; i++)
                for (j = 0; j < 2; j++)
                    if (vp56_rac_get_prob_branchy(&s->c, 252))
                        s->prob.p.tx16p[i][j] =
                            update_prob(&s->c, s->prob.p.tx16p[i][j]);
            for (i = 0; i < 2; i++)
                for (j = 0; j < 3; j++)
                    if (vp56_rac_get_prob_branchy(&s->c, 252))
                        s->prob.p.tx32p[i][j] =
                            update_prob(&s->c, s->prob.p.tx32p[i][j]);
        }
    }

    // coef updates
    for (i = 0; i < 4; i++) {
        uint8_t (*ref)[2][6][6][3] = s->prob_ctx[c].coef[i];
        if (vp8_rac_get(&s->c)) {
            for (j = 0; j < 2; j++)
                for (k = 0; k < 2; k++)
                    for (l = 0; l < 6; l++)
                        for (m = 0; m < 6; m++) {
                            uint8_t *p = s->prob.coef[i][j][k][l][m];
                            uint8_t *r = ref[j][k][l][m];
                            if (m >= 3 && l == 0) // dc only has 3 pt
                                break;
                            for (n = 0; n < 3; n++) {
                                if (vp56_rac_get_prob_branchy(&s->c, 252)) {
                                    p[n] = update_prob(&s->c, r[n]);
                                } else {
                                    p[n] = r[n];
                                }
                            }
                            p[3] = 0;
                        }
        } else {
            for (j = 0; j < 2; j++)
                for (k = 0; k < 2; k++)
                    for (l = 0; l < 6; l++)
                        for (m = 0; m < 6; m++) {
                            uint8_t *p = s->prob.coef[i][j][k][l][m];
                            uint8_t *r = ref[j][k][l][m];
                            if (m > 3 && l == 0) // dc only has 3 pt
                                break;
                            memcpy(p, r, 3);
                            p[3] = 0;
                        }
        }
        if (s->txfmmode == i)
            break;
    }

    // mode updates
    for (i = 0; i < 3; i++)
        if (vp56_rac_get_prob_branchy(&s->c, 252)) {
            s->prob.p.skip[i] = update_prob(&s->c, s->prob.p.skip[i]);
        }
    if (!s->keyframe && !s->intraonly) {
        for (i = 0; i < 7; i++)
            for (j = 0; j < 3; j++)
                if (vp56_rac_get_prob_branchy(&s->c, 252))
                    s->prob.p.mv_mode[i][j] =
                        update_prob(&s->c, s->prob.p.mv_mode[i][j]);

        if (s->filtermode == FILTER_SWITCHABLE)
            for (i = 0; i < 4; i++)
                for (j = 0; j < 2; j++)
                    if (vp56_rac_get_prob_branchy(&s->c, 252))
                        s->prob.p.filter[i][j] =
                            update_prob(&s->c, s->prob.p.filter[i][j]);

        for (i = 0; i < 4; i++)
            if (vp56_rac_get_prob_branchy(&s->c, 252))
                s->prob.p.intra[i] = update_prob(&s->c, s->prob.p.intra[i]);

        if (s->allowcompinter) {
            s->comppredmode = vp8_rac_get(&s->c);
            if (s->comppredmode)
                s->comppredmode += vp8_rac_get(&s->c);
            if (s->comppredmode == PRED_SWITCHABLE)
                for (i = 0; i < 5; i++)
                    if (vp56_rac_get_prob_branchy(&s->c, 252))
                        s->prob.p.comp[i] =
                            update_prob(&s->c, s->prob.p.comp[i]);
        } else {
            s->comppredmode = PRED_SINGLEREF;
        }

        if (s->comppredmode != PRED_COMPREF) {
            for (i = 0; i < 5; i++) {
                if (vp56_rac_get_prob_branchy(&s->c, 252))
                    s->prob.p.single_ref[i][0] =
                        update_prob(&s->c, s->prob.p.single_ref[i][0]);
                if (vp56_rac_get_prob_branchy(&s->c, 252))
                    s->prob.p.single_ref[i][1] =
                        update_prob(&s->c, s->prob.p.single_ref[i][1]);
            }
        }

        if (s->comppredmode != PRED_SINGLEREF) {
            for (i = 0; i < 5; i++)
                if (vp56_rac_get_prob_branchy(&s->c, 252))
                    s->prob.p.comp_ref[i] =
                        update_prob(&s->c, s->prob.p.comp_ref[i]);
        }

        // FIXME the order of y/uv modes is different in libvpx versus here,
        // so I think this will update the prob tables in the wrong order
        for (i = 0; i < 4; i++)
            for (j = 0; j < 9; j++)
                if (vp56_rac_get_prob_branchy(&s->c, 252))
                    s->prob.p.y_mode[i][j] =
                        update_prob(&s->c, s->prob.p.y_mode[i][j]);

        for (i = 0; i < 10; i++)
            for (j = 0; j < 9; j++)
                if (vp56_rac_get_prob_branchy(&s->c, 252))
                    s->prob.p.uv_mode[i][j] =
                        update_prob(&s->c, s->prob.p.uv_mode[i][j]);

        for (i = 0; i < 4; i++)
            for (j = 0; j < 4; j++)
                for (k = 0; k < 3; k++)
                    if (vp56_rac_get_prob_branchy(&s->c, 252))
                        s->prob.p.partition[i][j][k] =
                            update_prob(&s->c, s->prob.p.partition[i][j][k]);

        // mv fields don't use the update_prob subexp model for some reason
        for (i = 0; i < 3; i++)
            if (vp56_rac_get_prob_branchy(&s->c, 252))
                s->prob.p.mv_joint[i] = (vp8_rac_get_uint(&s->c, 7) << 1) | 1;

        for (i = 0; i < 2; i++) {
            if (vp56_rac_get_prob_branchy(&s->c, 252))
                s->prob.p.mv_comp[i].sign = (vp8_rac_get_uint(&s->c, 7) << 1) | 1;

            for (j = 0; j < 10; j++)
                if (vp56_rac_get_prob_branchy(&s->c, 252))
                    s->prob.p.mv_comp[i].classes[j] =
                        (vp8_rac_get_uint(&s->c, 7) << 1) | 1;

            if (vp56_rac_get_prob_branchy(&s->c, 252))
                s->prob.p.mv_comp[i].class0 = (vp8_rac_get_uint(&s->c, 7) << 1) | 1;

            for (j = 0; j < 10; j++)
                if (vp56_rac_get_prob_branchy(&s->c, 252))
                    s->prob.p.mv_comp[i].bits[j] =
                        (vp8_rac_get_uint(&s->c, 7) << 1) | 1;
        }

        for (i = 0; i < 2; i++) {
            for (j = 0; j < 2; j++)
                for (k = 0; k < 3; k++)
                    if (vp56_rac_get_prob_branchy(&s->c, 252))
                        s->prob.p.mv_comp[i].class0_fp[j][k] =
                            (vp8_rac_get_uint(&s->c, 7) << 1) | 1;

            for (j = 0; j < 3; j++)
                if (vp56_rac_get_prob_branchy(&s->c, 252))
                    s->prob.p.mv_comp[i].fp[j] =
                        (vp8_rac_get_uint(&s->c, 7) << 1) | 1;
        }

        if (s->highprecisionmvs) {
            for (i = 0; i < 2; i++) {
                if (vp56_rac_get_prob_branchy(&s->c, 252))
                    s->prob.p.mv_comp[i].class0_hp =
                        (vp8_rac_get_uint(&s->c, 7) << 1) | 1;

                if (vp56_rac_get_prob_branchy(&s->c, 252))
                    s->prob.p.mv_comp[i].hp =
                        (vp8_rac_get_uint(&s->c, 7) << 1) | 1;
            }
        }
    }

    return (data2 - data) + size2;
}

typedef struct {
    int seg_id, mode[4], uvmode, skip;
    enum BlockLevel bl;
    enum BlockPartition bp;
    enum TxfmMode tx, uvtx;
} VP9Block;

// the padding is to be able to distinguish between things that act on
// a per-8x8 block basis (like partitioning etc.), and things that act
// on a per-4x4 block basis (like intra mode).
static const uint8_t bwh_tab[5][4][2] = {
    { { 16, 16 }, { 16, 8 }, { 8, 16 } },
    { { 8, 8 }, { 8, 4 }, { 4, 8 } },
    { { 4, 4 }, { 4, 2 }, { 2, 4 } },
    { { 2, 2 }, { 2, 1 }, { 1, 2 }, { 1, 1 } },
    // For the sub8x8 case, these constants are only used for things that
    // act on a per-8x8 block basis, and thus we round everything up to
    // at least 1
    { { 1, 1 }, { 1, 1 }, { 1, 1 }, { 1, 1 } },
};

static int decode_mode(AVCodecContext *ctx, int row, int col, VP9Block *b)
{
    static const uint8_t left_ctx[4][4] = {
        { 0x0, 0x8, 0x0 }, { 0x8, 0xc, 0x8 }, { 0xc, 0xe, 0xc },
        { 0xe, 0xf, 0xe, 0xf }
    };
    static const uint8_t above_ctx[4][4] = {
        { 0x0, 0x0, 0x8 }, { 0x8, 0x8, 0xc }, { 0xc, 0xc, 0xe },
        { 0xe, 0xe, 0xf, 0xf }
    };
    static const uint8_t max_tx_for_bl_bp[4][4] = {
        { TX_32X32, TX_32X32, TX_32X32 }, { TX_32X32, TX_16X16, TX_16X16 },
        { TX_16X16, TX_8X8, TX_8X8 }, { TX_8X8, TX_4X4, TX_4X4, TX_4X4 }
    };
    VP9Context *s = ctx->priv_data;
    enum TxfmMode max_tx = max_tx_for_bl_bp[b->bl][b->bp];

    if (s->keyframe || s->intraonly) {
        uint8_t *a, *l;

        b->seg_id = s->segmentation.enabled && s->segmentation.update_map ?
            vp8_rac_get_tree(&s->c, vp9_segmentation_tree, s->prob.seg) : 0;
        b->skip = s->segmentation.enabled &&
            s->segmentation.feat[b->seg_id].skip_enabled;
        if (!b->skip) {
            int c = s->left_skip_ctx[row & 7] + s->above_skip_ctx[col];
            b->skip = vp56_rac_get_prob(&s->c, s->prob.p.skip[c]);
            s->counts.skip[c][b->skip]++;
        }
        // FIXME share this code with inter coding
        if (s->txfmmode == TX_SWITCHABLE) {
            int c;
            if (row > 0) {
                if (col > s->tiling.tile_col_start) {
                    c = (s->above_skip_ctx[col] ? max_tx :
                         s->above_txfm_ctx[col]) +
                        (s->left_skip_ctx[row & 7] ? max_tx :
                         s->left_txfm_ctx[row & 7]) > max_tx;
                } else {
                    c = s->above_skip_ctx[col] ? 1 :
                        (s->above_txfm_ctx[col] * 2 > max_tx);
                }
            } else {
                if (col > s->tiling.tile_col_start) {
                    c = s->left_skip_ctx[row & 7] ? 1 :
                        (s->left_txfm_ctx[row & 7] * 2 > max_tx);
                } else {
                    c = 1;
                }
            }
            switch (max_tx) {
            case TX_32X32:
                b->tx = vp56_rac_get_prob(&s->c, s->prob.p.tx32p[c][0]);
                if (b->tx) {
                    b->tx += vp56_rac_get_prob(&s->c, s->prob.p.tx32p[c][1]);
                    if (b->tx == 2)
                        b->tx += vp56_rac_get_prob(&s->c, s->prob.p.tx32p[c][2]);
                }
                s->counts.tx32p[c][b->tx]++;
                break;
            case TX_16X16:
                b->tx = vp56_rac_get_prob(&s->c, s->prob.p.tx16p[c][0]);
                if (b->tx)
                    b->tx += vp56_rac_get_prob(&s->c, s->prob.p.tx16p[c][1]);
                s->counts.tx16p[c][b->tx]++;
                break;
            case TX_8X8:
                b->tx = vp56_rac_get_prob(&s->c, s->prob.p.tx8p[c]);
                s->counts.tx8p[c][b->tx]++;
                break;
            case TX_4X4:
                b->tx = TX_4X4;
                break;
            }
        } else {
            b->tx = FFMIN(max_tx, s->txfmmode);
        }
        a = &s->above_mode_ctx[col * 2];
        l = &s->left_mode_ctx[(row & 7) << 1];
        if (b->bl == BL_8X8 && b->bp != PARTITION_NONE) {
            // FIXME the memory storage intermediates here aren't really
            // necessary, they're just there to make the code slightly
            // simpler for now
            b->mode[0] = a[0] = vp8_rac_get_tree(&s->c, vp9_intramode_tree,
                                    vp9_default_kf_ymode_probs[a[0]][l[0]]);
            if (b->bp != PARTITION_H) {
                b->mode[1] = vp8_rac_get_tree(&s->c, vp9_intramode_tree,
                                 vp9_default_kf_ymode_probs[a[1]][b->mode[0]]);
                l[0] = a[1] = b->mode[1];
            } else {
                l[0] = a[1] = b->mode[1] = b->mode[0];
            }
            if (b->bp != PARTITION_V) {
                b->mode[2] = a[0] = vp8_rac_get_tree(&s->c, vp9_intramode_tree,
                                        vp9_default_kf_ymode_probs[a[0]][l[1]]);
                if (b->bp != PARTITION_H) {
                    b->mode[3] = vp8_rac_get_tree(&s->c, vp9_intramode_tree,
                                  vp9_default_kf_ymode_probs[a[1]][b->mode[2]]);
                    l[1] = a[1] = b->mode[3];
                } else {
                    l[1] = a[1] = b->mode[3] = b->mode[2];
                }
            } else {
                b->mode[2] = b->mode[0];
                l[1] = a[1] = b->mode[3] = b->mode[1];
            }
        } else {
            b->mode[0] = vp8_rac_get_tree(&s->c, vp9_intramode_tree,
                                          vp9_default_kf_ymode_probs[*a][*l]);
            b->mode[3] = b->mode[2] = b->mode[1] = b->mode[0];
            // FIXME this can probably be optimized
            memset(a, b->mode[0], bwh_tab[b->bl][b->bp][0]);
            memset(l, b->mode[0], bwh_tab[b->bl][b->bp][1]);
        }
        b->uvmode = vp8_rac_get_tree(&s->c, vp9_intramode_tree,
                                     vp9_default_kf_uvmode_probs[b->mode[3]]);
    } else {
        // FIXME unimplemented inter coding
        printf("Inter frames not yet working\n");
        return -1;
    }

    // FIXME this can probably be optimized
    memset(&s->above_skip_ctx[col], b->skip, bwh_tab[b->bl + 1][b->bp][0]);
    memset(&s->left_skip_ctx[row & 7], b->skip, bwh_tab[b->bl + 1][b->bp][1]);
    memset(&s->above_txfm_ctx[col], b->tx, bwh_tab[b->bl + 1][b->bp][0]);
    memset(&s->left_txfm_ctx[row & 7], b->tx, bwh_tab[b->bl + 1][b->bp][1]);
    memset(&s->above_partition_ctx[col], above_ctx[b->bl][b->bp],
           bwh_tab[b->bl + 1][b->bp][0]);
    memset(&s->left_partition_ctx[row & 7], left_ctx[b->bl][b->bp],
           bwh_tab[b->bl + 1][b->bp][1]);

    return 0;
}

// FIXME remove tx argument, and merge cnt/eob arguments?
static int decode_coeffs_b(VP56RangeCoder *c, int16_t *coef, int n_coeffs,
                           enum TxfmMode tx, unsigned (*cnt)[6][3],
                           unsigned (*eob)[6][2], uint8_t (*p)[6][11],
                           int nnz, const int16_t *scan, const int16_t (*nb)[2],
                           const int16_t *band_counts, const int16_t *qmul)
{
    int i = 0, band = 0, band_left = band_counts[band];
    uint8_t *tp = p[0][nnz];
    uint8_t cache[1024];

    do {
        int val, rc;

        val = vp56_rac_get_prob_branchy(c, tp[0]); // eob
        eob[band][nnz][val]++;
        if (!val)
            break;

    skip_eob:
        if (!vp56_rac_get_prob_branchy(c, tp[1])) { // zero
            cnt[band][nnz][0]++;
            if (!--band_left)
                band_left = band_counts[++band];
            cache[scan[i]] = 0;
            nnz = (1 + cache[nb[i][0]] + cache[nb[i][1]]) >> 1;
            tp = p[band][nnz];
            if (++i == n_coeffs)
                break; //invalid input; blocks should end with EOB
            goto skip_eob;
        }

        rc = scan[i];
        if (!vp56_rac_get_prob_branchy(c, tp[2])) { // one
            cnt[band][nnz][1]++;
            val = 1;
            cache[rc] = 1;
        } else {
            // fill in p[3-10] (model fill) - only once per frame for each pos
            if (!tp[3])
                memcpy(&tp[3], vp9_model_pareto8[tp[2]], 8);

            cnt[band][nnz][2]++;
            if (!vp56_rac_get_prob_branchy(c, tp[3])) { // 2, 3, 4
                if (!vp56_rac_get_prob_branchy(c, tp[4])) {
                    cache[rc] = val = 2;
                } else {
                    val = 3 + vp56_rac_get_prob(c, tp[5]);
                    cache[rc] = 3;
                }
            } else if (!vp56_rac_get_prob_branchy(c, tp[6])) { // cat1/2
                cache[rc] = 4;
                if (!vp56_rac_get_prob_branchy(c, tp[7])) {
                    val = 5 + vp56_rac_get_prob(c, 159);
                } else {
                    val = 7 + (vp56_rac_get_prob(c, 165) << 1) +
                               vp56_rac_get_prob(c, 145);
                }
            } else { // cat 3-6
                cache[rc] = 5;
                if (!vp56_rac_get_prob_branchy(c, tp[8])) {
                    if (!vp56_rac_get_prob_branchy(c, tp[9])) {
                        val = 11 + (vp56_rac_get_prob(c, 173) << 2) +
                                   (vp56_rac_get_prob(c, 148) << 1) +
                                    vp56_rac_get_prob(c, 140);
                    } else {
                        val = 19 + (vp56_rac_get_prob(c, 176) << 3) +
                                   (vp56_rac_get_prob(c, 155) << 2) +
                                   (vp56_rac_get_prob(c, 140) << 1) +
                                    vp56_rac_get_prob(c, 135);
                    }
                } else if (!vp56_rac_get_prob_branchy(c, tp[10])) {
                    val = 35 + (vp56_rac_get_prob(c, 180) << 4) +
                               (vp56_rac_get_prob(c, 157) << 3) +
                               (vp56_rac_get_prob(c, 141) << 2) +
                               (vp56_rac_get_prob(c, 134) << 1) +
                                vp56_rac_get_prob(c, 130);
                } else {
                    val = 67 + (vp56_rac_get_prob(c, 254) << 13) +
                               (vp56_rac_get_prob(c, 254) << 12) +
                               (vp56_rac_get_prob(c, 254) << 11) +
                               (vp56_rac_get_prob(c, 252) << 10) +
                               (vp56_rac_get_prob(c, 249) << 9) +
                               (vp56_rac_get_prob(c, 243) << 8) +
                               (vp56_rac_get_prob(c, 230) << 7) +
                               (vp56_rac_get_prob(c, 196) << 6) +
                               (vp56_rac_get_prob(c, 177) << 5) +
                               (vp56_rac_get_prob(c, 153) << 4) +
                               (vp56_rac_get_prob(c, 140) << 3) +
                               (vp56_rac_get_prob(c, 133) << 2) +
                               (vp56_rac_get_prob(c, 130) << 1) +
                                vp56_rac_get_prob(c, 129);
                }
            }
        }
        if (!--band_left)
            band_left = band_counts[++band];
        if (tx == TX_32X32) // FIXME slow
            coef[rc] = ((vp8_rac_get(c) ? -val : val) * qmul[!!i]) / 2;
        else
            coef[rc] = (vp8_rac_get(c) ? -val : val) * qmul[!!i];
        nnz = (1 + cache[nb[i][0]] + cache[nb[i][1]]) >> 1;
        tp = p[band][nnz];
    } while (++i < n_coeffs);

    return !!i;
}

static int decode_coeffs(AVCodecContext *ctx, VP9Block *b, int row, int col)
{
    VP9Context *s = ctx->priv_data;
    uint8_t (*p)[6][11] = s->prob.coef[b->tx][0 /* y */][0 /* ref */];
    unsigned (*c)[6][3] = s->counts.coef[b->tx][0 /* y */][0 /* ref */];
    unsigned (*e)[6][2] = s->counts.eob[b->tx][0 /* y */][0 /* ref */];
    // FIXME skip fully out-of-visible-area tx-blocks
    int w4 = bwh_tab[b->bl + 1][b->bp][0] << 1;
    int h4 = bwh_tab[b->bl + 1][b->bp][1] << 1;
    int n, pl, x, y, step1d = 1 << b->tx, step = 1 << (b->tx * 2);
    int uvstep1d = 1 << b->uvtx, uvstep = 1 << (b->uvtx * 2), res;
    int16_t (*qmul)[2] = s->segmentation.feat[b->seg_id].qmul;
    const int16_t **yscans = vp9_scans[b->tx];
    const int16_t (**ynbs)[2] = vp9_scans_nb[b->tx];
    const int16_t *uvscan = vp9_scans[b->uvtx][DCT_DCT];
    const int16_t (*uvnb)[2] = vp9_scans_nb[b->uvtx][DCT_DCT];
    uint8_t *a = &s->above_y_nnz_ctx[col * 2];
    uint8_t *l = &s->left_y_nnz_ctx[(row & 7) << 1];
    static const int16_t band_counts[4][6] = {
        { 1, 2, 3, 4,  3,   16 - 13 },
        { 1, 2, 3, 4, 11,   64 - 21 },
        { 1, 2, 3, 4, 11,  256 - 21 },
        { 1, 2, 3, 4, 11, 1024 - 21 },
    };
    const int16_t *y_band_counts = band_counts[b->tx];
    const int16_t *uv_band_counts = band_counts[b->uvtx];

    /* y tokens */
    if (b->tx > TX_4X4) { // FIXME slow
        for (y = 0; y < h4; y += step1d)
            for (x = 1; x < step1d; x++)
                l[y] |= l[y + x];
        for (x = 0; x < w4; x += step1d)
            for (y = 1; y < step1d; y++)
                a[x] |= a[x + y];
    }
    for (n = 0, y = 0; y < h4; y += step1d) {
        for (x = 0; x < w4; x += step1d, n += step) {
            enum TxfmType txtp = vp9_intra_txfm_type[b->mode[b->tx == TX_4X4 &&
                                                             b->bl == BL_8X8 ?
                                                             n : 0]];
            int nnz = a[x] + l[y];
            if ((res = decode_coeffs_b(&s->c, s->block + 16 * n, 16 * step,
                                       b->tx, c, e, p, nnz, yscans[txtp],
                                       ynbs[txtp], y_band_counts, qmul[0])) < 0)
                return res;
            a[x] = l[y] = res;
        }
    }
    if (b->tx > TX_4X4) { // FIXME slow
        for (y = 0; y < h4; y += step1d)
            memset(&l[y + 1], l[y], step1d - 1);
        for (x = 0; x < w4; x += step1d)
            memset(&a[x + 1], a[x], step1d - 1);
    }

    p = s->prob.coef[b->uvtx][1 /* uv */][0 /* ref */];
    c = s->counts.coef[b->uvtx][1 /* uv */][0 /* ref */];
    e = s->counts.eob[b->uvtx][1 /* uv */][0 /* ref */];
    w4 >>= 1;
    h4 >>= 1;
    for (pl = 0; pl < 2; pl++) {
        a = &s->above_uv_nnz_ctx[pl][col];
        l = &s->left_uv_nnz_ctx[pl][row & 7];
        if (b->uvtx > TX_4X4) { // FIXME slow
            for (y = 0; y < h4; y += uvstep1d)
                for (x = 1; x < uvstep1d; x++)
                    l[y] |= l[y + x];
            for (x = 0; x < w4; x += uvstep1d)
                for (y = 1; y < uvstep1d; y++)
                    a[x] |= a[x + y];
        }
        for (n = 0, y = 0; y < h4; y += uvstep1d) {
            for (x = 0; x < w4; x += uvstep1d, n += uvstep) {
                int nnz = a[x] + l[y];
                if ((res = decode_coeffs_b(&s->c, s->uvblock[pl] + 16 * n,
                                           16 * uvstep, b->uvtx, c, e, p, nnz,
                                           uvscan, uvnb, uv_band_counts,
                                           qmul[1])) < 0)
                    return res;
                a[x] = l[y] = res;
            }
        }
        if (b->uvtx > TX_4X4) { // FIXME slow
            for (y = 0; y < h4; y += uvstep1d)
                memset(&l[y + 1], l[y], uvstep1d - 1);
            for (x = 0; x < w4; x += uvstep1d)
                memset(&a[x + 1], a[x], uvstep1d - 1);
        }
    }

    return 0;
}

static av_always_inline int check_intra_mode(VP9Context *s, int mode, uint8_t **a,
                                             uint8_t *dst, ptrdiff_t stride,
                                             uint8_t *l, int col, int x, int w,
                                             int row, int y, enum TxfmMode tx,
                                             int p)
{
    // FIXME make work with tiling enabled (have_left check is wrong)
    int have_top = row * 2 + y > 0, have_left = col * 2 + x > 0, have_topright = x < w - 1;
    static const uint8_t mode_conv[10][2 /* have_left */][2 /* have_top */] = {
        [VERT_PRED]            = { { DC_127_PRED,          VERT_PRED },
                                   { DC_127_PRED,          VERT_PRED } },
        [HOR_PRED]             = { { DC_129_PRED,          DC_129_PRED },
                                   { HOR_PRED,             HOR_PRED } },
        [DC_PRED]              = { { DC_128_PRED,          TOP_DC_PRED },
                                   { LEFT_DC_PRED,         DC_PRED } },
        [DIAG_DOWN_LEFT_PRED]  = { { DC_127_PRED,          DIAG_DOWN_LEFT_PRED },
                                   { DC_127_PRED,          DIAG_DOWN_LEFT_PRED } },
        [DIAG_DOWN_RIGHT_PRED] = { { DIAG_DOWN_RIGHT_PRED, DIAG_DOWN_RIGHT_PRED },
                                   { DIAG_DOWN_RIGHT_PRED, DIAG_DOWN_RIGHT_PRED } },
        [VERT_RIGHT_PRED]      = { { VERT_RIGHT_PRED,      VERT_RIGHT_PRED },
                                   { VERT_RIGHT_PRED,      VERT_RIGHT_PRED } },
        [HOR_DOWN_PRED]        = { { HOR_DOWN_PRED,        HOR_DOWN_PRED },
                                   { HOR_DOWN_PRED,        HOR_DOWN_PRED } },
        [VERT_LEFT_PRED]       = { { DC_127_PRED,          VERT_LEFT_PRED },
                                   { DC_127_PRED,          VERT_LEFT_PRED } },
        [HOR_UP_PRED]          = { { DC_129_PRED,          DC_129_PRED },
                                   { HOR_UP_PRED,          HOR_UP_PRED } },
        [TM_VP8_PRED]          = { { DC_129_PRED,          VERT_PRED },
                                   { HOR_PRED,             TM_VP8_PRED } },
    };
    static const struct {
        uint8_t needs_left:1;
        uint8_t needs_top:1;
        uint8_t needs_topleft:1;
        uint8_t needs_topright:1;
    } edges[N_INTRA_PRED_MODES] = {
        [VERT_PRED]            = { .needs_top  = 1 },
        [HOR_PRED]             = { .needs_left = 1 },
        [DC_PRED]              = { .needs_top  = 1, .needs_left = 1 },
        [DIAG_DOWN_LEFT_PRED]  = { .needs_top  = 1, .needs_topright = 1 },
        [DIAG_DOWN_RIGHT_PRED] = { .needs_left = 1, .needs_top = 1, .needs_topleft = 1 },
        [VERT_RIGHT_PRED]      = { .needs_left = 1, .needs_top = 1, .needs_topleft = 1 },
        [HOR_DOWN_PRED]        = { .needs_left = 1, .needs_top = 1, .needs_topleft = 1 },
        [VERT_LEFT_PRED]       = { .needs_top  = 1, .needs_topright = 1 },
        [HOR_UP_PRED]          = { .needs_left = 1 },
        [TM_VP8_PRED]          = { .needs_left = 1, .needs_top = 1, .needs_topleft = 1 },
        [LEFT_DC_PRED]         = { .needs_left = 1 },
        [TOP_DC_PRED]          = { .needs_top  = 1 },
        [DC_128_PRED]          = { 0 },
        [DC_127_PRED]          = { 0 },
        [DC_129_PRED]          = { 0 }
    };

    assert(mode >= 0 && mode < 10);
    mode = mode_conv[mode][have_left][have_top];
    if (edges[mode].needs_top) {
        uint8_t *top;

        // if top of sb64-row, use s->intra_pred_data[] instead of
        // dst[-stride] for intra prediction (it contains pre- instead of
        // post-loopfilter data)
        if (have_top)
            top = !(row & 7) && !y ?
                s->intra_pred_data[p] + col * (8 >> !!p) + x * 4 : &dst[-stride];

        if (have_top &&
            (!edges[mode].needs_topleft || have_left) &&
            (tx != TX_4X4 || !edges[mode].needs_topright || have_topright)) {
            *a = top;
        } else {
            if (have_top) {
                memcpy(*a, top, 4 << tx);
            } else {
                memset(*a, 127, 4 << tx);
            }
            if (edges[mode].needs_topleft) {
                if (have_left && have_top) {
                    (*a)[-1] = top[-1];
                } else {
                    (*a)[-1] = have_top ? 129 : 127;
                }
            }
            if (tx == TX_4X4 && edges[mode].needs_topright) {
                if (have_topright) {
                    memcpy(&(*a)[4], &top[4], 4);
                } else {
                    memset(&(*a)[4], (*a)[3], 4);
                }
            }
        }
    }
    if (edges[mode].needs_left) {
        if (have_left) {
            int i;

            for (i = 0; i < (4 << tx); i++) {
                l[i] = dst[i * stride - 1];
            }
        } else {
            memset(l, 129, 4 << tx);
        }
    }

    return mode;
}

static void intra_recon(AVCodecContext *ctx, VP9Block *b, int row, int col,
                        ptrdiff_t yoff, ptrdiff_t uvoff)
{
    VP9Context *s = ctx->priv_data;
    int w4 = bwh_tab[b->bl + 1][b->bp][0] << 1, step1d = 1 << b->tx, n;
    int h4 = bwh_tab[b->bl + 1][b->bp][1] << 1, x, y, step = 1 << (b->tx * 2);
    int uvstep1d = 1 << b->uvtx, p;
    uint8_t *dst = s->f->data[0] + yoff;

    // FIXME skip blocks fully out of visible area
    // FIXME fix overhangs (e.g. if pos+32>stride, then we need to emulate
    //       frame width, else we destroy pixels; for height, it also writes
    //       beyond allocated buffer edges)
    for (n = 0, y = 0; y < h4; y += step1d) {
        uint8_t *ptr = dst;
        for (x = 0; x < w4; x += step1d, ptr += 4 * step1d, n += step) {
            int mode = b->mode[b->bl == BL_8X8 && b->tx == TX_4X4 ?
                               y * 2 + x : 0];
            // FIXME alignment
            uint8_t a_buf[48], *a = &a_buf[16], l[32];
            enum TxfmType txtp = vp9_intra_txfm_type[mode];

            mode = check_intra_mode(s, mode, &a, ptr, s->f->linesize[0], l,
                                    col, x, w4, row, y, b->tx, 0);
            s->dsp.intra_pred[b->tx][mode](ptr, s->f->linesize[0], l, a);
            // FIXME eob
            // FIXME lossless
            s->dsp.itxfm_add[b->tx][txtp](ptr, s->f->linesize[0],
                                          s->block + 16 * n, 16 * step);
        }
        dst += 4 * s->f->linesize[0] * step1d;
    }

    // U/V
    h4 >>= 1;
    w4 >>= 1;
    step = 1 << (b->uvtx * 2);
    for (p = 0; p < 2; p++) {
        dst = s->f->data[1 + p] + uvoff;
        for (n = 0, y = 0; y < h4; y += uvstep1d) {
            uint8_t *ptr = dst;
            for (x = 0; x < w4; x += uvstep1d, ptr += 4 * uvstep1d, n += step) {
                int mode = b->uvmode;
                // FIXME alignment
                uint8_t a_buf[48], *a = &a_buf[16], l[32];

                mode = check_intra_mode(s, mode, &a, ptr, s->f->linesize[1], l,
                                        col, x, w4, row, y, b->uvtx, p + 1);
                s->dsp.intra_pred[b->uvtx][mode](ptr, s->f->linesize[1], l, a);
                // FIXME eob
                // FIXME lossless
                s->dsp.itxfm_add[b->uvtx][DCT_DCT](ptr, s->f->linesize[1],
                                                   s->uvblock[p] + 16 * n,
                                                   16 * step);
            }
            dst += 4 * uvstep1d * s->f->linesize[1];
        }
    }
}

struct VP9Filter {
    uint8_t level[8 * 8];
    uint8_t /* bit=col */ mask[2 /* 0=y, 1=uv */][2 /* 0=col, 1=row */]
                              [8 /* rows */][4 /* 0=16, 1=8, 2=4, 3=inner4 */];
};

static av_always_inline void mask_edges(struct VP9Filter *lflvl, int is_uv,
                                        int row_and_7, int col_and_7,
                                        int w, int h, enum TxfmMode tx)
{
    // FIXME for inter blocks, also skip loopfilter across edges inside a
    // predictor block if we're a skipblock (i.e. no coefficients)

    // FIXME I'm pretty sure all loops can be replaced by a single LUT if
    // we make VP9Filter.mask uint64_t (i.e. row/col all single variable)
    // and make the LUT 5-indexed (bl, bp, is_uv, tx and row/col), and then
    // use row_and_7/col_and_7 as shifts (1*col_and_7+8*row_and_7)

    if (tx == TX_4X4) {
        int t = 1 << col_and_7, m_col = (t << w) - t, y;

        // on 32-px edges, use the 8-px wide loopfilter; else, use 4-px wide
        if (is_uv) {
            int m_row_8 = m_col & 0x01, m_row_4 = m_col - m_row_8;

            for (y = row_and_7; y < h + row_and_7; y++) {
                int col_mask_id = 2 - !(y & 7);

                lflvl->mask[is_uv][0][y][1] |= m_row_8;
                lflvl->mask[is_uv][0][y][2] |= m_row_4;
                lflvl->mask[is_uv][1][y][col_mask_id] |= m_col;
            }
        } else {
            int m_row_8 = m_col & 0x11, m_row_4 = m_col - m_row_8;

            for (y = row_and_7; y < h + row_and_7; y++) {
                int col_mask_id = 2 - !(y & 3);

                lflvl->mask[is_uv][0][y][1] |= m_row_8; // row edge
                lflvl->mask[is_uv][0][y][2] |= m_row_4;
                lflvl->mask[is_uv][1][y][col_mask_id] |= m_col; // col edge
                lflvl->mask[is_uv][0][y][3] |= m_col;
                lflvl->mask[is_uv][1][y][3] |= m_col;
            }
        }
    } else {
        static const unsigned masks[4] = { 0xff, 0x55, 0x11, 0x01 };
        int l2 = tx + is_uv - 1, step1d = 1 << l2, mask_id = (tx == TX_8X8), y;
        int t = 1 << col_and_7, m_col = (t << w) - t, m_row = m_col & masks[l2];

        for (y = row_and_7; y < h + row_and_7; y++)
            lflvl->mask[is_uv][0][y][mask_id] |= m_row;
        for (y = row_and_7; y < h + row_and_7; y += step1d)
            lflvl->mask[is_uv][1][y][mask_id] |= m_col;
    }
}

// FIXME maybe merge bl/bp into a single argument from this point onwards,
// since we never use them separately above/in this function, only below
// (in decode_sb())
static int decode_b(AVCodecContext *ctx, int row, int col,
                    struct VP9Filter *lflvl, ptrdiff_t yoff, ptrdiff_t uvoff,
                    enum BlockLevel bl, enum BlockPartition bp)
{
    VP9Context *s = ctx->priv_data;
    VP9Block b;
    int res, y, w4 = bwh_tab[bl + 1][bp][0], h4 = bwh_tab[bl + 1][bp][1], lvl;

    b.bl = bl;
    b.bp = bp;
    if ((res = decode_mode(ctx, row, col, &b)) < 0)
        return res;
    b.uvtx = b.tx - (w4 * 2 == (1 << b.tx) || h4 * 2 == (1 << b.tx));

    if (!b.skip) {
        if ((res = decode_coeffs(ctx, &b, row, col)) < 0)
            return res;
    } else {
        int pl;

        memset(&s->above_y_nnz_ctx[col * 2], 0, w4 * 2);
        memset(&s->left_y_nnz_ctx[(row & 7) << 1], 0, h4 * 2);
        for (pl = 0; pl < 2; pl++) {
            memset(&s->above_uv_nnz_ctx[pl][col], 0, w4);
            memset(&s->left_uv_nnz_ctx[pl][row & 7], 0, h4);
        }
    }
    // FIXME inter predict
    intra_recon(ctx, &b, row, col, yoff, uvoff);

    // pick filter level and find edges to apply filter to
    if (s->filter.level &&
        (lvl = s->segmentation.feat[b.seg_id].lflvl[0][0]) > 0) {
        for (y = 0; y < h4; y++)
            memset(&lflvl->level[((row & 7) + y) * 8 + (col & 7)], lvl, w4);
        mask_edges(lflvl, 0, row & 7, col & 7, w4, h4, b.tx);
        mask_edges(lflvl, 1, row & 7, col & 7, w4, h4, b.uvtx);

        if (!s->filter.lim_lut[lvl]) {
            int sharp = s->filter.sharpness;
            int limit = lvl;

            if (sharp > 0) {
                limit >>= (sharp + 3) >> 2;
                limit = FFMIN(limit, 9 - sharp);
            }
            limit = FFMAX(limit, 1);

            s->filter.lim_lut[lvl] = limit;
            s->filter.mblim_lut[lvl] = 2 * (lvl + 2) + limit;
        }
    }

    return 0;
}

static int decode_sb(AVCodecContext *ctx, int row, int col, struct VP9Filter *lflvl,
                     ptrdiff_t yoff, ptrdiff_t uvoff, enum BlockLevel bl)
{
    VP9Context *s = ctx->priv_data;
    int c = ((s->above_partition_ctx[col] >> (3 - bl)) & 1) |
            (((s->left_partition_ctx[row & 0x7] >> (3 - bl)) & 1) << 1), res;
    const uint8_t *p = s->keyframe ? vp9_default_kf_partition_probs[bl][c] :
                                     s->prob.p.partition[bl][c];
    enum BlockPartition bp;
    ptrdiff_t hbs = 4 >> bl;

    if (bl == BL_8X8) {
        bp = vp8_rac_get_tree(&s->c, vp9_partition_tree, p);
        res = decode_b(ctx, row, col, lflvl, yoff, uvoff, bl, bp);
    } else if (col + hbs < s->cols) {
        if (row + hbs < s->rows) {
            bp = vp8_rac_get_tree(&s->c, vp9_partition_tree, p);
            switch (bp) {
            case PARTITION_NONE:
                res = decode_b(ctx, row, col, lflvl, yoff, uvoff, bl, bp);
                break;
            case PARTITION_H:
                if (!(res = decode_b(ctx, row, col, lflvl, yoff, uvoff, bl, bp))) {
                    yoff  += hbs * 8 * s->f->linesize[0];
                    uvoff += hbs * 4 * s->f->linesize[1];
                    res = decode_b(ctx, row + hbs, col, lflvl, yoff, uvoff, bl, bp);
                }
                break;
            case PARTITION_V:
                if (!(res = decode_b(ctx, row, col, lflvl, yoff, uvoff, bl, bp))) {
                    yoff  += hbs * 8;
                    uvoff += hbs * 4;
                    res = decode_b(ctx, row, col + hbs, lflvl, yoff, uvoff, bl, bp);
                }
                break;
            case PARTITION_SPLIT:
                if (!(res = decode_sb(ctx, row, col, lflvl, yoff, uvoff, bl + 1))) {
                    if (!(res = decode_sb(ctx, row, col + hbs, lflvl,
                                          yoff + 8 * hbs, uvoff + 4 * hbs, bl + 1))) {
                        yoff  += hbs * 8 * s->f->linesize[0];
                        uvoff += hbs * 4 * s->f->linesize[1];
                        if (!(res = decode_sb(ctx, row + hbs, col, lflvl,
                                              yoff, uvoff, bl + 1)))
                            res = decode_sb(ctx, row + hbs, col + hbs, lflvl,
                                            yoff + 8 * hbs, uvoff + 4 * hbs, bl + 1);
                    }
                }
                break;
            }
        } else if (vp56_rac_get_prob_branchy(&s->c, p[1])) {
            bp = PARTITION_SPLIT;
            if (!(res = decode_sb(ctx, row, col, lflvl, yoff, uvoff, bl + 1)))
                res = decode_sb(ctx, row, col + hbs, lflvl,
                                yoff + 8 * hbs, uvoff + 4 * hbs, bl + 1);
        } else {
            bp = PARTITION_H;
            res = decode_b(ctx, row, col, lflvl, yoff, uvoff, bl, bp);
        }
    } else if (row + hbs < s->rows) {
        if (vp56_rac_get_prob_branchy(&s->c, p[2])) {
            bp = PARTITION_SPLIT;
            if (!(res = decode_sb(ctx, row, col, lflvl, yoff, uvoff, bl + 1))) {
                yoff  += hbs * 8 * s->f->linesize[0];
                uvoff += hbs * 4 * s->f->linesize[1];
                res = decode_sb(ctx, row + hbs, col, lflvl,
                                yoff, uvoff, bl + 1);
            }
        } else {
            bp = PARTITION_V;
            res = decode_b(ctx, row, col, lflvl, yoff, uvoff, bl, bp);
        }
    } else {
        bp = PARTITION_SPLIT;
        res = decode_sb(ctx, row, col, lflvl, yoff, uvoff, bl + 1);
    }
    s->counts.partition[bl][c][bp]++;

    return res;
}

static void loopfilter_sb(AVCodecContext *ctx, struct VP9Filter *lflvl,
                          int row, int col, ptrdiff_t yoff, ptrdiff_t uvoff)
{
    VP9Context *s = ctx->priv_data;
    uint8_t *dst = s->f->data[0] + yoff, *lvl = lflvl->level;
    ptrdiff_t ls_y = s->f->linesize[0], ls_uv = s->f->linesize[1];
    int y, x, p;

    // FIXME in how far can we interleave the v/h loopfilter calls? E.g.
    // if you think of them as acting on a 8x8 block max, we can interleave
    // each v/h within the single x loop, but that only works if we work on
    // 8 pixel blocks, and we won't always do that (we want at least 16px
    // to use SSE2 optimizations, perhaps 32 for AVX2)

    // FIXME also don't loopfilter edges that are out of visible area

    // filter edges between columns, Y plane (e.g. block1 | block2)
    for (y = 0; y < 8; y += 2, dst += 16 * ls_y, lvl += 16) {
        uint8_t *ptr = dst, *l = lvl, *hmask1 = lflvl->mask[0][0][y];
        uint8_t *hmask2 = lflvl->mask[0][0][y + 1];
        unsigned hm1 = hmask1[0] | hmask1[1] | hmask1[2], hm13 = hmask1[3];
        unsigned hm2 = hmask2[1] | hmask2[2], hm23 = hmask2[3];
        unsigned hm = hm1 | hm2 | hm13 | hm23;

        for (x = 1; hm & ~(x - 1); x <<= 1, ptr += 8, l++) {
            if (hm1 & x) {
                int L = *l, H = L >> 4;
                int E = s->filter.mblim_lut[L], I = s->filter.lim_lut[L];

                if (col || x > 1) {
                    if (hmask1[0] & x) {
                        assert(hmask2[0] & x);
                        assert(l[8] == L);
                        s->dsp.loop_filter[2][0][1](ptr, ls_y, E, I, H);
                        hm2 &= ~x;
                    } else if (hmask1[1] & x) {
                        if ((hmask2[1] & x) && l[8] == L) {
                            s->dsp.loop_filter[1][0][1](ptr, ls_y, E, I, H);
                            hm2 &= ~x;
                        } else {
                            s->dsp.loop_filter[1][0][0](ptr, ls_y, E, I, H);
                        }
                    } else if (hmask1[2] & x) {
                        if ((hmask2[2] & x) && l[8] == L) {
                            s->dsp.loop_filter[0][0][1](ptr, ls_y, E, I, H);
                            hm2 &= ~x;
                        } else {
                            s->dsp.loop_filter[0][0][0](ptr, ls_y, E, I, H);
                        }
                    }
                }
            }
            if (hm2 & x) {
                int L = l[8], H = L >> 4;
                int E = s->filter.mblim_lut[L], I = s->filter.lim_lut[L];

                if (col || x > 1) {
                    if (hmask2[1] & x) {
                        s->dsp.loop_filter[1][0][0](ptr + 8 * ls_y, ls_y, E, I, H);
                    } else if (hmask2[2] & x) {
                        s->dsp.loop_filter[0][0][0](ptr + 8 * ls_y, ls_y, E, I, H);
                    }
                }
            }
            if (hm13 & x) {
                int L = *l, H = L >> 4;
                int E = s->filter.mblim_lut[L], I = s->filter.lim_lut[L];

                if (hmask1[3] & x) {
                    if ((hmask2[3] & x) && l[8] == L) {
                        s->dsp.loop_filter[0][0][1](ptr + 4, ls_y, E, I, H);
                        hm23 &= ~x;
                    } else {
                        s->dsp.loop_filter[0][0][0](ptr + 4, ls_y, E, I, H);
                    }
                }
            }
            if (hm23 & x) {
                int L = l[8], H = L >> 4;
                int E = s->filter.mblim_lut[L], I = s->filter.lim_lut[L];

                if (hmask2[3] & x) {
                    s->dsp.loop_filter[0][0][0](ptr + 8 * ls_y + 4, ls_y, E, I, H);
                }
            }
        }
    }

    //                                          block1
    // filter edges between rows, Y plane (e.g. ------)
    //                                          block2
    dst = s->f->data[0] + yoff;
    lvl = lflvl->level;
    for (y = 0; y < 8; y++, dst += 8 * ls_y, lvl += 8) {
        uint8_t *ptr = dst, *l = lvl, *vmask = lflvl->mask[0][1][y];
        unsigned vm = vmask[0] | vmask[1] | vmask[2], vm3 = vmask[3];

        for (x = 1; vm & ~(x - 1); x <<= 2, ptr += 16, l += 2) {
            if (row || y) {
                if (vm & x) {
                    int L = *l, H = L >> 4;
                    int E = s->filter.mblim_lut[L], I = s->filter.lim_lut[L];

                    if (vmask[0] & x) {
                        assert(vmask[0] & (x << 1));
                        assert(l[1] == L);
                        s->dsp.loop_filter[2][1][1](ptr, ls_y, E, I, H);
                        vm &= ~(x << 1);
                    } else if (vmask[1] & x) {
                        if ((vmask[1] & (x << 1)) && l[1] == L) {
                            s->dsp.loop_filter[1][1][1](ptr, ls_y, E, I, H);
                            vm &= ~(x << 1);
                        } else {
                            s->dsp.loop_filter[1][1][0](ptr, ls_y, E, I, H);
                        }
                    } else if (vmask[2] & x) {
                        if ((vmask[2] & (x << 1)) && l[1] == L) {
                            s->dsp.loop_filter[0][1][1](ptr, ls_y, E, I, H);
                            vm &= ~(x << 1);
                        } else {
                            s->dsp.loop_filter[0][1][0](ptr, ls_y, E, I, H);
                        }
                    }
                }

                if (vm & (x << 1)) {
                    int L = l[1], H = L >> 4;
                    int E = s->filter.mblim_lut[L], I = s->filter.lim_lut[L];

                    if (vmask[1] & (x << 1)) {
                        s->dsp.loop_filter[1][1][0](ptr + 8, ls_y, E, I, H);
                    } else if (vmask[2] & (x << 1)) {
                        s->dsp.loop_filter[0][1][0](ptr + 8, ls_y, E, I, H);
                    }
                }
            }
            if (vm3 & x) {
                int L = *l, H = L >> 4;
                int E = s->filter.mblim_lut[L], I = s->filter.lim_lut[L];

                if (vmask[3] & x) {
                    if ((x & 0x55) && (vmask[3] & (x << 1)) && l[1] == L) {
                        s->dsp.loop_filter[0][1][1](ptr + ls_y * 4, ls_y, E, I, H);
                        vm3 &= ~(x << 1);
                    } else {
                        s->dsp.loop_filter[0][1][0](ptr + ls_y * 4, ls_y, E, I, H);
                    }
                }
            }
            if (vm3 & (x << 1)) {
                int L = l[1], H = L >> 4;
                int E = s->filter.mblim_lut[L], I = s->filter.lim_lut[L];

                if (vmask[3] & (x << 1)) {
                    s->dsp.loop_filter[0][1][0](ptr + ls_y * 4 + 8, ls_y, E, I, H);
                }
            }
        }
    }

    // same principle but for U/V planes
    for (p = 0; p < 2; p++) {
        lvl = lflvl->level;
        dst = s->f->data[1 + p] + uvoff;
        for (y = 0; y < 8; y += 4, dst += 16 * ls_uv, lvl += 32) {
            uint8_t *ptr = dst, *l = lvl, *hmask1 = lflvl->mask[1][0][y];
            uint8_t *hmask2 = lflvl->mask[1][0][y + 2];
            unsigned hm1 = hmask1[0] | hmask1[1] | hmask1[2];
            unsigned hm2 = hmask2[1] | hmask2[2], hm = hm1 | hm2;

            for (x = 1; hm & ~(x - 1); x <<= 1, ptr += 4) {
                if ((col || x > 1) && (hm1 & x)) {
                    int L = *l, H = L >> 4;
                    int E = s->filter.mblim_lut[L], I = s->filter.lim_lut[L];

                    if (hmask1[0] & x) {
                        assert(hmask2[0] & x);
                        assert(l[16] == L);
                        s->dsp.loop_filter[2][0][1](ptr, ls_uv, E, I, H);
                        hm2 &= ~x;
                    } else if (hmask1[1] & x) {
                        if ((hmask2[1] & x) && l[16] == L) {
                            s->dsp.loop_filter[1][0][1](ptr, ls_uv, E, I, H);
                            hm2 &= ~x;
                        } else {
                            s->dsp.loop_filter[1][0][0](ptr, ls_uv, E, I, H);
                        }
                    } else if (hmask1[2] & x) {
                        if ((hmask2[2] & x) && l[16] == L) {
                            s->dsp.loop_filter[0][0][1](ptr, ls_uv, E, I, H);
                            hm2 &= ~x;
                        } else {
                            s->dsp.loop_filter[0][0][0](ptr, ls_uv, E, I, H);
                        }
                    }
                }
                if ((col || x > 1) && (hm2 & x)) {
                    int L = l[16], H = L >> 4;
                    int E = s->filter.mblim_lut[L], I = s->filter.lim_lut[L];

                    if (hmask2[1] & x) {
                        s->dsp.loop_filter[1][0][0](ptr + 8 * ls_uv, ls_uv, E, I, H);
                    } else if (hmask2[2] & x) {
                        s->dsp.loop_filter[0][0][0](ptr + 8 * ls_uv, ls_uv, E, I, H);
                    }
                }
                if (x & 0xAA)
                    l += 2;
            }
        }
        lvl = lflvl->level;
        dst = s->f->data[1 + p] + uvoff;
        for (y = 0; y < 8; y++, dst += 4 * ls_uv) {
            uint8_t *ptr = dst, *l = lvl, *vmask = lflvl->mask[1][1][y];
            unsigned vm = vmask[0] | vmask[1] | vmask[2];

            for (x = 1; vm & ~(x - 1); x <<= 2, ptr += 8, l += 2) {
                if ((row || y) && (vm & x)) {
                    int L = *l, H = L >> 4;
                    int E = s->filter.mblim_lut[L], I = s->filter.lim_lut[L];

                    if (vmask[0] & x) {
                        assert(x & 0x11);
                        assert(vmask[0] & (x << 2));
                        assert(l[2] == L);
                        s->dsp.loop_filter[2][1][1](ptr, ls_uv, E, I, H);
                        vm &= ~(x << 2);
                    } else if (vmask[1] & x) {
                        if ((x & 0x11) && (vmask[1] & (x << 2)) && l[2] == L) {
                            s->dsp.loop_filter[1][1][1](ptr, ls_uv, E, I, H);
                            vm &= ~(x << 2);
                        } else {
                            s->dsp.loop_filter[1][1][0](ptr, ls_uv, E, I, H);
                        }
                    } else if (vmask[2] & x) {
                        if ((x & 0x11) && (vmask[2] & (x << 2)) && l[2] == L) {
                            s->dsp.loop_filter[0][1][1](ptr, ls_uv, E, I, H);
                            vm &= ~(x << 2);
                        } else {
                            s->dsp.loop_filter[0][1][0](ptr, ls_uv, E, I, H);
                        }
                    }
                }
            }
            if (y & 1)
                lvl += 16;
        }
    }
}

static void set_tile_offset(int *start, int *end, int idx, int log2_n, int n)
{
    int sb_start = ( idx      * n) >> log2_n;
    int sb_end   = ((idx + 1) * n) >> log2_n;
    *start = FFMIN(sb_start, n) << 3;
    *end   = FFMIN(sb_end,   n) << 3;
}

static av_always_inline int adapt_prob(int p1, unsigned ct0, unsigned ct1,
                                       int max_count, int update_factor)
{
    unsigned ct = ct0 + ct1, p2;

    if (!ct)
        return p1;

    p2 = ((ct0 << 8) + (ct >> 1)) / ct;
    p2 = FFMAX(p2, 1);
    ct = FFMIN(ct, max_count);
    update_factor = FASTDIV(update_factor * ct, max_count);

    // (p1 * (256 - update_factor) + p2 * update_factor + 128) >> 8
    return p1 + (((p2 - p1) * update_factor + 128) >> 8);
}

static void adapt_probs(VP9Context *s)
{
    int i, j, k, l, m;

    for (i = 0; i < 4; i++)
        for (j = 0; j < 2; j++)
            for (k = 0; k < 2; k++)
                for (l = 0; l < 6; l++)
                    for (m = 0; m < 6; m++) {
                        uint8_t *p = s->prob_ctx[s->framectxid].coef[i][j][k][l][m];
                        unsigned *e = s->counts.eob[i][j][k][l][m];
                        unsigned *c = s->counts.coef[i][j][k][l][m];

                        if (l == 0 && m >= 3) // dc only has 3 pt
                            break;

                        // FIXME make '112' configurable b/c it's different
                        // for keyframes, non-keyframes directly following
                        // a keyframe, and subsequent non-keyframes
                        p[0] = adapt_prob(p[0], e[0], e[1], 24, 112);
                        p[1] = adapt_prob(p[1], c[0], c[1] + c[2], 24, 112);
                        p[2] = adapt_prob(p[2], c[1], c[2], 24, 112);
                    }

    if (s->keyframe || s->intraonly)
        return;

    // FIXME all other mode probability updates go here
}

static int vp9_decode_frame(AVCodecContext *ctx, void *out_pic,
                            int *got_frame, const uint8_t *data, int size)
{
    VP9Context *s = ctx->priv_data;
    int res, tile_row, tile_col;

    if ((res = decode_frame_header(ctx, data, size)) < 0)
        return res;
    data += res;
    size -= res;

    // FIXME keep track of reference frames, discard old references
    if ((res = ff_get_buffer(ctx, s->f, AV_GET_BUFFER_FLAG_REF)) < 0)
        return res;

    // main tile decode loop
    memset(s->above_partition_ctx, 0, s->cols);
    memset(s->above_skip_ctx, 0, s->cols);
    memset(s->above_mode_ctx, DC_PRED, s->cols * 2);
    memset(s->above_y_nnz_ctx, 0, s->cols * 2);
    memset(s->above_uv_nnz_ctx[0], 0, s->cols);
    memset(s->above_uv_nnz_ctx[1], 0, s->cols);
    // FIXME clean above contexts
    for (tile_row = 0; tile_row < s->tiling.tile_rows; tile_row++) {
        ptrdiff_t yoff, uvoff;
        set_tile_offset(&s->tiling.tile_row_start, &s->tiling.tile_row_end,
                        tile_row, s->tiling.log2_tile_rows, s->sb_rows);
        yoff  = s->f->linesize[0] * s->tiling.tile_row_start * 8;
        uvoff = s->f->linesize[1] * s->tiling.tile_row_start * 4;
        for (tile_col = 0; tile_col < s->tiling.tile_cols; tile_col++) {
            unsigned tile_size;
            int row, col;
            ptrdiff_t yoff2, uvoff2;

            set_tile_offset(&s->tiling.tile_col_start, &s->tiling.tile_col_end,
                            tile_col, s->tiling.log2_tile_cols, s->sb_cols);
            if (tile_col == s->tiling.tile_cols - 1 &&
                tile_row == s->tiling.tile_rows - 1) {
                tile_size = size;
            } else {
                tile_size = AV_RB32(data);
                data += 4;
                size -= 4;
            }
            if (tile_size < size)
                return AVERROR_INVALIDDATA;
            ff_vp56_init_range_decoder(&s->c, data, tile_size);
            if (vp56_rac_get_prob_branchy(&s->c, 128)) // marker bit
                return AVERROR_INVALIDDATA;
            data += tile_size;
            size -= tile_size;

            yoff2 = yoff + s->tiling.tile_col_start * 8;
            uvoff2 = uvoff + s->tiling.tile_col_start * 4;
            for (row = s->tiling.tile_row_start;
                 row < s->tiling.tile_row_end;
                 row += 8, yoff2 += s->f->linesize[0] * 64,
                 uvoff2 += s->f->linesize[1] * 32) {
                // FIXME remove VLA
                struct VP9Filter lflvl[s->sb_cols], *lflvl_ptr = lflvl;
                ptrdiff_t yoff3 = yoff2, uvoff3 = uvoff2;

                memset(s->left_partition_ctx, 0, 8);
                memset(s->left_skip_ctx, 0, 8);
                memset(s->left_mode_ctx, DC_PRED, 16);
                memset(s->left_y_nnz_ctx, 0, 16);
                memset(s->left_uv_nnz_ctx, 0, 16);
                // FIXME clear left context
                for (col = s->tiling.tile_col_start;
                     col  < s->tiling.tile_col_end;
                     col += 8, yoff3 += 64, uvoff3 += 32, lflvl_ptr++) {
                    // FIXME integrate with lf code (i.e. zero after each
                    // use, similar to invtxfm coefficients, or similar)
                    memset(lflvl_ptr->mask, 0, sizeof(lflvl_ptr->mask));

                    if ((res = decode_sb(ctx, row, col, lflvl_ptr,
                                         yoff3, uvoff3, BL_64X64)) < 0)
                        return res;
                }

                // backup pre-loopfilter reconstruction data for intra
                // prediction of next row of sb64s
                memcpy(s->intra_pred_data[0] + s->tiling.tile_col_start * 8,
                       s->f->data[0] + yoff2 + 63 * s->f->linesize[0],
                       64 * s->sb_cols);
                memcpy(s->intra_pred_data[1] + s->tiling.tile_col_start * 4,
                       s->f->data[1] + uvoff2 + 31 * s->f->linesize[1],
                       32 * s->sb_cols);
                memcpy(s->intra_pred_data[2] + s->tiling.tile_col_start * 4,
                       s->f->data[2] + uvoff2 + 31 * s->f->linesize[2],
                       32 * s->sb_cols);

                // loopfilter one row
                if (s->filter.level) {
                    yoff3 = yoff2;
                    uvoff3 = uvoff2;
                    lflvl_ptr = lflvl;
                    for (col = s->tiling.tile_col_start;
                         col  < s->tiling.tile_col_end;
                         col += 8, yoff3 += 64, uvoff3 += 32, lflvl_ptr++) {
                        loopfilter_sb(ctx, lflvl_ptr, row, col, yoff3, uvoff3);
                    }
                }
            }
        }
    }

    // bw adaptivity (or in case of parallel decoding mode, fw adaptivity
    // probability maintenance between frames)
    if (s->refreshctx) {
        if (s->parallelmode) {
            int i, j, k, l, m;

            for (i = 0; i < 4; i++)
                for (j = 0; j < 2; j++)
                    for (k = 0; k < 2; k++)
                        for (l = 0; l < 6; l++)
                            for (m = 0; m < 6; m++)
                                memcpy(s->prob_ctx[s->framectxid].coef[i][j][k][l][m],
                                       s->prob.coef[i][j][k][l][m], 3);
            s->prob_ctx[s->framectxid].p = s->prob.p;
        } else {
            adapt_probs(s);
        }
    }

    // FIXME ref frame setup

    if (!s->invisible) {
        if ((res = av_frame_ref(out_pic, s->f)) < 0)
            return res;
        *got_frame = 1;
    }

    return 0;
}

static int vp9_decode_packet(AVCodecContext *avctx, void *out_pic,
                             int *got_frame, AVPacket *avpkt)
{
    const uint8_t *data = avpkt->data;
    int size = avpkt->size, marker, res;

    // read superframe index - this is a collection of individual frames that
    // together lead to one visible frame
    if (size <= 0)
        return AVERROR_INVALIDDATA;
    marker = data[size - 1];
    if ((marker & 0xe0) == 0xc0) {
        int nbytes = 1 + ((marker >> 3) & 0x3);
        int n_frames = 1 + (marker & 0x7), idx_sz = 2 + n_frames * nbytes;
        if (size >= idx_sz && data[size - idx_sz] == marker) {
            const uint8_t *idx = data + size + 1 - idx_sz;
            switch (nbytes) {
#define case_n(a, rd) \
                case a: \
                    while (n_frames--) { \
                        int sz = rd; \
                        idx += a; \
                        res = vp9_decode_frame(avctx, out_pic, got_frame, \
                                               data, sz); \
                        if (res < 0) \
                            return res; \
                        data += sz; \
                    } \
                    break;
                case_n(1, *idx);
                case_n(2, AV_RB16(idx));
                case_n(3, AV_RB24(idx));
                case_n(4, AV_RB32(idx));
            }
            return size;
        }
    }
    // if we get here, there was no valid superframe index, i.e. this is just
    // one whole single frame - decode it as such from the complete input buf
    if ((res = vp9_decode_frame(avctx, out_pic, got_frame, data, size)) < 0)
        return res;
    return size;
}

static void vp9_decode_flush(AVCodecContext *ctx)
{
    printf("flush\n");
}

static int vp9_decode_init(AVCodecContext *ctx)
{
    VP9Context *s = ctx->priv_data;

    ctx->pix_fmt = AV_PIX_FMT_YUV420P;
    ff_vp9dsp_init(&s->dsp);
    ff_videodsp_init(&s->vdsp, 8);
    s->f = av_frame_alloc();
    if (!s->f)
        return AVERROR(ENOMEM);
    s->filter.sharpness = -1;

    return 0;
}

static int vp9_decode_free(AVCodecContext *ctx)
{
    VP9Context *s = ctx->priv_data;

    av_frame_free(&s->f);
    av_freep(&s->above_partition_ctx);
    s->above_skip_ctx = s->above_txfm_ctx = s->above_mode_ctx = NULL;
    s->above_y_nnz_ctx = s->above_uv_nnz_ctx[0] = s->above_uv_nnz_ctx[1] = NULL;
    s->intra_pred_data[0] = s->intra_pred_data[1] = s->intra_pred_data[2] = NULL;

    return 0;
}

AVCodec ff_vp9_decoder = {
  .name                  = "vp9",
  .type                  = AVMEDIA_TYPE_VIDEO,
  .id                    = AV_CODEC_ID_VP9,
  .priv_data_size        = sizeof(VP9Context),
  .init                  = vp9_decode_init,
  .close                 = vp9_decode_free,
  .decode                = vp9_decode_packet,
  .capabilities          = CODEC_CAP_DR1,
  .flush                 = vp9_decode_flush,
  .long_name             = NULL_IF_CONFIG_SMALL("Google VP9"),
};
