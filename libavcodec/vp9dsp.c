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

#include "libavutil/common.h"
#include "libavutil/intreadwrite.h"
#include "vp9dsp.h"

// FIXME see whether we can merge parts of this (perhaps at least 4x4 and 8x8)
// back with h264pred.[ch]

static void vert_4x4_c(uint8_t *dst, ptrdiff_t stride,
                       const uint8_t *left, const uint8_t *top)
{
    unsigned p4 = AV_RN32A(top);

    AV_WN32A(dst + stride * 0, p4);
    AV_WN32A(dst + stride * 1, p4);
    AV_WN32A(dst + stride * 2, p4);
    AV_WN32A(dst + stride * 3, p4);
}

static void vert_8x8_c(uint8_t *dst, ptrdiff_t stride,
                       const uint8_t *left, const uint8_t *top)
{
    uint64_t p8 = AV_RN64A(top);
    int y;

    for (y = 0; y < 8; y++) {
        AV_WN64A(dst, p8);
        dst += stride;
    }
}

static void vert_16x16_c(uint8_t *dst, ptrdiff_t stride,
                         const uint8_t *left, const uint8_t *top)
{
    uint64_t p8a = AV_RN64A(top + 0), p8b = AV_RN64A(top + 8);
    int y;

    for (y = 0; y < 16; y++) {
        AV_WN64A(dst + 0, p8a);
        AV_WN64A(dst + 8, p8b);
        dst += stride;
    }
}

static void hor_4x4_c(uint8_t *dst, ptrdiff_t stride,
                      const uint8_t *left, const uint8_t *top)
{
    AV_WN32A(dst + stride * 0, left[0] * 0x01010101U);
    AV_WN32A(dst + stride * 1, left[1] * 0x01010101U);
    AV_WN32A(dst + stride * 2, left[2] * 0x01010101U);
    AV_WN32A(dst + stride * 3, left[3] * 0x01010101U);
}

static void hor_8x8_c(uint8_t *dst, ptrdiff_t stride,
                      const uint8_t *left, const uint8_t *top)
{
    int y;

    for (y = 0; y < 8; y++) {
        AV_WN64A(dst, left[y] * 0x0101010101010101ULL);
        dst += stride;
    }
}

static void hor_16x16_c(uint8_t *dst, ptrdiff_t stride,
                        const uint8_t *left, const uint8_t *top)
{
    int y;

    for (y = 0; y < 16; y++) {
        uint64_t p8 = left[y] * 0x0101010101010101ULL;

        AV_WN64A(dst + 0, p8);
        AV_WN64A(dst + 8, p8);
        dst += stride;
    }
}

static void tm_4x4_c(uint8_t *dst, ptrdiff_t stride,
                     const uint8_t *left, const uint8_t *top)
{
    int y, tl = top[-1];
    
    for (y = 0; y < 4; y++) {
        int l_m_tl = left[y] - tl;
        
        dst[0] = av_clip_uint8(top[0] + l_m_tl);
        dst[1] = av_clip_uint8(top[1] + l_m_tl);
        dst[2] = av_clip_uint8(top[2] + l_m_tl);
        dst[3] = av_clip_uint8(top[3] + l_m_tl);
        dst += stride;
    }
}

static void tm_8x8_c(uint8_t *dst, ptrdiff_t stride,
                     const uint8_t *left, const uint8_t *top)
{
    int y, tl = top[-1];

    for (y = 0; y < 8; y++) {
        int l_m_tl = left[y] - tl;

        dst[0] = av_clip_uint8(top[0] + l_m_tl);
        dst[1] = av_clip_uint8(top[1] + l_m_tl);
        dst[2] = av_clip_uint8(top[2] + l_m_tl);
        dst[3] = av_clip_uint8(top[3] + l_m_tl);
        dst[4] = av_clip_uint8(top[4] + l_m_tl);
        dst[5] = av_clip_uint8(top[5] + l_m_tl);
        dst[6] = av_clip_uint8(top[6] + l_m_tl);
        dst[7] = av_clip_uint8(top[7] + l_m_tl);
        dst += stride;
    }
}

static void tm_16x16_c(uint8_t *dst, ptrdiff_t stride,
                       const uint8_t *left, const uint8_t *top)
{
    int y, tl = top[-1];

    for (y = 0; y < 16; y++) {
        int l_m_tl = left[y] - tl;

        dst[ 0] = av_clip_uint8(top[ 0] + l_m_tl);
        dst[ 1] = av_clip_uint8(top[ 1] + l_m_tl);
        dst[ 2] = av_clip_uint8(top[ 2] + l_m_tl);
        dst[ 3] = av_clip_uint8(top[ 3] + l_m_tl);
        dst[ 4] = av_clip_uint8(top[ 4] + l_m_tl);
        dst[ 5] = av_clip_uint8(top[ 5] + l_m_tl);
        dst[ 6] = av_clip_uint8(top[ 6] + l_m_tl);
        dst[ 7] = av_clip_uint8(top[ 7] + l_m_tl);
        dst[ 8] = av_clip_uint8(top[ 8] + l_m_tl);
        dst[ 9] = av_clip_uint8(top[ 9] + l_m_tl);
        dst[10] = av_clip_uint8(top[10] + l_m_tl);
        dst[11] = av_clip_uint8(top[11] + l_m_tl);
        dst[12] = av_clip_uint8(top[12] + l_m_tl);
        dst[13] = av_clip_uint8(top[13] + l_m_tl);
        dst[14] = av_clip_uint8(top[14] + l_m_tl);
        dst[15] = av_clip_uint8(top[15] + l_m_tl);
        dst += stride;
    }
}

static void dc_4x4_c(uint8_t *dst, ptrdiff_t stride,
                     const uint8_t *left, const uint8_t *top)
{
    unsigned dc = 0x01010101U * ((left[0] + left[1] + left[2] + left[3] +
                                  top[0] + top[1] + top[2] + top[3] + 4) >> 3);

    AV_WN32A(dst + stride * 0, dc);
    AV_WN32A(dst + stride * 1, dc);
    AV_WN32A(dst + stride * 2, dc);
    AV_WN32A(dst + stride * 3, dc);
}

static void dc_8x8_c(uint8_t *dst, ptrdiff_t stride,
                     const uint8_t *left, const uint8_t *top)
{
    uint64_t dc = 0x0101010101010101ULL *
        ((left[0] + left[1] + left[2] + left[3] + left[4] + left[5] +
          left[6] + left[7] + top[0] + top[1] + top[2] + top[3] +
          top[4] + top[5] + top[6] + top[7] + 8) >> 4);
    int y;

    for (y = 0; y < 8; y++) {
        AV_WN64A(dst, dc);
        dst += stride;
    }
}

static void dc_16x16_c(uint8_t *dst, ptrdiff_t stride,
                       const uint8_t *left, const uint8_t *top)
{
    uint64_t dc = 0x0101010101010101ULL *
        ((left[0] + left[1] + left[2] + left[3] + left[4] + left[5] + left[6] +
          left[7] + left[8] + left[9] + left[10] + left[11] + left[12] +
          left[13] + left[14] + left[15] + top[0] + top[1] + top[2] + top[3] +
          top[4] + top[5] + top[6] + top[7] + top[8] + top[9] + top[10] +
          top[11] + top[12] + top[13] + top[14] + top[15] + 16) >> 5);
    int y;

    for (y = 0; y < 16; y++) {
        AV_WN64A(dst + 0, dc);
        AV_WN64A(dst + 8, dc);
        dst += stride;
    }
}

static void dc_left_4x4_c(uint8_t *dst, ptrdiff_t stride,
                          const uint8_t *left, const uint8_t *top)
{
    unsigned dc = 0x01010101U * ((left[0] + left[1] + left[2] + left[3] + 2) >> 2);
    
    AV_WN32A(dst + stride * 0, dc);
    AV_WN32A(dst + stride * 1, dc);
    AV_WN32A(dst + stride * 2, dc);
    AV_WN32A(dst + stride * 3, dc);
}

static void dc_left_8x8_c(uint8_t *dst, ptrdiff_t stride,
                          const uint8_t *left, const uint8_t *top)
{
    uint64_t dc = 0x0101010101010101ULL *
        ((left[0] + left[1] + left[2] + left[3] +
          left[4] + left[5] + left[6] + left[7] + 4) >> 3);
    int y;

    for (y = 0; y < 8; y++) {
        AV_WN64A(dst, dc);
        dst += stride;
    }
}

static void dc_left_16x16_c(uint8_t *dst, ptrdiff_t stride,
                            const uint8_t *left, const uint8_t *top)
{
    uint64_t dc = 0x0101010101010101ULL *
        ((left[0] + left[1] + left[2] + left[3] + left[4] + left[5] +
          left[6] + left[7] + left[8] + left[9] + left[10] + left[11] +
          left[12] + left[13] + left[14] + left[15] + 4) >> 3);
    int y;

    for (y = 0; y < 16; y++) {
        AV_WN64A(dst + 0, dc);
        AV_WN64A(dst + 8, dc);
        dst += stride;
    }
}

static void dc_top_4x4_c(uint8_t *dst, ptrdiff_t stride,
                         const uint8_t *left, const uint8_t *top)
{
    unsigned dc = 0x01010101U * ((top[0] + top[1] + top[2] + top[3] + 2) >> 2);
    
    AV_WN32A(dst + stride * 0, dc);
    AV_WN32A(dst + stride * 1, dc);
    AV_WN32A(dst + stride * 2, dc);
    AV_WN32A(dst + stride * 3, dc);
}

static void dc_top_8x8_c(uint8_t *dst, ptrdiff_t stride,
                         const uint8_t *left, const uint8_t *top)
{
    uint64_t dc = 0x0101010101010101ULL *
        ((top[0] + top[1] + top[2] + top[3] +
          top[4] + top[5] + top[6] + top[7] + 2) >> 2);
    int y;

    for (y = 0; y < 8; y++) {
        AV_WN64A(dst, dc);
        dst += stride;
    }
}

static void dc_top_16x16_c(uint8_t *dst, ptrdiff_t stride,
                           const uint8_t *left, const uint8_t *top)
{
    uint64_t dc = 0x0101010101010101ULL *
        ((top[0] + top[1] + top[2] + top[3] + top[4] + top[5] +
          top[6] + top[7] + top[8] + top[9] + top[10] + top[11] +
          top[12] + top[13] + top[14] + top[15] + 2) >> 2);
    int y;

    for (y = 0; y < 16; y++) {
        AV_WN64A(dst + 0, dc);
        AV_WN64A(dst + 8, dc);
        dst += stride;
    }
}

static void dc_128_4x4_c(uint8_t *dst, ptrdiff_t stride,
                         const uint8_t *left, const uint8_t *top)
{
    AV_WN32A(dst + stride * 0, 0x80808080U);
    AV_WN32A(dst + stride * 1, 0x80808080U);
    AV_WN32A(dst + stride * 2, 0x80808080U);
    AV_WN32A(dst + stride * 3, 0x80808080U);
}

static void dc_128_8x8_c(uint8_t *dst, ptrdiff_t stride,
                         const uint8_t *left, const uint8_t *top)
{
    int y;

    for (y = 0; y < 8; y++) {
        AV_WN64A(dst, 0x8080808080808080ULL);
        dst += stride;
    }
}

static void dc_128_16x16_c(uint8_t *dst, ptrdiff_t stride,
                           const uint8_t *left, const uint8_t *top)
{
    int y;

    for (y = 0; y < 16; y++) {
        AV_WN64A(dst + 0, 0x8080808080808080ULL);
        AV_WN64A(dst + 8, 0x8080808080808080ULL);
        dst += stride;
    }
}

static void dc_127_4x4_c(uint8_t *dst, ptrdiff_t stride,
                         const uint8_t *left, const uint8_t *top)
{
    AV_WN32A(dst + stride * 0, 0x7F7F7F7FU);
    AV_WN32A(dst + stride * 1, 0x7F7F7F7FU);
    AV_WN32A(dst + stride * 2, 0x7F7F7F7FU);
    AV_WN32A(dst + stride * 3, 0x7F7F7F7FU);
}

static void dc_127_8x8_c(uint8_t *dst, ptrdiff_t stride,
                         const uint8_t *left, const uint8_t *top)
{
    int y;

    for (y = 0; y < 8; y++) {
        AV_WN64A(dst, 0x7F7F7F7F7F7F7F7FULL);
        dst += stride;
    }
}

static void dc_127_16x16_c(uint8_t *dst, ptrdiff_t stride,
                           const uint8_t *left, const uint8_t *top)
{
    int y;

    for (y = 0; y < 16; y++) {
        AV_WN64A(dst + 0, 0x7F7F7F7F7F7F7F7FULL);
        AV_WN64A(dst + 8, 0x7F7F7F7F7F7F7F7FULL);
        dst += stride;
    }
}

static void dc_129_4x4_c(uint8_t *dst, ptrdiff_t stride,
                         const uint8_t *left, const uint8_t *top)
{
    AV_WN32A(dst + stride * 0, 0x81818181U);
    AV_WN32A(dst + stride * 1, 0x81818181U);
    AV_WN32A(dst + stride * 2, 0x81818181U);
    AV_WN32A(dst + stride * 3, 0x81818181U);
}

static void dc_129_8x8_c(uint8_t *dst, ptrdiff_t stride,
                         const uint8_t *left, const uint8_t *top)
{
    int y;

    for (y = 0; y < 8; y++) {
        AV_WN64A(dst, 0x8181818181818181ULL);
        dst += stride;
    }
}

static void dc_129_16x16_c(uint8_t *dst, ptrdiff_t stride,
                           const uint8_t *left, const uint8_t *top)
{
    int y;

    for (y = 0; y < 16; y++) {
        AV_WN64A(dst + 0, 0x8181818181818181ULL);
        AV_WN64A(dst + 8, 0x8181818181818181ULL);
        dst += stride;
    }
}

#define DST(x, y) dst[(x) + (y) * stride]

static void diag_downleft_4x4_c(uint8_t *dst, ptrdiff_t stride,
                                const uint8_t *left, const uint8_t *top)
{
    int a0 = top[0], a1 = top[1], a2 = top[2], a3 = top[3],
        a4 = top[4], a5 = top[5], a6 = top[6], a7 = top[7];

    DST(0,0) = (a0 + a1 * 2 + a2 + 2) >> 2;
    DST(1,0) = DST(0,1) = (a1 + a2 * 2 + a3 + 2) >> 2;
    DST(2,0) = DST(1,1) = DST(0,2) = (a2 + a3 * 2 + a4 + 2) >> 2;
    DST(3,0) = DST(2,1) = DST(1,2) = DST(0,3) = (a3 + a4 * 2 + a5 + 2) >> 2;
    DST(3,1) = DST(2,2) = DST(1,3) = (a4 + a5 * 2 + a6 + 2) >> 2;
    DST(3,2) = DST(2,3) = (a5 + a6 * 2 + a7 + 2) >> 2;
    DST(3,3) = a7;  // note: this is different from vp8 and such
}

static void diag_downleft_8x8_c(uint8_t *dst, ptrdiff_t stride,
                                const uint8_t *left, const uint8_t *top)
{
    int a0 = top[0], a1 = top[1], a2 = top[2], a3 = top[3],
        a4 = top[4], a5 = top[5], a6 = top[6], a7 = top[7];

    DST(0,0) = (a0 + a1 * 2 + a2 + 2) >> 2;
    DST(1,0) = DST(0,1) = (a1 + a2 * 2 + a3 + 2) >> 2;
    DST(2,0) = DST(1,1) = DST(0,2) = (a2 + a3 * 2 + a4 + 2) >> 2;
    DST(3,0) = DST(2,1) = DST(1,2) = DST(0,3) = (a3 + a4 * 2 + a5 + 2) >> 2;
    DST(4,0) = DST(3,1) = DST(2,2) = DST(1,3) =
               DST(0,4) = (a4 + a5 * 2 + a6 + 2) >> 2;
    DST(5,0) = DST(4,1) = DST(3,2) = DST(2,3) = DST(1,4) =
               DST(0,5) = (a5 + a6 * 2 + a7 + 2) >> 2;
    DST(6,0) = DST(5,1) = DST(4,2) = DST(3,3) = DST(2,4) = DST(1,5) =
               DST(0,6) = (a6 + a7 * 3 + 2) >> 2;
    DST(7,0) = DST(6,1) = DST(7,1) = DST(5,2) = DST(6,2) = DST(7,2) =
               DST(4,3) = DST(5,3) = DST(6,3) = DST(7,3) = DST(3,4) =
               DST(4,4) = DST(5,4) = DST(6,4) = DST(7,4) = DST(2,5) =
               DST(3,5) = DST(4,5) = DST(5,5) = DST(6,5) = DST(7,5) =
               DST(1,6) = DST(2,6) = DST(3,6) = DST(4,6) = DST(5,6) =
               DST(6,6) = DST(7,6) = DST(0,7) = DST(1,7) = DST(2,7) =
               DST(3,7) = DST(4,7) = DST(5,7) = DST(6,7) = DST(7,7) = a7;
}

static void diag_downleft_16x16_c(uint8_t *dst, ptrdiff_t stride,
                                  const uint8_t *left, const uint8_t *top)
{
    int a0 = top[0], a1 = top[1], a2 = top[2], a3 = top[3],
        a4 = top[4], a5 = top[5], a6 = top[6], a7 = top[7],
        a8 = top[8], a9 = top[9], a10 = top[10], a11 = top[11],
        a12 = top[12], a13 = top[13], a14 = top[14], a15 = top[15];

    DST(0,0)  = (a0 + a1 * 2 + a2 + 2) >> 2;
    DST(1,0)  = DST(0,1)  = (a1 + a2 * 2 + a3 + 2) >> 2;
    DST(2,0)  = DST(1,1)  = DST(0,2)  = (a2 + a3 * 2 + a4 + 2) >> 2;
    DST(3,0)  = DST(2,1)  = DST(1,2)  = DST(0,3)  = (a3 + a4 * 2 + a5 + 2) >> 2;
    DST(4,0)  = DST(3,1)  = DST(2,2)  = DST(1,3)  =
                DST(0,4)  = (a4 + a5 * 2 + a6 + 2) >> 2;
    DST(5,0)  = DST(4,1)  = DST(3,2)  = DST(2,3)  = DST(1,4)  =
                DST(0,5)  = (a5 + a6 * 2 + a7 + 2) >> 2;
    DST(6,0)  = DST(5,1)  = DST(4,2)  = DST(3,3)  = DST(2,4)  = DST(1,5)  =
                DST(0,6)  = (a6 + a7 * 2 + a8 + 2) >> 2;
    DST(7,0)  = DST(6,1)  = DST(5,2)  = DST(4,3)  = DST(3,4)  = DST(2,5)  =
                DST(1,6)  = DST(0,7)  = (a7 + a8 * 2 + a9 + 2) >> 2;
    DST(8,0)  = DST(7,1)  = DST(6,2)  = DST(5,3)  = DST(4,4)  = DST(3,5)  =
                DST(2,6)  = DST(1,7)  = DST(0,8)  = (a8 + a9 * 2 + a10 + 2) >> 2;
    DST(9,0)  = DST(8,1)  = DST(7,2)  = DST(6,3)  = DST(5,4)  = DST(4,5)  =
                DST(3,6)  = DST(2,7)  = DST(1,8)  =
                DST(0,9)  = (a9 + a10 * 2 + a11 + 2) >> 2;
    DST(10,0) = DST(9,1)  = DST(8,2)  = DST(7,3)  = DST(6,4)  = DST(5,5)  =
                DST(4,6)  = DST(3,7)  = DST(2,8)  = DST(1,9)  =
                DST(0,10) = (a10 + a11 * 2 + a12 + 2) >> 2;
    DST(11,0) = DST(10,1) = DST(9,2)  = DST(8,3)  = DST(7,4)  = DST(6,5)  =
                DST(5,6)  = DST(4,7)  = DST(3,8)  = DST(2,9)  = DST(1,10) =
                DST(0,11) = (a11 + a12 * 2 + a13 + 2) >> 2;
    DST(12,0) = DST(11,1) = DST(10,2) = DST(9,3)  = DST(8,4)  = DST(7,5)  =
                DST(6,6)  = DST(5,7)  = DST(4,8)  = DST(3,9)  = DST(2,10) =
                DST(1,11) = DST(0,12) = (a12 + a13 * 2 + a14 + 2) >> 2;
    DST(13,0) = DST(12,1) = DST(11,2) = DST(10,3) = DST(9,4)  = DST(8,5)  =
                DST(7,6)  = DST(6,7)  = DST(5,8)  = DST(4,9)  = DST(3,10) =
                DST(2,11) = DST(1,12) = DST(0,13) = (a13 + a14 * 2 + a15 + 2) >> 2;
    DST(14,0) = DST(13,1) = DST(12,2) = DST(11,3) = DST(10,4) = DST(9,5)  =
                DST(8,6)  = DST(7,7)  = DST(8,6)  = DST(9,5)  = DST(10,4) =
                DST(11,3) = DST(12,2) = DST(13,1) =
                DST(14,0) = (a14 + a15 * 3 + 2) >> 2;
    DST(15,0) = DST(14,1) = DST(15,1) = DST(13,2) = DST(14,2) = DST(15,2) =
                DST(12,3) = DST(13,3) = DST(14,3) = DST(15,3) = DST(11,4) =
                DST(12,4) = DST(13,4) = DST(14,4) = DST(15,4) = DST(10,5) =
                DST(11,5) = DST(12,5) = DST(13,5) = DST(14,5) = DST(15,5) =
                DST(9,6)  = DST(10,6) = DST(11,6) = DST(12,6) = DST(13,6) =
                DST(14,6) = DST(15,6) = DST(8,7)  = DST(9,7)  = DST(10,7) =
                DST(11,7) = DST(12,7) = DST(13,7) = DST(14,7) = DST(15,7) =
                DST(7,8)  = DST(8,8)  = DST(9,8)  = DST(10,8) = DST(11,8) =
                DST(12,8) = DST(13,8) = DST(14,8) = DST(15,8) = DST(6,9)  =
                DST(7,9)  = DST(8,9)  = DST(9,9)  = DST(10,9) = DST(11,9) =
                DST(12,9) = DST(13,9) = DST(14,9) = DST(15,9) = DST(5,10) =
                DST(6,10) = DST(7,10) = DST(8,10) = DST(9,10) = DST(10,10) =
                DST(11,10) = DST(12,10) = DST(13,10) = DST(14,10) = DST(15,10) =
                DST(4,11) = DST(5,11) = DST(6,11) = DST(7,11) = DST(8,11) =
                DST(9,11) = DST(10,11) = DST(11,11) = DST(12,11) = DST(13,11) =
                DST(14,11) = DST(15,11) = DST(3,12) = DST(4,12) = DST(5,12) =
                DST(6,12) = DST(7,12) = DST(8,12) = DST(9,12) = DST(10,12) =
                DST(11,12) = DST(12,12) = DST(13,12) = DST(14,12) = DST(15,12) =
                DST(2,13) = DST(3,13) = DST(4,13) = DST(5,13) = DST(6,13) =
                DST(7,13) = DST(8,13) = DST(9,13) = DST(10,13) = DST(11,13) =
                DST(12,13) = DST(13,13) = DST(14,13) = DST(15,13) = DST(1,14) =
                DST(2,14) = DST(3,14) = DST(4,14) = DST(5,14) = DST(6,14) =
                DST(7,14) = DST(8,14) = DST(9,14) = DST(10,14) = DST(11,14) =
                DST(12,14) = DST(13,14) = DST(14,14) = DST(15,14) = DST(0,15) =
                DST(1,15) = DST(2,15) = DST(3,15) = DST(4,15) = DST(5,15) =
                DST(6,15) = DST(7,15) = DST(8,15) = DST(9,15) = DST(10,15) =
                DST(11,15) = DST(12,15) = DST(13,15) = DST(14,15) =
                DST(15,15) = a15;
}

static void diag_downright_4x4_c(uint8_t *dst, ptrdiff_t stride,
                                 const uint8_t *left, const uint8_t *top)
{
    int tl = top[-1], a0 = top[0], a1 = top[1], a2 = top[2], a3 = top[3],
        l0 = left[0], l1 = left[1], l2 = left[2], l3 = left[3];

    DST(0,3) = (l1 + l2 * 2 + l3 + 2) >> 2;
    DST(0,2) = DST(1,3) = (l0 + l1 * 2 + l2 + 2) >> 2;
    DST(0,1) = DST(1,2) = DST(2,3) = (tl + l0 * 2 + l1 + 2) >> 2;
    DST(0,0) = DST(1,1) = DST(2,2) = DST(3,3) = (l0 + tl * 2 + a0 + 2) >> 2;
    DST(1,0) = DST(2,1) = DST(3,2) = (tl + a0 * 2 + a1 + 2) >> 2;
    DST(2,0) = DST(3,1) = (a0 + a1 * 2 + a2 + 2) >> 2;
    DST(3,0) = (a1 + a2 * 2 + a3 + 2) >> 2;
}

static void diag_downright_8x8_c(uint8_t *dst, ptrdiff_t stride,
                                 const uint8_t *left, const uint8_t *top)
{
    int tl = top[-1], a0 = top[0], a1 = top[1], a2 = top[2], a3 = top[3],
        a4 = top[4], a5 = top[5], a6 = top[6], a7 = top[7],
        l0 = left[0], l1 = left[1], l2 = left[2], l3 = left[3],
        l4 = left[4], l5 = left[5], l6 = left[6], l7 = left[7];

    DST(0,7) = (l5 + l6 * 2 + l7 + 2) >> 2;
    DST(0,6) = DST(1,7) = (l4 + l5 * 2 + l6 + 2) >> 2;
    DST(0,5) = DST(1,6) = DST(2,7) = (l3 + l4 * 2 + l5 + 2) >> 2;
    DST(0,4) = DST(1,5) = DST(2,6) = DST(3,7) = (l2 + l3 * 2 + l4 + 2) >> 2;
    DST(0,3) = DST(1,4) = DST(2,5) = DST(3,6) =
               DST(4,7) = (l1 + l2 * 2 + l3 + 2) >> 2;
    DST(0,2) = DST(1,3) = DST(2,4) = DST(3,5) = DST(4,6) =
               DST(5,7) = (l0 + l1 * 2 + l2 + 2) >> 2;
    DST(0,1) = DST(1,2) = DST(2,3) = DST(3,4) = DST(4,5) = DST(5,6) =
               DST(6,7) = (tl + l0 * 2 + l1 + 2) >> 2;
    DST(0,0) = DST(1,1) = DST(2,2) = DST(3,3) = DST(4,4) = DST(5,5) =
               DST(6,6) = DST(7,7) = (l0 + tl * 2 + a0 + 2) >> 2;
    DST(1,0) = DST(2,1) = DST(3,2) = DST(4,3) = DST(5,4) = DST(6,5) =
               DST(7,6) = (tl + a0 * 2 + a1 + 2) >> 2;
    DST(2,0) = DST(3,1) = DST(4,2) = DST(5,3) = DST(6,4) =
               DST(7,5) = (a0 + a1 * 2 + a2 + 2) >> 2;
    DST(3,0) = DST(4,1) = DST(5,2) = DST(6,3) =
               DST(7,4) = (a1 + a2 * 2 + a3 + 2) >> 2;
    DST(4,0) = DST(5,1) = DST(6,2) = DST(7,3) = (a2 + a3 * 2 + a4 + 2) >> 2;
    DST(5,0) = DST(6,1) = DST(7,2) = (a3 + a4 * 2 + a5 + 2) >> 2;
    DST(6,0) = DST(7,1) = (a4 + a5 * 2 + a6 + 2) >> 2;
    DST(7,0) = (a5 + a6 * 2 + a7 + 2) >> 2;
}

static void diag_downright_16x16_c(uint8_t *dst, ptrdiff_t stride,
                                   const uint8_t *left, const uint8_t *top)
{
    int tl = top[-1], a0 = top[0], a1 = top[1], a2 = top[2], a3 = top[3],
        a4 = top[4], a5 = top[5], a6 = top[6], a7 = top[7],
        a8 = top[8], a9 = top[9], a10 = top[10], a11 = top[11],
        a12 = top[12], a13 = top[13], a14 = top[14], a15 = top[15],
        l0 = left[0], l1 = left[1], l2 = left[2], l3 = left[3],
        l4 = left[4], l5 = left[5], l6 = left[6], l7 = left[7],
        l8 = left[8], l9 = left[9], l10 = left[10], l11 = left[11],
        l12 = left[12], l13 = left[13], l14 = left[14], l15 = left[15];

    DST(0,15) = (l13 + l14 * 2 + l15 + 2) >> 2;
    DST(0,14) = DST(1,15) = (l12 + l13 * 2 + l14 + 2) >> 2;
    DST(0,13) = DST(1,14) = DST(2,15) = (l11 + l12 * 2 + l13 + 2) >> 2;
    DST(0,12) = DST(1,13) = DST(2,14) = DST(3,15) = (l10 + l11 * 2 + l12 + 2) >> 2;
    DST(0,11) = DST(1,12) = DST(2,13) = DST(3,14) =
                DST(4,15) = (l9 + l10 * 2 + l11 + 2) >> 2;
    DST(0,10) = DST(1,11) = DST(2,12) = DST(3,13) = DST(4,14) =
                DST(5,15) = (l8 + l9 * 2 + l10 + 2) >> 2;
    DST(0,9)  = DST(1,10) = DST(2,11) = DST(3,12) = DST(4,13) = DST(5,14) =
                DST(6,15) = (l7 + l8 * 2 + l9 + 2) >> 2;
    DST(0,8)  = DST(1,9)  = DST(2,10) = DST(3,11) = DST(4,12) = DST(5,13) =
                DST(6,14) = DST(7,15) = (l6 + l7 * 2 + l8 + 2) >> 2;
    DST(0,7)  = DST(1,8)  = DST(2,9)  = DST(3,10) = DST(4,11) = DST(5,12) =
                DST(6,13) = DST(7,14) = DST(8,15) = (l5 + l6 * 2 + l7 + 2) >> 2;
    DST(0,6)  = DST(1,7)  = DST(2,8)  = DST(3,9)  = DST(4,10) = DST(5,11) =
                DST(6,12) = DST(7,13) = DST(8,14) =
                DST(9,15) = (l4 + l5 * 2 + l6 + 2) >> 2;
    DST(0,5)  = DST(1,6)  = DST(2,7)  = DST(3,8)  = DST(4,9)  = DST(5,10) =
                DST(6,11) = DST(7,12) = DST(8,13) = DST(9,14) =
                DST(10,15) = (l3 + l4 * 2 + l5 + 2) >> 2;
    DST(0,4)  = DST(1,5)  = DST(2,6)  = DST(3,7)  = DST(4,8)  = DST(5,9)  =
                DST(6,10) = DST(7,11) = DST(8,12) = DST(9,13) = DST(10,14) =
                DST(11,15) = (l2 + l3 * 2 + l4 + 2) >> 2;
    DST(0,3)  = DST(1,4)  = DST(2,5)  = DST(3,6)  = DST(4,7)  = DST(5,8)  =
                DST(6,9)  = DST(7,10) = DST(8,11) = DST(9,12) = DST(10,13) =
                DST(11,14) = DST(12,15) = (l1 + l2 * 2 + l3 + 2) >> 2;
    DST(0,2)  = DST(1,3)  = DST(2,4)  = DST(3,5)  = DST(4,6)  = DST(5,7)  =
                DST(6,8)  = DST(7,9)  = DST(8,10) = DST(9,11) = DST(10,12) =
                DST(11,13) = DST(12,14) = DST(13,15) = (l0 + l1 * 2 + l2 + 2) >> 2;
    DST(0,1)  = DST(1,2)  = DST(2,3)  = DST(3,4)  = DST(4,5)  = DST(5,6)  =
                DST(6,7)  = DST(7,8)  = DST(8,9)  = DST(9,10) = DST(10,11) =
                DST(11,12) = DST(12,13) = DST(13,14) =
                DST(14,15) = (tl + l0 * 2 + l1 + 2) >> 2;
    DST(0,0)  = DST(1,1)  = DST(2,2)  = DST(3,3)  = DST(4,4)  = DST(5,5)  =
                DST(6,6)  = DST(7,7)  = DST(8,8)  = DST(9,9)  = DST(10,10) =
                DST(11,11) = DST(12,12) = DST(13,13) = DST(14,14) =
                DST(15,15) = (l0 + tl * 2 + a0 + 2) >> 2;
    DST(1,0)  = DST(2,1)  = DST(3,2)  = DST(4,3)  = DST(5,4)  = DST(6,5)  =
                DST(7,6)  = DST(8,7)  = DST(9,8)  = DST(10,9) = DST(11,10) =
                DST(12,11) = DST(13,12) = DST(14,13) =
                DST(15,14) = (tl + a0 * 2 + a1 + 2) >> 2;
    DST(2,0)  = DST(3,1)  = DST(4,2)  = DST(5,3)  = DST(6,4)  = DST(7,5)  =
                DST(8,6)  = DST(9,7)  = DST(10,8) = DST(11,9) = DST(12,10) =
                DST(13,11) = DST(14,12) = DST(15,13) = (a0 + a1 * 2 + a2 + 2) >> 2;
    DST(3,0)  = DST(4,1)  = DST(5,2)  = DST(6,3)  = DST(7,4)  = DST(8,5)  =
                DST(9,6)  = DST(10,7) = DST(11,8) = DST(12,9) = DST(13,10) =
                DST(14,11) = DST(15,12) = (a1 + a2 * 2 + a3 + 2) >> 2;
    DST(4,0)  = DST(5,1)  = DST(6,2)  = DST(7,3)  = DST(8,4)  = DST(9,5)  =
                DST(10,6) = DST(11,7) = DST(12,8) = DST(13,9) = DST(14,10) =
                DST(15,11) = (a2 + a3 * 2 + a4 + 2) >> 2;
    DST(5,0)  = DST(6,1)  = DST(7,2)  = DST(8,3)  = DST(9,4)  = DST(10,5) =
                DST(11,6) = DST(12,7) = DST(13,8) = DST(14,9) =
                DST(15,10) = (a3 + a4 * 2 + a5 + 2) >> 2;
    DST(6,0)  = DST(7,1)  = DST(8,2)  = DST(9,3)  = DST(10,4) = DST(11,5) =
                DST(12,6) = DST(13,7) = DST(14,8) =
                DST(15,9) = (a4 + a5 * 2 + a6 + 2) >> 2;
    DST(7,0)  = DST(8,1)  = DST(9,2)  = DST(10,3) = DST(11,4) = DST(12,5) =
                DST(13,6) = DST(14,7) = DST(15,8) = (a5 + a6 * 2 + a7 + 2) >> 2;
    DST(8,0)  = DST(9,1)  = DST(10,2) = DST(11,3) = DST(12,4) = DST(13,5) =
                DST(14,6) = DST(15,7) = (a6 + a7 * 2 + a8 + 2) >> 2;
    DST(9,0)  = DST(10,1) = DST(11,2) = DST(12,3) = DST(13,4) = DST(14,5) =
                DST(15,6) = (a7 + a8 * 2 + a9 + 2) >> 2;
    DST(10,0) = DST(11,1) = DST(12,2) = DST(13,3) = DST(14,4) =
                DST(15,5) = (a8 + a9 * 2 + a10 + 2) >> 2;
    DST(11,0) = DST(12,1) = DST(13,2) = DST(14,3) =
                DST(15,4) = (a9 + a10 * 2 + a11 + 2) >> 2;
    DST(12,0) = DST(13,1) = DST(14,2) = DST(15,3) = (a10 + a11 * 2 + a12 + 2) >> 2;
    DST(13,0) = DST(14,1) = DST(15,2) = (a11 + a12 * 2 + a13 + 2) >> 2;
    DST(14,0) = DST(15,1) = (a12 + a13 * 2 + a14 + 2) >> 2;
    DST(15,0) = (a13 + a14 * 2 + a15 + 2) >> 2;
}

static void vert_right_4x4_c(uint8_t *dst, ptrdiff_t stride,
                             const uint8_t *left, const uint8_t *top)
{
    int tl = top[-1], a0 = top[0], a1 = top[1], a2 = top[2], a3 = top[3],
        l0 = left[0], l1 = left[1], l2 = left[2];

    DST(0,3) = (l0 + l1 * 2 + l2 + 2) >> 2;
    DST(0,2) = (tl + l0 * 2 + l1 + 2) >> 2;
    DST(0,0) = DST(1,2) = (tl + a0 + 1) >> 1;
    DST(0,1) = DST(1,3) = (l0 + tl * 2 + a0 + 2) >> 2;
    DST(1,0) = DST(2,2) = (a0 + a1 + 1) >> 1;
    DST(1,1) = DST(2,3) = (tl + a0 * 2 + a1 + 2) >> 2;
    DST(2,0) = DST(3,2) = (a1 + a2 + 1) >> 1;
    DST(2,1) = DST(3,3) = (a0 + a1 * 2 + a2 + 2) >> 2;
    DST(3,0) = (a2 + a3 + 1) >> 1;
    DST(3,1) = (a1 + a2 * 2 + a3 + 2) >> 2;
}

static void vert_right_8x8_c(uint8_t *dst, ptrdiff_t stride,
                             const uint8_t *left, const uint8_t *top)
{
    int tl = top[-1], a0 = top[0], a1 = top[1], a2 = top[2], a3 = top[3],
        a4 = top[4], a5 = top[5], a6 = top[6], a7 = top[7],
        l0 = left[0], l1 = left[1], l2 = left[2], l3 = left[3],
        l4 = left[4], l5 = left[5], l6 = left[6];

    DST(0,7) = (l4 + l5 * 2 + l6 + 2) >> 2;
    DST(0,6) = (l3 + l4 * 2 + l5 + 2) >> 2;
    DST(0,5) = DST(1,7) = (l2 + l3 * 2 + l4 + 2) >> 2;
    DST(0,4) = DST(1,6) = (l1 + l2 * 2 + l3 + 2) >> 2;
    DST(0,3) = DST(1,5) = DST(2,7) = (l0 + l1 * 2 + l2 + 2) >> 2;
    DST(0,2) = DST(1,4) = DST(2,6) = (tl + l0 * 2 + l1 + 2) >> 2;
    DST(0,0) = DST(1,2) = DST(2,4) = DST(3,6) = (tl + a0 + 1) >> 1;
    DST(0,1) = DST(1,3) = DST(2,5) = DST(3,7) = (l0 + tl * 2 + a0 + 2) >> 2;
    DST(1,0) = DST(2,2) = DST(3,4) = DST(4,6) = (a0 + a1 + 1) >> 1;
    DST(1,1) = DST(2,3) = DST(3,5) = DST(4,7) = (tl + a0 * 2 + a1 + 2) >> 2;
    DST(2,0) = DST(3,2) = DST(4,4) = DST(5,6) = (a1 + a2 + 1) >> 1;
    DST(2,1) = DST(3,3) = DST(4,5) = DST(5,7) = (a0 + a1 * 2 + a2 + 2) >> 2;
    DST(3,0) = DST(4,2) = DST(5,4) = DST(6,6) = (a2 + a3 + 1) >> 1;
    DST(3,1) = DST(4,3) = DST(5,5) = DST(6,7) = (a1 + a2 * 2 + a3 + 2) >> 2;
    DST(4,0) = DST(5,2) = DST(6,4) = DST(7,6) = (a3 + a4 + 1) >> 1;
    DST(4,1) = DST(5,3) = DST(6,5) = DST(7,7) = (a2 + a3 * 2 + a4 + 2) >> 2;
    DST(5,0) = DST(6,2) = DST(7,4) = (a4 + a5 + 1) >> 1;
    DST(5,1) = DST(6,3) = DST(7,5) = (a3 + a4 * 2 + a5 + 2) >> 2;
    DST(6,0) = DST(7,2) = (a5 + a6 + 1) >> 1;
    DST(6,1) = DST(7,3) = (a4 + a5 * 2 + a6 + 2) >> 2;
    DST(7,0) = (a6 + a7 + 1) >> 1;
    DST(7,1) = (a5 + a6 * 2 + a7 + 2) >> 2;
}

static void vert_right_16x16_c(uint8_t *dst, ptrdiff_t stride,
                               const uint8_t *left, const uint8_t *top)
{
    int tl = top[-1], a0 = top[0], a1 = top[1], a2 = top[2], a3 = top[3],
        a4 = top[4], a5 = top[5], a6 = top[6], a7 = top[7],
        a8 = top[8], a9 = top[9], a10 = top[10], a11 = top[11],
        a12 = top[12], a13 = top[13], a14 = top[14], a15 = top[15],
        l0 = left[0], l1 = left[1], l2 = left[2], l3 = left[3],
        l4 = left[4], l5 = left[5], l6 = left[6], l7 = left[7],
        l8 = left[8], l9 = left[9], l10 = left[10], l11 = left[11],
        l12 = left[12], l13 = left[13], l14 = left[14];

    DST(0,15) = (l12 + l13 * 2 + l14 + 2) >> 2;
    DST(0,14) = (l11 + l12 * 2 + l13 + 2) >> 2;
    DST(0,13) = DST(1,15) = (l10 + l11 * 2 + l12 + 2) >> 2;
    DST(0,12) = DST(1,14) = (l9 + l10 * 2 + l11 + 2) >> 2;
    DST(0,11) = DST(1,13) = DST(2,15) = (l8 + l9 * 2 + l10 + 2) >> 2;
    DST(0,10) = DST(1,12) = DST(2,14) = (l7 + l8 * 2 + l9 + 2) >> 2;
    DST(0,9)  = DST(1,11) = DST(2,13) = DST(3,15) = (l6 + l7 * 2 + l8 + 2) >> 2;
    DST(0,8)  = DST(1,10) = DST(2,12) = DST(3,14) = (l5 + l6 * 2 + l7 + 2) >> 2;
    DST(0,7)  = DST(1,9)  = DST(2,11) = DST(3,13)  =
                DST(4,15) = (l4 + l5 * 2 + l6 + 2) >> 2;
    DST(0,6)  = DST(1,8)  = DST(2,10) = DST(3,12) =
                DST(4,14) = (l3 + l4 * 2 + l5 + 2) >> 2;
    DST(0,5)  = DST(1,7)  = DST(2,9)  = DST(3,11) = DST(4,13) =
                DST(5,15) = (l2 + l3 * 2 + l4 + 2) >> 2;
    DST(0,4)  = DST(1,6)  = DST(2,8)  = DST(3,10) = DST(4,12) =
                DST(5,14) = (l1 + l2 * 2 + l3 + 2) >> 2;
    DST(0,3)  = DST(1,5)  = DST(2,7)  = DST(3,9)  = DST(4,11) = DST(5,13) =
                DST(6,15) = (l0 + l1 * 2 + l2 + 2) >> 2;
    DST(0,2)  = DST(1,4)  = DST(2,6)  = DST(3,8)  = DST(4,10) = DST(5,12) =
                DST(6,14) = (tl + l0 * 2 + l1 + 2) >> 2;
    DST(0,0)  = DST(1,2)  = DST(2,4)  = DST(3,6)  = DST(4,8)  = DST(5,10) =
                DST(6,12) = DST(7,14) = (tl + a0 + 1) >> 1;
    DST(0,1)  = DST(1,3)  = DST(2,5)  = DST(3,7)  = DST(4,9)  = DST(5,11) =
                DST(6,13) = DST(7,15) = (l0 + tl * 2 + a0 + 2) >> 2;
    DST(1,0)  = DST(2,2)  = DST(3,4)  = DST(4,6)  = DST(5,8)  = DST(6,10) =
                DST(7,12) = DST(8,14) = (a0 + a1 + 1) >> 1;
    DST(1,1)  = DST(2,3)  = DST(3,5)  = DST(4,7)  = DST(5,9)  = DST(6,11) =
                DST(7,13) = DST(8,15) = (tl + a0 * 2 + a1 + 2) >> 2;
    DST(2,0)  = DST(3,2)  = DST(4,4)  = DST(5,6)  = DST(6,8)  = DST(7,10) =
                DST(8,12) = DST(9,14) = (a1 + a2 + 1) >> 1;
    DST(2,1)  = DST(3,3)  = DST(4,5)  = DST(5,7)  = DST(6,9)  = DST(7,11) =
                DST(8,13) = DST(9,15) = (a0 + a1 * 2 + a2 + 2) >> 2;
    DST(3,0)  = DST(4,2)  = DST(5,4)  = DST(6,6)  = DST(7,8)  = DST(8,10) =
                DST(9,12) = DST(10,14) = (a2 + a3 + 1) >> 1;
    DST(3,1)  = DST(4,3)  = DST(5,5)  = DST(6,7)  = DST(7,9)  = DST(8,11) =
                DST(9,13) = DST(10,15) = (a1 + a2 * 2 + a3 + 2) >> 2;
    DST(4,0)  = DST(5,2)  = DST(6,4)  = DST(7,6)  = DST(8,8)  = DST(9,10) =
                DST(10,12) = DST(11,14) = (a3 + a4 + 1) >> 1;
    DST(4,1)  = DST(5,3)  = DST(6,5)  = DST(7,7)  = DST(8,9)  = DST(9,11) =
                DST(10,13) = DST(11,15) = (a2 + a3 * 2 + a4 + 2) >> 2;
    DST(5,0)  = DST(6,2)  = DST(7,4)  = DST(8,6)  = DST(9,8)  = DST(10,10) =
                DST(11,12) = DST(12,14) = (a4 + a5 + 1) >> 1;
    DST(5,1)  = DST(6,3)  = DST(7,5)  = DST(8,7)  = DST(9,9)  = DST(10,11) =
                DST(11,13) = DST(12,15) = (a3 + a4 * 2 + a5 + 2) >> 2;
    DST(6,0)  = DST(7,2)  = DST(8,4)  = DST(9,6)  = DST(10,8) = DST(11,10) =
                DST(12,12) = DST(13,14) = (a5 + a6 + 1) >> 1;
    DST(6,1)  = DST(7,3)  = DST(8,5)  = DST(9,7)  = DST(10,9) = DST(11,11) =
                DST(12,13) = DST(13,15) = (a4 + a5 * 2 + a6 + 2) >> 2;
    DST(7,0)  = DST(8,2)  = DST(9,4)  = DST(10,6) = DST(11,8) = DST(12,10) =
                DST(13,12) = DST(14,14) = (a6 + a7 + 1) >> 1;
    DST(7,1)  = DST(8,3)  = DST(9,5)  = DST(10,7) = DST(11,9) = DST(12,11) =
                DST(13,13) = DST(14,15) = (a5 + a6 * 2 + a7 + 2) >> 2;
    DST(8,0)  = DST(9,2)  = DST(10,4) = DST(11,6) = DST(12,8) = DST(13,10) =
                DST(14,12) = DST(15,14) = (a7 + a8 + 1) >> 1;
    DST(8,1)  = DST(9,3)  = DST(10,5) = DST(11,7) = DST(12,9) = DST(13,11) =
                DST(14,13) = DST(15,15) = (a6 + a7 * 2 + a8 + 2) >> 2;
    DST(9,0)  = DST(10,2) = DST(11,4) = DST(12,6) = DST(13,8) = DST(14,10) =
                DST(15,12) = (a8 + a9 + 1) >> 1;
    DST(9,1)  = DST(10,3) = DST(11,5) = DST(12,7) = DST(13,9) = DST(14,11) =
                DST(15,13) = (a7 + a8 * 2 + a9 + 2) >> 2;
    DST(10,0) = DST(11,2) = DST(12,4) = DST(13,6) = DST(14,8) =
                DST(15,10) = (a9 + a10 + 1) >> 1;
    DST(10,1) = DST(11,3) = DST(12,5) = DST(13,7) = DST(14,9) =
                DST(15,11) = (a8 + a9 * 2 + a10 + 2) >> 2;
    DST(11,0) = DST(12,2) = DST(13,4) = DST(14,6) =
                DST(15,8) = (a10 + a11 + 1) >> 1;
    DST(11,1) = DST(12,3) = DST(13,5) = DST(14,7) =
                DST(15,9) = (a9 + a10 * 2 + a11 + 2) >> 2;
    DST(12,0) = DST(13,2) = DST(14,4) = DST(15,6) = (a11 + a12 + 1) >> 1;
    DST(12,1) = DST(13,3) = DST(14,5) = DST(15,7) = (a10 + a11 * 2 + a12 + 2) >> 2;
    DST(13,0) = DST(14,2) = DST(15,4) = (a12 + a13 + 1) >> 1;
    DST(13,1) = DST(14,3) = DST(15,5) = (a11 + a12 * 2 + a13 + 2) >> 2;
    DST(14,0) = DST(15,2) = (a13 + a14 + 1) >> 1;
    DST(14,1) = DST(15,3) = (a12 + a13 * 2 + a14 + 2) >> 2;
    DST(15,0) = (a14 + a15 + 1) >> 1;
    DST(15,1) = (a13 + a14 * 2 + a15 + 2) >> 2;
}

static void hor_down_4x4_c(uint8_t *dst, ptrdiff_t stride,
                           const uint8_t *left, const uint8_t *top)
{
    int l0 = left[0], l1 = left[1], l2 = left[2], l3 = left[3],
        tl = top[-1], a0 = top[0], a1 = top[1], a2 = top[2];

    DST(2,0) = (tl + a0 * 2 + a1 + 2) >> 2;
    DST(3,0) = (a0 + a1 * 2 + a2 + 2) >> 2;
    DST(0,0) = DST(2,1) = (tl + l0 + 1) >> 1;
    DST(1,0) = DST(3,1) = (a0 + tl * 2 + l0 + 2) >> 2;
    DST(0,1) = DST(2,2) = (l0 + l1 + 1) >> 1;
    DST(1,1) = DST(3,2) = (tl + l0 * 2 + l1 + 2) >> 2;
    DST(0,2) = DST(2,3) = (l1 + l2 + 1) >> 1;
    DST(1,2) = DST(3,3) = (l0 + l1 * 2 + l2 + 2) >> 2;
    DST(0,3) = (l2 + l3 + 1) >> 1;
    DST(1,3) = (l1 + l2 * 2 + l3 + 2) >> 2;
}

static void hor_down_8x8_c(uint8_t *dst, ptrdiff_t stride,
                           const uint8_t *left, const uint8_t *top)
{
    int l0 = left[0], l1 = left[1], l2 = left[2], l3 = left[3],
        l4 = left[4], l5 = left[5], l6 = left[6], l7 = left[7],
        tl = top[-1], a0 = top[0], a1 = top[1], a2 = top[2],
        a3 = top[3], a4 = top[4], a5 = top[5], a6 = top[6];

    DST(6,0) = (a3 + a4 * 2 + a5 + 2) >> 2;
    DST(7,0) = (a4 + a5 * 2 + a6 + 2) >> 2;
    DST(4,0) = DST(6,1) = (a1 + a2 * 2 + a3 + 2) >> 2;
    DST(5,0) = DST(7,1) = (a2 + a3 * 2 + a4 + 2) >> 2;
    DST(2,0) = DST(4,1) = DST(6,2) = (tl + a0 * 2 + a1 + 2) >> 2;
    DST(3,0) = DST(5,1) = DST(7,2) = (a0 + a1 * 2 + a2 + 2) >> 2;
    DST(0,0) = DST(2,1) = DST(4,2) = DST(6,3) = (tl + l0 + 1) >> 1;
    DST(1,0) = DST(3,1) = DST(5,2) = DST(7,3) = (a0 + tl * 2 + l0 + 2) >> 2;
    DST(0,1) = DST(2,2) = DST(4,3) = DST(6,4) = (l0 + l1 + 1) >> 1;
    DST(1,1) = DST(3,2) = DST(5,3) = DST(7,4) = (tl + l0 * 2 + l1 + 2) >> 2;
    DST(0,2) = DST(2,3) = DST(4,4) = DST(6,5) = (l1 + l2 + 1) >> 1;
    DST(1,2) = DST(3,3) = DST(5,4) = DST(7,5) = (l0 + l1 * 2 + l2 + 2) >> 2;
    DST(0,3) = DST(2,4) = DST(4,5) = DST(6,6) = (l2 + l3 + 1) >> 1;
    DST(1,3) = DST(3,4) = DST(5,5) = DST(7,6) = (l1 + l2 * 2 + l3 + 2) >> 2;
    DST(0,4) = DST(2,5) = DST(4,6) = DST(6,7) = (l3 + l4 + 1) >> 1;
    DST(1,4) = DST(3,5) = DST(5,6) = DST(7,7) = (l2 + l3 * 2 + l4 + 2) >> 2;
    DST(0,5) = DST(2,6) = DST(4,7) = (l4 + l5 + 1) >> 1;
    DST(1,5) = DST(3,6) = DST(5,7) = (l3 + l4 * 2 + l5 + 2) >> 2;
    DST(0,6) = DST(2,7) = (l5 + l6 + 1) >> 1;
    DST(1,6) = DST(3,7) = (l4 + l5 * 2 + l6 + 2) >> 2;
    DST(0,7) = (l6 + l7 + 1) >> 1;
    DST(1,7) = (l5 + l6 * 2 + l7 + 2) >> 2;
}

static void hor_down_16x16_c(uint8_t *dst, ptrdiff_t stride,
                             const uint8_t *left, const uint8_t *top)
{
    int tl = top[-1], a0 = top[0], a1 = top[1], a2 = top[2],
        a3 = top[3], a4 = top[4], a5 = top[5], a6 = top[6],
        a7 = top[7], a8 = top[8], a9 = top[9], a10 = top[10],
        a11 = top[11], a12 = top[12], a13 = top[13], a14 = top[14],
        l0 = left[0], l1 = left[1], l2 = left[2], l3 = left[3],
        l4 = left[4], l5 = left[5], l6 = left[6], l7 = left[7],
        l8 = left[8], l9 = left[9], l10 = left[10], l11 = left[11],
        l12 = left[12], l13 = left[13], l14 = left[14], l15 = left[15];

    DST(14,0) = (a11 + a12 * 2 + a13 + 2) >> 2;
    DST(15,0) = (a12 + a13 * 2 + a14 + 2) >> 2;
    DST(12,0) = DST(14,1) = (a9 + a10 * 2 + a11 + 2) >> 2;
    DST(13,0) = DST(15,1) = (a10 + a11 * 2 + a12 + 2) >> 2;
    DST(10,0) = DST(12,1) = DST(14,2) = (a7 + a8 * 2 + a9 + 2) >> 2;
    DST(11,0) = DST(13,1) = DST(15,2) = (a8 + a9 * 2 + a10 + 2) >> 2;
    DST(8,0)  = DST(10,1) = DST(12,2) = DST(14,3) = (a5 + a6 * 2 + a7 + 2) >> 2;
    DST(9,0)  = DST(11,1) = DST(13,2) = DST(15,3) = (a6 + a7 * 2 + a8 + 2) >> 2;
    DST(6,0)  = DST(8,1)  = DST(10,2) = DST(12,3) =
                DST(14,4) = (a3 + a4 * 2 + a5 + 2) >> 2;
    DST(7,0)  = DST(9,1)  = DST(11,2) = DST(13,3) =
                DST(15,4) = (a4 + a5 * 2 + a6 + 2) >> 2;
    DST(4,0)  = DST(6,1)  = DST(8,2)  = DST(10,3) = DST(12,4) =
                DST(14,5) = (a1 + a2 * 2 + a3 + 2) >> 2;
    DST(5,0)  = DST(7,1)  = DST(9,2)  = DST(11,3) = DST(13,4) =
                DST(15,5) = (a2 + a3 * 2 + a4 + 2) >> 2;
    DST(2,0)  = DST(4,1)  = DST(6,2)  = DST(8,3)  = DST(10,4) = DST(12,5) =
                DST(14,6) = (tl + a0 * 2 + a1 + 2) >> 2;
    DST(3,0)  = DST(5,1)  = DST(7,2)  = DST(9,3)  = DST(11,4) = DST(13,5) =
                DST(15,6) = (a0 + a1 * 2 + a2 + 2) >> 2;
    DST(0,0)  = DST(2,1)  = DST(4,2)  = DST(6,3)  = DST(8,4)  = DST(10,5) =
                DST(12,6) = DST(14,7) = (tl + l0 + 1) >> 1;
    DST(1,0)  = DST(3,1)  = DST(5,2)  = DST(7,3)  = DST(9,4)  = DST(11,5) =
                DST(13,6) = DST(15,7) = (a0 + tl * 2 + l0 + 2) >> 2;
    DST(0,1)  = DST(2,2)  = DST(4,3)  = DST(6,4)  = DST(8,5)  = DST(10,6) =
                DST(12,7) = DST(14,8) = (l0 + l1 + 1) >> 1;
    DST(1,1)  = DST(3,2)  = DST(5,3)  = DST(7,4)  = DST(9,5)  = DST(11,6) =
                DST(13,7) = DST(15,8) = (tl + l0 * 2 + l1 + 2) >> 2;
    DST(0,2)  = DST(2,3)  = DST(4,4)  = DST(6,5)  = DST(8,6)  = DST(10,7) =
                DST(12,8) = DST(14,9) = (l1 + l2 + 1) >> 1;
    DST(1,2)  = DST(3,3)  = DST(5,4)  = DST(7,5)  = DST(9,6)  = DST(11,7) =
                DST(13,8) = DST(15,9) = (l0 + l1 * 2 + l2 + 2) >> 2;
    DST(0,3)  = DST(2,4)  = DST(4,5)  = DST(6,6)  = DST(8,7)  = DST(10,8) =
                DST(12,9) = DST(14,10) = (l2 + l3 + 1) >> 1;
    DST(1,3)  = DST(3,4)  = DST(5,5)  = DST(7,6)  = DST(9,7)  = DST(11,8) =
                DST(13,9) = DST(15,10) = (l1 + l2 * 2 + l3 + 2) >> 2;
    DST(0,4)  = DST(2,5)  = DST(4,6)  = DST(6,7)  = DST(8,8)  = DST(10,9) =
                DST(12,10) = DST(14,11) = (l3 + l4 + 1) >> 1;
    DST(1,4)  = DST(3,5)  = DST(5,6)  = DST(7,7)  = DST(9,8)  = DST(11,9) =
                DST(13,10) = DST(15,11) = (l2 + l3 * 2 + l4 + 2) >> 2;
    DST(0,5)  = DST(2,6)  = DST(4,7)  = DST(6,8)  = DST(8,9)  = DST(10,10) =
                DST(12,11) = DST(14,12) = (l4 + l5 + 1) >> 1;
    DST(1,5)  = DST(3,6)  = DST(5,7)  = DST(7,8)  = DST(9,9)  = DST(11,10) =
                DST(13,11) = DST(15,12) = (l3 + l4 * 2 + l5 + 2) >> 2;
    DST(0,6)  = DST(2,7)  = DST(4,8)  = DST(6,9)  = DST(8,10) = DST(10,11) =
                DST(12,12) = DST(14,13) = (l5 + l6 + 1) >> 1;
    DST(1,6)  = DST(3,7)  = DST(5,8)  = DST(7,9)  = DST(9,10) = DST(11,11) =
                DST(13,12) = DST(15,13) = (l4 + l5 * 2 + l6 + 2) >> 2;
    DST(0,7)  = DST(2,8)  = DST(4,9)  = DST(6,10) = DST(8,11) = DST(10,12) =
                DST(12,13) = DST(14,14) = (l6 + l7 + 1) >> 1;
    DST(1,7)  = DST(3,8)  = DST(5,9)  = DST(7,10) = DST(9,11) = DST(11,12) =
                DST(13,13) = DST(15,14) = (l5 + l6 * 2 + l7 + 2) >> 2;
    DST(0,8)  = DST(2,9)  = DST(4,10) = DST(6,11) = DST(8,12) = DST(10,13) =
                DST(12,14) = DST(14,15) = (l7 + l8 + 1) >> 1;
    DST(1,8)  = DST(3,9)  = DST(5,10) = DST(7,11) = DST(9,12) = DST(11,13) =
                DST(13,14) = DST(15,15) = (l6 + l7 * 2 + l8 + 2) >> 2;
    DST(0,9)  = DST(2,10) = DST(4,11) = DST(6,12) = DST(8,13) = DST(10,14) =
                DST(12,15) = (l8 + l9 + 1) >> 1;
    DST(1,9)  = DST(3,10) = DST(5,11) = DST(7,12) = DST(9,13) = DST(11,14) =
                DST(13,15) = (l7 + l8 * 2 + l9 + 2) >> 2;
    DST(0,10) = DST(2,11) = DST(4,12) = DST(6,13) = DST(8,14) =
                DST(10,15) = (l9 + l10 + 1) >> 1;
    DST(1,10) = DST(3,11) = DST(5,12) = DST(7,13) = DST(9,14) =
                DST(11,15) = (l8 + l9 * 2 + l10 + 2) >> 2;
    DST(0,11) = DST(2,12) = DST(4,13) = DST(6,14) =
                DST(8,15) = (l10 + l11 + 1) >> 1;
    DST(1,11) = DST(3,12) = DST(5,13) = DST(7,14) =
                DST(9,15) = (l9 + l10 * 2 + l11 + 2) >> 2;
    DST(0,12) = DST(2,13) = DST(4,14) = DST(6,15) = (l11 + l12 + 1) >> 1;
    DST(1,12) = DST(3,13) = DST(5,14) = DST(7,15) = (l10 + l11 * 2 + l12 + 2) >> 2;
    DST(0,13) = DST(2,14) = DST(4,15) = (l12 + l13 + 1) >> 1;
    DST(1,13) = DST(3,14) = DST(5,15) = (l11 + l12 * 2 + l13 + 2) >> 2;
    DST(0,14) = DST(2,15) = (l13 + l14 + 1) >> 1;
    DST(1,14) = DST(3,15) = (l12 + l13 * 2 + l14 + 2) >> 2;
    DST(0,15) = (l14 + l15 + 1) >> 1;
    DST(1,15) = (l13 + l14 * 2 + l15 + 2) >> 2;
}

static void vert_left_4x4_c(uint8_t *dst, ptrdiff_t stride,
                            const uint8_t *left, const uint8_t *top)
{
    int a0 = top[0], a1 = top[1], a2 = top[2], a3 = top[3],
        a4 = top[4], a5 = top[5], a6 = top[6];
    
    DST(0,0) = (a0 + a1 + 1) >> 1;
    DST(0,1) = (a0 + a1 * 2 + a2 + 2) >> 2;
    DST(1,0) = DST(0,2) = (a1 + a2 + 1) >> 1;
    DST(1,1) = DST(0,3) = (a1 + a2 * 2 + a3 + 2) >> 2;
    DST(2,0) = DST(1,2) = (a2 + a3 + 1) >> 1;
    DST(2,1) = DST(1,3) = (a2 + a3 * 2 + a4 + 2) >> 2;
    DST(3,0) = DST(2,2) = (a3 + a4 + 1) >> 1;
    DST(3,1) = DST(2,3) = (a3 + a4 * 2 + a5 + 2) >> 2;
    DST(3,2) = (a4 + a5 + 1) >> 1;
    DST(3,3) = (a4 + a5 * 2 + a6 + 2) >> 2;
}

static void vert_left_8x8_c(uint8_t *dst, ptrdiff_t stride,
                            const uint8_t *left, const uint8_t *top)
{
    int a0 = top[0], a1 = top[1], a2 = top[2], a3 = top[3],
        a4 = top[4], a5 = top[5], a6 = top[6], a7 = top[7];

    DST(0,0) = (a0 + a1 + 1) >> 1;
    DST(0,1) = (a0 + a1 * 2 + a2 + 2) >> 2;
    DST(1,0) = DST(0,2) = (a1 + a2 + 1) >> 1;
    DST(1,1) = DST(0,3) = (a1 + a2 * 2 + a3 + 2) >> 2;
    DST(2,0) = DST(1,2) = DST(0,4) = (a2 + a3 + 1) >> 1;
    DST(2,1) = DST(1,3) = DST(0,5) = (a2 + a3 * 2 + a4 + 2) >> 2;
    DST(3,0) = DST(2,2) = DST(1,4) = DST(0,6) = (a3 + a4 + 1) >> 1;
    DST(3,1) = DST(2,3) = DST(1,5) = DST(0,7) = (a3 + a4 * 2 + a5 + 2) >> 2;
    DST(4,0) = DST(3,2) = DST(2,4) = DST(1,6) = (a4 + a5 + 1) >> 1;
    DST(4,1) = DST(3,3) = DST(2,5) = DST(1,7) = (a4 + a5 * 2 + a6 + 2) >> 2;
    DST(5,0) = DST(4,2) = DST(3,4) = DST(2,6) = (a5 + a6 + 1) >> 1;
    DST(5,1) = DST(4,3) = DST(3,5) = DST(2,7) = (a5 + a6 * 2 + a7 + 2) >> 2;
    DST(6,0) = DST(5,2) = DST(4,4) = DST(3,6) = (a6 + a7 + 1) >> 1;
    DST(6,1) = DST(5,3) = DST(4,5) = DST(3,7) = (a6 + a7 * 3 + 2) >> 2;
    DST(7,0) = DST(7,1) = DST(6,2) = DST(7,2) = DST(6,3) = DST(7,3) =
               DST(5,4) = DST(6,4) = DST(7,4) = DST(5,5) = DST(6,5) =
               DST(7,5) = DST(4,6) = DST(5,6) = DST(6,6) = DST(7,6) =
               DST(4,7) = DST(5,7) = DST(6,7) = DST(7,7) = a7;
}

static void vert_left_16x16_c(uint8_t *dst, ptrdiff_t stride,
                              const uint8_t *left, const uint8_t *top)
{
    int a0 = top[0], a1 = top[1], a2 = top[2], a3 = top[3],
        a4 = top[4], a5 = top[5], a6 = top[6], a7 = top[7],
        a8 = top[8], a9 = top[9], a10 = top[10], a11 = top[11],
        a12 = top[12], a13 = top[13], a14 = top[14], a15 = top[15];

    DST(0,0)  = (a0 + a1 + 1) >> 1;
    DST(0,1)  = (a0 + a1 * 2 + a2 + 2) >> 2;
    DST(1,0)  = DST(0,2)  = (a1 + a2 + 1) >> 1;
    DST(1,1)  = DST(0,3)  = (a1 + a2 * 2 + a3 + 2) >> 2;
    DST(2,0)  = DST(1,2)  = DST(0,4)  = (a2 + a3 + 1) >> 1;
    DST(2,1)  = DST(1,3)  = DST(0,5)  = (a2 + a3 * 2 + a4 + 2) >> 2;
    DST(3,0)  = DST(2,2)  = DST(1,4)  = DST(0,6)  = (a3 + a4 + 1) >> 1;
    DST(3,1)  = DST(2,3)  = DST(1,5)  = DST(0,7)  = (a3 + a4 * 2 + a5 + 2) >> 2;
    DST(4,0)  = DST(3,2)  = DST(2,4)  = DST(1,6)  =
                DST(0,8) = (a4 + a5 + 1) >> 1;
    DST(4,1)  = DST(3,3)  = DST(2,5)  = DST(1,7)  =
                DST(0,9) = (a4 + a5 * 2 + a6 + 2) >> 2;
    DST(5,0)  = DST(4,2)  = DST(3,4)  = DST(2,6)  = DST(1,8)  =
                DST(0,10) = (a5 + a6 + 1) >> 1;
    DST(5,1)  = DST(4,3)  = DST(3,5)  = DST(2,7)  = DST(1,9)  =
                DST(0,11) = (a5 + a6 * 2 + a7 + 2) >> 2;
    DST(6,0)  = DST(5,2)  = DST(4,4)  = DST(3,6)  = DST(2,8)  = DST(1,10) =
                DST(0,12) = (a6 + a7 + 1) >> 1;
    DST(6,1)  = DST(5,3)  = DST(4,5)  = DST(3,7)  = DST(2,9)  = DST(1,11) =
                DST(0,13) = (a6 + a7 * 3 + 2) >> 2;
    DST(7,0)  = DST(6,2)  = DST(5,4)  = DST(4,6)  = DST(3,8)  = DST(2,10) =
                DST(1,12) = DST(0,14) = (a7 + a8 + 1) >> 1;
    DST(7,1)  = DST(6,3)  = DST(5,5)  = DST(4,7)  = DST(3,9)  = DST(2,11) =
                DST(1,13) = DST(0,15) = (a7 + a8 * 2 + a9 + 2) >> 2;
    DST(8,0)  = DST(7,2)  = DST(6,4)  = DST(5,6)  = DST(4,8)  = DST(3,10) =
                DST(2,12) = DST(1,14) = (a8 + a9 + 1) >> 1;
    DST(8,1)  = DST(7,3)  = DST(6,5)  = DST(5,7)  = DST(4,9)  = DST(3,11) =
                DST(2,13) = DST(1,15) = (a8 + a9 * 2 + a10 + 2) >> 2;
    DST(9,0)  = DST(8,2)  = DST(7,4)  = DST(6,6)  = DST(5,8)  = DST(4,10) =
                DST(3,12) = DST(2,14) = (a9 + a10 + 1) >> 1;
    DST(9,1)  = DST(8,3)  = DST(7,5)  = DST(6,7)  = DST(5,9)  = DST(4,11) =
                DST(3,13) = DST(2,15) = (a9 + a10 * 2 + a11 + 2) >> 2;
    DST(10,0) = DST(9,2)  = DST(8,4)  = DST(7,6)  = DST(6,8)  = DST(5,10) =
                DST(4,12) = DST(3,14) = (a10 + a11 + 1) >> 1;
    DST(10,1) = DST(9,3)  = DST(8,5)  = DST(7,7)  = DST(6,9)  = DST(5,11) =
                DST(4,13) = DST(3,15) = (a10 + a11 * 2 + a12 + 2) >> 2;
    DST(11,0) = DST(10,2) = DST(9,4)  = DST(8,6)  = DST(7,8)  = DST(6,10) =
                DST(5,12) = DST(4,14) = (a11 + a12 + 1) >> 1;
    DST(11,1) = DST(10,3) = DST(9,5)  = DST(8,7)  = DST(7,9)  = DST(6,11) =
                DST(5,13) = DST(4,15) = (a11 + a12 * 2 + a13 + 2) >> 2;
    DST(12,0) = DST(11,2) = DST(10,4) = DST(9,6)  = DST(8,8)  = DST(7,10) =
                DST(6,12) = DST(5,14) = (a12 + a13 + 1) >> 1;
    DST(12,1) = DST(11,3) = DST(10,5) = DST(9,7)  = DST(8,9)  = DST(7,11) =
                DST(6,13) = DST(5,15) = (a12 + a13 * 2 + a14 + 2) >> 2;
    DST(13,0) = DST(12,2) = DST(11,4) = DST(10,6) = DST(9,8)  = DST(8,10) =
                DST(7,12) = DST(6,14) = (a13 + a14 + 1) >> 1;
    DST(13,1) = DST(12,3) = DST(11,5) = DST(10,7) = DST(9,9)  = DST(8,11) =
                DST(7,13) = DST(6,15) = (a13 + a14 * 2 + a15 + 2) >> 2;
    DST(14,0) = DST(13,2) = DST(12,4) = DST(11,6) = DST(10,8) = DST(9,10) =
                DST(8,12) = DST(7,14) = (a14 + a15 + 1) >> 1;
    DST(14,1) = DST(13,3) = DST(12,5) = DST(11,7) = DST(10,9) = DST(9,11) =
                DST(8,13) = DST(7,15) = (a14 + a15 * 3 + 2) >> 2;
    DST(15,0) = DST(15,1) = DST(14,2) = DST(15,2) = DST(14,3) = DST(15,3) =
                DST(13,4) = DST(14,4) = DST(15,4) = DST(13,5) = DST(14,5) =
                DST(15,5) = DST(12,6) = DST(13,6) = DST(14,6) = DST(15,6) =
                DST(12,7) = DST(13,7) = DST(14,7) = DST(15,7) = DST(11,8) =
                DST(12,8) = DST(13,8) = DST(14,8) = DST(15,8) = DST(11,9) =
                DST(12,9) = DST(13,9) = DST(14,9) = DST(15,9) = DST(10,10) =
                DST(11,10) = DST(12,10) = DST(13,10) = DST(14,10) = DST(15,10) =
                DST(10,11) = DST(11,11) = DST(12,11) = DST(13,11) = DST(14,11) =
                DST(15,11) = DST(9,12) = DST(10,12) = DST(11,12) = DST(12,12) =
                DST(13,12) = DST(14,12) = DST(15,12) = DST(9,13) = DST(10,13) =
                DST(11,13) = DST(12,13) = DST(13,13) = DST(14,13) = DST(15,13) =
                DST(8,14) = DST(9,14) = DST(10,14) = DST(11,14) = DST(12,14) =
                DST(13,14) = DST(14,14) = DST(15,14) = DST(8,15) = DST(9,15) =
                DST(10,15) = DST(11,15) = DST(12,15) = DST(13,15) = DST(14,15) =
                DST(15,15) = a15;
}

static void hor_up_4x4_c(uint8_t *dst, ptrdiff_t stride,
                         const uint8_t *left, const uint8_t *top)
{
    int l0 = left[0], l1 = left[1], l2 = left[2], l3 = left[3];

    DST(0,0) = (l0 + l1 + 1) >> 1;
    DST(1,0) = (l0 + l1 * 2 + l2 + 2) >> 2;
    DST(0,1) = DST(2,0) = (l1 + l2 + 1) >> 1;
    DST(1,1) = DST(3,0) = (l1 + l2 * 2 + l3 + 2) >> 2;
    DST(0,2) = DST(2,1) = (l2 + l3 + 1) >> 1;
    DST(1,2) = DST(3,1) = (l2 + l3 * 3 + 2) >> 2;
    DST(0,3) = DST(1,3) = DST(2,2) = DST(2,3) = DST(3,2) = DST(3,3) = l3;
}

static void hor_up_8x8_c(uint8_t *dst, ptrdiff_t stride,
                         const uint8_t *left, const uint8_t *top)
{
    int l0 = left[0], l1 = left[1], l2 = left[2], l3 = left[3],
        l4 = left[4], l5 = left[5], l6 = left[6], l7 = left[7];

    DST(0,0) = (l0 + l1 + 1) >> 1;
    DST(1,0) = (l0 + l1 * 2 + l2 + 2) >> 2;
    DST(0,1) = DST(2,0) = (l1 + l2 + 1) >> 1;
    DST(1,1) = DST(3,0) = (l1 + l2 * 2 + l3 + 2) >> 2;
    DST(0,2) = DST(2,1) = DST(4,0) = (l2 + l3 + 1) >> 1;
    DST(1,2) = DST(3,1) = DST(5,0) = (l2 + l3 * 3 + 2) >> 2;
    DST(0,3) = DST(2,2) = DST(4,1) = DST(6,0) = (l3 + l4 + 1) >> 1;
    DST(1,3) = DST(3,2) = DST(5,1) = DST(7,0) = (l3 + l4 * 2 + l5 + 2) >> 2;
    DST(0,4) = DST(2,3) = DST(4,2) = DST(6,1) = (l4 + l5 + 1) >> 1;
    DST(1,4) = DST(3,3) = DST(5,2) = DST(7,1) = (l4 + l5 * 2 + l6 + 2) >> 2;
    DST(0,5) = DST(2,4) = DST(4,3) = DST(6,2) = (l5 + l6 + 1) >> 2;
    DST(1,5) = DST(3,4) = DST(5,3) = DST(7,2) = (l5 + l6 * 2 + l7 + 2) >> 2;
    DST(0,6) = DST(2,5) = DST(4,4) = DST(6,3) = (l6 + l7 + 1) >> 1;
    DST(1,6) = DST(3,5) = DST(5,4) = DST(7,3) = (l6 + l7 * 3 + 2) >> 2;
    DST(0,7) = DST(1,7) = DST(2,6) = DST(2,7) = DST(3,6) = DST(3,7) = 
               DST(4,5) = DST(4,6) = DST(4,7) = DST(5,5) = DST(5,6) =
               DST(5,7) = DST(6,4) = DST(6,5) = DST(6,6) = DST(6,7) =
               DST(7,4) = DST(7,5) = DST(7,6) = DST(7,7) = l7;
}

static void hor_up_16x16_c(uint8_t *dst, ptrdiff_t stride,
                           const uint8_t *left, const uint8_t *top)
{
    int l0 = left[0], l1 = left[1], l2 = left[2], l3 = left[3],
        l4 = left[4], l5 = left[5], l6 = left[6], l7 = left[7],
        l8 = left[8], l9 = left[9], l10 = left[10], l11 = left[11],
        l12 = left[12], l13 = left[13], l14 = left[14], l15 = left[15];

    DST(0,0)  = (l0 + l1 + 1) >> 1;
    DST(1,0)  = (l0 + l1 * 2 + l2 + 2) >> 2;
    DST(0,1)  = DST(2,0)  = (l1 + l2 + 1) >> 1;
    DST(1,1)  = DST(3,0)  = (l1 + l2 * 2 + l3 + 2) >> 2;
    DST(0,2)  = DST(2,1)  = DST(4,0)  = (l2 + l3 + 1) >> 1;
    DST(1,2)  = DST(3,1)  = DST(5,0)  = (l2 + l3 * 3 + 2) >> 2;
    DST(0,3)  = DST(2,2)  = DST(4,1)  = DST(6,0)  = (l3 + l4 + 1) >> 1;
    DST(1,3)  = DST(3,2)  = DST(5,1)  = DST(7,0)  = (l3 + l4 * 2 + l5 + 2) >> 2;
    DST(0,4)  = DST(2,3)  = DST(4,2)  = DST(6,1)  =
                DST(8,0)  = (l4 + l5 + 1) >> 1;
    DST(1,4)  = DST(3,3)  = DST(5,2)  = DST(7,1)  =
                DST(9,0)  = (l4 + l5 * 2 + l6 + 2) >> 2;
    DST(0,5)  = DST(2,4)  = DST(4,3)  = DST(6,2)  = DST(8,1)  =
                DST(10,0) = (l5 + l6 + 1) >> 2;
    DST(1,5)  = DST(3,4)  = DST(5,3)  = DST(7,2)  = DST(9,1)  =
                DST(11,0) = (l5 + l6 * 2 + l7 + 2) >> 2;
    DST(0,6)  = DST(2,5)  = DST(4,4)  = DST(6,3)  = DST(8,2)  = DST(10,1) =
                DST(12,0) = (l6 + l7 + 1) >> 1;
    DST(1,6)  = DST(3,5)  = DST(5,4)  = DST(7,3)  = DST(9,2)  = DST(11,1) =
                DST(13,0) = (l6 + l7 * 3 + 2) >> 2;
    DST(0,7)  = DST(2,6)  = DST(4,5)  = DST(6,4)  = DST(8,3)  = DST(10,2) =
                DST(12,1) = DST(14,0) = (l7 + l8 + 1) >> 1;
    DST(1,7)  = DST(3,6)  = DST(5,5)  = DST(7,4)  = DST(9,3)  = DST(11,2) =
                DST(13,1) = DST(15,0) = (l7 + l8 * 2 + l9 + 2) >> 2;
    DST(0,8)  = DST(2,7)  = DST(4,6)  = DST(6,5)  = DST(8,4)  = DST(10,3) =
                DST(12,2) = DST(14,1) = (l8 + l9 + 1) >> 1;
    DST(1,8)  = DST(3,7)  = DST(5,6)  = DST(7,5)  = DST(9,4)  = DST(11,3) =
                DST(13,2) = DST(15,1) = (l8 + l9 * 2 + l10 + 2) >> 2;
    DST(0,9)  = DST(2,8)  = DST(4,7)  = DST(6,6)  = DST(8,5)  = DST(10,4) =
                DST(12,3) = DST(14,2) = (l9 + l10 + 1) >> 1;
    DST(1,9)  = DST(3,8)  = DST(5,7)  = DST(7,6)  = DST(9,5)  = DST(11,4) =
                DST(13,3) = DST(15,2) = (l9 + l10 * 2 + l11 + 2) >> 2;
    DST(0,10) = DST(2,9)  = DST(4,8)  = DST(6,7)  = DST(8,6)  = DST(10,5) =
                DST(12,4) = DST(14,3) = (l10 + l11 + 1) >> 1;
    DST(1,10) = DST(3,9)  = DST(5,8)  = DST(7,7)  = DST(9,6)  = DST(11,5) =
                DST(13,4) = DST(15,3) = (l10 + l11 * 2 + l12 + 2) >> 2;
    DST(0,11) = DST(2,10) = DST(4,9)  = DST(6,8)  = DST(8,7)  = DST(10,6) =
                DST(12,5) = DST(14,4) = (l11 + l12 + 1) >> 1;
    DST(1,11) = DST(3,10) = DST(5,9)  = DST(7,8)  = DST(9,7)  = DST(11,6) =
                DST(13,5) = DST(15,4) = (l11 + l12 * 2 + l13 + 2) >> 2;
    DST(0,12) = DST(2,11) = DST(4,10) = DST(6,9)  = DST(8,8)  = DST(10,7) =
                DST(12,6) = DST(14,5) = (l12 + l13 + 1) >> 1;
    DST(1,12) = DST(3,11) = DST(5,10) = DST(7,9)  = DST(9,8)  = DST(11,7) =
                DST(13,6) = DST(15,5) = (l12 + l13 * 2 + l14 + 2) >> 2;
    DST(0,13) = DST(2,12) = DST(4,11) = DST(6,10) = DST(8,9)  = DST(10,8) =
                DST(12,7) = DST(14,6) = (l13 + l14 + 1) >> 1;
    DST(1,13) = DST(3,12) = DST(5,11) = DST(7,10) = DST(9,9)  = DST(11,8) =
                DST(13,7) = DST(15,6) = (l13 + l14 * 2 + l15 + 2) >> 2;
    DST(0,14) = DST(2,13) = DST(4,12) = DST(6,11) = DST(8,10) = DST(10,9) =
                DST(12,8) = DST(14,7) = (l14 + l15 + 1) >> 1;
    DST(1,14) = DST(3,13) = DST(5,12) = DST(7,11) = DST(9,10) = DST(11,9) =
                DST(13,8) = DST(15,7) = (l14 + l15 * 3 + 2) >> 2;
    DST(0,15) = DST(1,15) = DST(2,14) = DST(2,15) = DST(3,14) = DST(3,15) = 
                DST(4,13) = DST(4,14) = DST(4,15) = DST(5,13) = DST(5,14) =
                DST(5,15) = DST(6,12) = DST(6,13) = DST(6,14) = DST(6,15) =
                DST(7,12) = DST(7,13) = DST(7,14) = DST(7,15) = DST(8,11) =
                DST(8,12) = DST(8,13) = DST(8,14) = DST(8,15) = DST(9,11) =
                DST(9,12) = DST(9,13) = DST(9,14) = DST(9,15) = DST(10,10) =
                DST(10,11) = DST(10,12) = DST(10,13) = DST(10,14) = DST(10,15) =
                DST(11,10) = DST(11,11) = DST(11,12) = DST(11,13) = DST(11,14) =
                DST(11,15) = DST(12,9) = DST(12,10) = DST(12,11) = DST(12,12) =
                DST(12,13) = DST(12,14) = DST(12,15) = DST(13,9) = DST(13,10) =
                DST(13,11) = DST(13,12) = DST(13,13) = DST(13,14) = DST(13,15) =
                DST(14,8) = DST(14,9) = DST(14,10) = DST(14,11) = DST(14,12) =
                DST(14,13) = DST(14,14) = DST(14,15) = DST(15,8) = DST(15,9) =
                DST(15,10) = DST(15,11) = DST(15,12) = DST(15,13) = DST(15,14) =
                DST(15,15) = l15;
}

#undef DST

static void vp9dsp_intrapred_init(VP9DSPContext *dsp)
{
#define init_intra_pred(tx, sz) \
    dsp->intra_pred[tx][VERT_PRED]            = vert_##sz##_c; \
    dsp->intra_pred[tx][HOR_PRED]             = hor_##sz##_c; \
    dsp->intra_pred[tx][DC_PRED]              = dc_##sz##_c; \
    dsp->intra_pred[tx][DIAG_DOWN_LEFT_PRED]  = diag_downleft_##sz##_c; \
    dsp->intra_pred[tx][DIAG_DOWN_RIGHT_PRED] = diag_downright_##sz##_c; \
    dsp->intra_pred[tx][VERT_RIGHT_PRED]      = vert_right_##sz##_c; \
    dsp->intra_pred[tx][HOR_DOWN_PRED]        = hor_down_##sz##_c; \
    dsp->intra_pred[tx][VERT_LEFT_PRED]       = vert_left_##sz##_c; \
    dsp->intra_pred[tx][HOR_UP_PRED]          = hor_up_##sz##_c; \
    dsp->intra_pred[tx][TM_VP8_PRED]          = tm_##sz##_c; \
    dsp->intra_pred[tx][LEFT_DC_PRED]         = dc_left_##sz##_c; \
    dsp->intra_pred[tx][TOP_DC_PRED]          = dc_top_##sz##_c; \
    dsp->intra_pred[tx][DC_128_PRED]          = dc_128_##sz##_c; \
    dsp->intra_pred[tx][DC_127_PRED]          = dc_127_##sz##_c; \
    dsp->intra_pred[tx][DC_129_PRED]          = dc_129_##sz##_c

    init_intra_pred(TX_4X4,   4x4);
    init_intra_pred(TX_8X8,   8x8);
    init_intra_pred(TX_16X16, 16x16);
    // FIXME
    //init_intra_pred(TX_32X32, 32x32);

#undef init_intra_pred
}

#define itxfm_wrapper(type_a, type_b, sz, bits) \
static void type_a##_##type_b##_##sz##x##sz##_add_c(uint8_t *dst, \
                                                    ptrdiff_t stride, \
                                                    int16_t *block, int eob) \
{ \
    int i, j; \
    int16_t tmp[sz * sz], out[sz]; \
    for (i = 0; i < sz; i++) { \
        type_a##sz##_1d(block, 1, tmp + i * sz); \
        for (j = 0; j < sz; j++) \
            block[j] = 0; \
        block += sz; \
    } \
    for (i = 0; i < sz; i++) { \
        type_b##sz##_1d(tmp + i, sz, out); \
        for (j = 0; j < sz; j++) \
            dst[j * stride] = av_clip_uint8(dst[j * stride] + \
                                            (bits ? \
                                             (out[j] + (1 << (bits - 1))) >> bits : \
                                             out[j])); \
        dst++; \
    } \
}

#define itxfm_wrap(sz, bits) \
itxfm_wrapper(idct,  idct,  sz, bits) \
itxfm_wrapper(iadst, idct,  sz, bits) \
itxfm_wrapper(idct,  iadst, sz, bits) \
itxfm_wrapper(iadst, iadst, sz, bits)

#define IN(x) in[x * stride]

static av_always_inline void idct4_1d(const int16_t *in, ptrdiff_t stride,
                                      int16_t *out)
{
    int t0, t1, t2, t3;

    t0 = ((IN(0) + IN(2)) * 11585 + (1 << 13)) >> 14;
    t1 = ((IN(0) - IN(2)) * 11585 + (1 << 13)) >> 14;
    t2 = (IN(1) *  6270 - IN(3) * 15137 + (1 << 13)) >> 14;
    t3 = (IN(1) * 15137 + IN(3) *  6270 + (1 << 13)) >> 14;

    out[0] = t0 + t3;
    out[1] = t1 + t2;
    out[2] = t1 - t2;
    out[3] = t0 - t3;
}

static av_always_inline void iadst4_1d(const int16_t *in, ptrdiff_t stride,
                                       int16_t *out)
{
    int t0, t1, t2, t3;

    t0 =  5283 * IN(0) + 15212 * IN(2) +  9929 * IN(3);
    t1 =  9929 * IN(0) -  5283 * IN(2) - 15212 * IN(3);
    t2 = 13377 * (IN(0) - IN(2) + IN(3));
    t3 = 13377 * IN(1);

    out[0] = (t0 + t3      + (1 << 13)) >> 14;
    out[1] = (t1 + t3      + (1 << 13)) >> 14;
    out[2] = (t2           + (1 << 13)) >> 14;
    out[3] = (t0 + t1 - t3 + (1 << 13)) >> 14;
}

itxfm_wrap(4, 4)

static av_always_inline void idct8_1d(const int16_t *in, ptrdiff_t stride,
                                      int16_t *out)
{
    int t0, t0a, t1, t1a, t2, t2a, t3, t3a, t4, t4a, t5, t5a, t6, t6a, t7, t7a;

    t0a = ((IN(0) + IN(4)) * 11585 + (1 << 13)) >> 14;
    t1a = ((IN(0) - IN(4)) * 11585 + (1 << 13)) >> 14;
    t2a = (IN(2) *  6270 - IN(6) * 15137 + (1 << 13)) >> 14;
    t3a = (IN(2) * 15137 + IN(6) *  6270 + (1 << 13)) >> 14;
    t4a = (IN(1) *  3196 - IN(7) * 16069 + (1 << 13)) >> 14;
    t5a = (IN(1) * 16069 + IN(7) *  3196 + (1 << 13)) >> 14;
    t6a = (IN(5) * 13623 - IN(3) *  9102 + (1 << 13)) >> 14;
    t7a = (IN(5) *  9102 + IN(3) * 13623 + (1 << 13)) >> 14;

    t0  = t0a + t3a;
    t1  = t1a + t2a;
    t2  = t1a - t2a;
    t3  = t0a - t3a;
    t4  = t4a + t5a;
    t5a = t4a - t5a;
    t7  = t7a + t6a;
    t6a = t7a - t6a;

    t5  = ((t6a - t5a) * 11585 + (1 << 13)) >> 14;
    t6  = ((t6a + t5a) * 11585 + (1 << 13)) >> 14;

    out[0] = t0 + t7;
    out[1] = t1 + t6;
    out[2] = t2 + t5;
    out[3] = t3 + t4;
    out[4] = t3 - t4;
    out[5] = t2 - t5;
    out[6] = t1 - t6;
    out[7] = t0 - t7;
}

static av_always_inline void iadst8_1d(const int16_t *in, ptrdiff_t stride,
                                       int16_t *out)
{
    int t0, t0a, t1, t1a, t2, t2a, t3, t3a, t4, t4a, t5, t5a, t6, t6a, t7, t7a;

    t0a = 16305 * IN(7) +  1606 * IN(0);
    t1a =  1606 * IN(7) - 16305 * IN(0);
    t2a = 14449 * IN(5) +  7723 * IN(2);
    t3a =  7723 * IN(5) - 14449 * IN(2);
    t4a = 10394 * IN(3) + 12665 * IN(4);
    t5a = 12665 * IN(3) - 10394 * IN(4);
    t6a =  4756 * IN(1) + 15679 * IN(6);
    t7a = 15679 * IN(1) -  4756 * IN(6);

    t0 = (t0a + t4a + (1 << 13)) >> 14;
    t1 = (t1a + t5a + (1 << 13)) >> 14;
    t2 = (t2a + t6a + (1 << 13)) >> 14;
    t3 = (t3a + t7a + (1 << 13)) >> 14;
    t4 = (t0a - t4a + (1 << 13)) >> 14;
    t5 = (t1a - t5a + (1 << 13)) >> 14;
    t6 = (t2a - t6a + (1 << 13)) >> 14;
    t7 = (t3a - t7a + (1 << 13)) >> 14;

    t4a = 15137 * t4 +  6270 * t5;
    t5a =  6270 * t4 - 15137 * t5;
    t6a = 15137 * t7 -  6270 * t6;
    t7a =  6270 * t7 - 15137 * t6;

    out[0] =   t0 + t2;
    out[7] = -(t1 + t3);
    t2     =   t0 - t2;
    t3     =   t1 - t3;

    out[1] = -((t4a + t6a + (1 << 13)) >> 14);
    out[6] =   (t5a + t7a + (1 << 13)) >> 14;
    t6     =   (t4a - t6a + (1 << 13)) >> 14;
    t7     =   (t5a - t7a + (1 << 13)) >> 14;

    out[3] = -(((t2 + t3) * 11585 + (1 << 13)) >> 14);
    out[4] =   ((t2 - t3) * 11585 + (1 << 13)) >> 14;
    out[2] =   ((t6 + t7) * 11585 + (1 << 13)) >> 14;
    out[5] = -(((t6 - t7) * 11585 + (1 << 13)) >> 14);
}

itxfm_wrap(8, 5)

static av_always_inline void idct16_1d(const int16_t *in, ptrdiff_t stride,
                                       int16_t *out)
{
    int t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15;
    int t0a, t1a, t2a, t3a, t4a, t5a, t6a, t7a;
    int t8a, t9a, t10a, t11a, t12a, t13a, t14a, t15a;

    t0a  = ((IN(0) + IN(8)) * 11585 + (1 << 13)) >> 14;
    t1a  = ((IN(0) - IN(8)) * 11585 + (1 << 13)) >> 14;
    t2a  = (IN(4)  *  6270 - IN(12) * 15137 + (1 << 13)) >> 14;
    t3a  = (IN(4)  * 15137 + IN(12) *  6270 + (1 << 13)) >> 14;
    t4a  = (IN(2)  *  3196 - IN(14) * 16069 + (1 << 13)) >> 14;
    t7a  = (IN(2)  * 16069 + IN(14) *  3196 + (1 << 13)) >> 14;
    t5a  = (IN(10) * 13623 - IN(6)  *  9102 + (1 << 13)) >> 14;
    t6a  = (IN(10) *  9102 + IN(6)  * 13623 + (1 << 13)) >> 14;
    t8a  = (IN(1)  *  1606 - IN(15) * 16305 + (1 << 13)) >> 14;
    t15a = (IN(1)  * 16305 + IN(15) *  1606 + (1 << 13)) >> 14;
    t9a  = (IN(9)  * 12665 - IN(7)  * 10394 + (1 << 13)) >> 14;
    t14a = (IN(9)  * 10394 + IN(7)  * 12665 + (1 << 13)) >> 14;
    t10a = (IN(5)  *  7723 - IN(11) * 14449 + (1 << 13)) >> 14;
    t13a = (IN(5)  * 14449 + IN(11) *  7723 + (1 << 13)) >> 14;
    t11a = (IN(13) * 15679 - IN(3)  *  4756 + (1 << 13)) >> 14;
    t12a = (IN(13) *  4756 + IN(3)  * 15679 + (1 << 13)) >> 14;

    t0  = t0a  + t3a;
    t1  = t1a  + t2a;
    t2  = t1a  - t2a;
    t3  = t0a  - t3a;
    t4  = t4a  + t5a;
    t5  = t4a  - t5a;
    t6  = t7a  - t6a;
    t7  = t7a  + t6a;
    t8  = t8a  + t9a;
    t9  = t8a  - t9a;
    t10 = t11a - t10a;
    t11 = t11a + t10a;
    t12 = t12a + t13a;
    t13 = t12a - t13a;
    t14 = t15a - t14a;
    t15 = t15a + t14a;

    t5a  = ((t6 - t5) * 11585 + (1 << 13)) >> 14;
    t6a  = ((t6 + t5) * 11585 + (1 << 13)) >> 14;
    t9a  = (  t14 *  6270 - t9  * 15137  + (1 << 13)) >> 14;
    t14a = (  t14 * 15137 + t9  *  6270  + (1 << 13)) >> 14;
    t10a = (-(t13 * 15137 + t10 *  6270) + (1 << 13)) >> 14;
    t13a = (  t13 *  6270 - t10 * 15137  + (1 << 13)) >> 14;

    t0a  = t0   + t7;
    t1a  = t1   + t6a;
    t2a  = t2   + t5a;
    t3a  = t3   + t4;
    t4   = t3   - t4;
    t5   = t2   - t5a;
    t6   = t1   - t6a;
    t7   = t0   - t7;
    t8a  = t8   + t11;
    t9   = t9a  + t10a;
    t10  = t9a  - t10a;
    t11a = t8   - t11;
    t12a = t15  - t12;
    t13  = t14a - t13a;
    t14  = t14a + t13a;
    t15a = t15  + t12;

    t10a = ((t13  - t10)  * 11585 + (1 << 13)) >> 14;
    t13a = ((t13  + t10)  * 11585 + (1 << 13)) >> 14;
    t11  = ((t12a - t11a) * 11585 + (1 << 13)) >> 14;
    t12  = ((t12a + t11a) * 11585 + (1 << 13)) >> 14;

    out[ 0] = t0a + t15a;
    out[ 1] = t1a + t14;
    out[ 2] = t2a + t13a;
    out[ 3] = t3a + t12;
    out[ 4] = t4  + t11;
    out[ 5] = t5  + t10a;
    out[ 6] = t6  + t9;
    out[ 7] = t7  + t8a;
    out[ 8] = t7  - t8a;
    out[ 9] = t6  - t9;
    out[10] = t5  - t10a;
    out[11] = t4  - t11;
    out[12] = t3a - t12;
    out[13] = t2a - t13a;
    out[14] = t1a - t14;
    out[15] = t0a - t15a;
}

static av_always_inline void iadst16_1d(const int16_t *in, ptrdiff_t stride,
                                        int16_t *out)
{
    int t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15;
    int t0a, t1a, t2a, t3a, t4a, t5a, t6a, t7a;
    int t8a, t9a, t10a, t11a, t12a, t13a, t14a, t15a;

    t0  = IN(15) * 16364 + IN(0)  *   804;
    t1  = IN(15) *   804 - IN(0)  * 16364;
    t2  = IN(13) * 15893 + IN(2)  *  3981;
    t3  = IN(13) *  3981 - IN(2)  * 15893;
    t4  = IN(11) * 14811 + IN(4)  *  7005;
    t5  = IN(11) *  7005 - IN(4)  * 14811;
    t6  = IN(9)  * 13160 + IN(6)  *  9760;
    t7  = IN(9)  *  9760 - IN(6)  * 13160;
    t8  = IN(7)  * 11003 + IN(8)  * 12140;
    t9  = IN(7)  * 12140 - IN(8)  * 11003;
    t10 = IN(5)  *  8423 + IN(10) * 14053;
    t11 = IN(5)  * 14053 - IN(10) *  8423;
    t12 = IN(3)  *  5520 + IN(12) * 15426;
    t13 = IN(3)  * 15426 - IN(12) *  5520;
    t14 = IN(1)  *  2404 + IN(14) * 16207;
    t15 = IN(1)  * 16207 - IN(14) *  2404;

    t0a  = (t0 + t8  + (1 << 13)) >> 14;
    t1a  = (t1 + t9  + (1 << 13)) >> 14;
    t2a  = (t2 + t10 + (1 << 13)) >> 14;
    t3a  = (t3 + t11 + (1 << 13)) >> 14;
    t4a  = (t4 + t12 + (1 << 13)) >> 14;
    t5a  = (t5 + t13 + (1 << 13)) >> 14;
    t6a  = (t6 + t14 + (1 << 13)) >> 14;
    t7a  = (t7 + t15 + (1 << 13)) >> 14;
    t8a  = (t0 - t8  + (1 << 13)) >> 14;
    t9a  = (t1 - t9  + (1 << 13)) >> 14;
    t10a = (t2 - t10 + (1 << 13)) >> 14;
    t11a = (t3 - t11 + (1 << 13)) >> 14;
    t12a = (t4 - t12 + (1 << 13)) >> 14;
    t13a = (t5 - t13 + (1 << 13)) >> 14;
    t14a = (t6 - t14 + (1 << 13)) >> 14;
    t15a = (t7 - t15 + (1 << 13)) >> 14;

    t8   = t8a  * 16069 + t9a  *  3196;
    t9   = t8a  *  3196 - t9a  * 16069;
    t10  = t10a *  9102 + t11a * 13623;
    t11  = t10a * 13623 - t11a *  9102;
    t12  = t13a * 16069 - t12a *  3196;
    t13  = t13a *  3196 + t12a * 16069;
    t14  = t15a *  9102 - t14a * 13623;
    t15  = t15a * 13623 + t14a *  9102;

    t0   = t0a + t4a;
    t1   = t1a + t5a;
    t2   = t2a + t6a;
    t3   = t3a + t7a;
    t4   = t0a - t4a;
    t5   = t1a - t5a;
    t6   = t2a - t6a;
    t7   = t3a - t7a;
    t8a  = (t8  + t12 + (1 << 13)) >> 14;
    t9a  = (t9  + t13 + (1 << 13)) >> 14;
    t10a = (t10 + t14 + (1 << 13)) >> 14;
    t11a = (t11 + t15 + (1 << 13)) >> 14;
    t12a = (t8  - t12 + (1 << 13)) >> 14;
    t13a = (t9  - t13 + (1 << 13)) >> 14;
    t14a = (t10 - t14 + (1 << 13)) >> 14;
    t15a = (t11 - t15 + (1 << 13)) >> 14;

    t4a  = t4 * 15137 + t5 *  6270;
    t5a  = t4 *  6270 - t5 * 15137;
    t6a  = t7 * 15137 - t6 *  6270;
    t7a  = t7 *  6270 + t6 * 15137;
    t12  = t12a * 15137 + t13a *  6270;
    t13  = t12a *  6270 - t13a * 15137;
    t14  = t15a * 15137 - t14a *  6270;
    t15  = t15a *  6270 + t14a * 15137;

    out[ 0] =   t0 + t2;
    out[15] = -(t1 + t3);
    t2a     =   t0 - t2;
    t3a     =   t1 - t3;
    out[ 3] = -((t4a + t6a + (1 << 13)) >> 14);
    out[12] =   (t5a + t7a + (1 << 13)) >> 14;
    t6      =   (t4a - t6a + (1 << 13)) >> 14;
    t7      =   (t5a - t7a + (1 << 13)) >> 14;
    out[ 1] = -(t8a + t10a);
    out[14] =   t9a + t11a;
    t10     =   t8a - t10a;
    t11     =   t9a - t11a;
    out[ 2] =   (t12 + t14 + (1 << 13)) >> 14;
    out[13] = -((t13 + t15 + (1 << 13)) >> 14);
    t14a    =   (t12 - t14 + (1 << 13)) >> 14;
    t15a    =   (t13 - t15 + (1 << 13)) >> 14;

    out[ 7] = ((t2a  + t3a)  * -11585 + (1 << 13)) >> 14;
    out[ 8] = ((t2a  - t3a)  *  11585 + (1 << 13)) >> 14;
    out[ 4] = ((t7   + t6)   *  11585 + (1 << 13)) >> 14;
    out[11] = ((t7   - t6)   *  11585 + (1 << 13)) >> 14;
    out[ 6] = ((t11  + t10)  *  11585 + (1 << 13)) >> 14;
    out[ 9] = ((t11  - t10)  *  11585 + (1 << 13)) >> 14;
    out[ 5] = ((t15a + t14a) * -11585 + (1 << 13)) >> 14;
    out[10] = ((t15a - t14a) *  11585 + (1 << 13)) >> 14;
}

itxfm_wrap(16, 6)

// FIXME idct32x32

static av_always_inline void iwht4_1d(const int16_t *in, ptrdiff_t stride,
                                      int16_t *out)
{
    int t0, t1, t2, t3, t4;

    // FIXME this is very ugly and isn't compatible with doing transpose
    // in the detokenize step
    t0 = IN(0) >> 2 * (stride == 1);
    t1 = IN(3) >> 2 * (stride == 1);
    t2 = IN(1) >> 2 * (stride == 1);
    t3 = IN(2) >> 2 * (stride == 1);

    t0 += t2;
    t3 -= t1;
    t4 = (t0 + t3) >> 1;
    t1 = t4 - t1;
    t2 = t4 - t2;
    t0 -= t1;
    t3 += t2;

    out[0] = t0;
    out[1] = t1;
    out[2] = t2;
    out[3] = t3;
}

itxfm_wrapper(iwht, iwht, 4, 0)

#undef IN
#undef itxfm_wrapper
#undef itxfm_wrap

static void vp9dsp_itxfm_init(VP9DSPContext *dsp)
{
#define init_itxfm(tx, sz) \
    dsp->itxfm_add[tx][DCT_DCT]   = idct_idct_##sz##_add_c; \
    dsp->itxfm_add[tx][DCT_ADST]  = iadst_idct_##sz##_add_c; \
    dsp->itxfm_add[tx][ADST_DCT]  = idct_iadst_##sz##_add_c; \
    dsp->itxfm_add[tx][ADST_ADST] = iadst_iadst_##sz##_add_c

#define init_idct(tx, nm) \
    dsp->itxfm_add[tx][DCT_DCT]   = \
    dsp->itxfm_add[tx][ADST_DCT]  = \
    dsp->itxfm_add[tx][DCT_ADST]  = \
    dsp->itxfm_add[tx][ADST_ADST] = nm##_add_c

    init_itxfm(TX_4X4,   4x4);
    init_itxfm(TX_8X8,   8x8);
    init_itxfm(TX_16X16, 16x16);
    //init_idct(TX_32X32,  idct_idct_32x32);
    init_idct(4 /* lossless */, iwht_iwht_4x4);

#undef init_itxfm
#undef init_idct
}

void ff_vp9dsp_init(VP9DSPContext *dsp)
{
    vp9dsp_intrapred_init(dsp);
    vp9dsp_itxfm_init(dsp);
}
