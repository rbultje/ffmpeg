/*
 * Copyright (c) 2012 Michael Niedermayer <michaelni@gmx.at>
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

#include "libavutil/x86/asm.h"
#include "libavutil/cpu.h"
#include "libswresample/swresample_internal.h"

#define COMMON_CORE_DBL_SSE2 \
    x86_reg len= -8*c->filter_length;\
__asm__ volatile(\
    "xorpd     %%xmm0, %%xmm0     \n\t"\
    "1:                           \n\t"\
    "movupd  (%1, %0), %%xmm1     \n\t"\
    "mulpd   (%2, %0), %%xmm1     \n\t"\
    "addpd     %%xmm1, %%xmm0     \n\t"\
    "add       $16, %0            \n\t"\
    " js 1b                       \n\t"\
    "movhlps   %%xmm0, %%xmm1     \n\t"\
    "addpd     %%xmm1, %%xmm0     \n\t"\
    "movsd     %%xmm0, (%3)       \n\t"\
    : "+r" (len)\
    : "r" (((uint8_t*)(src+sample_index))-len),\
      "r" (((uint8_t*)filter)-len),\
      "r" (dst+dst_index)\
    XMM_CLOBBERS_ONLY("%xmm0", "%xmm1")\
);

#define LINEAR_CORE_DBL_SSE2 \
    x86_reg len= -8*c->filter_length;\
__asm__ volatile(\
    "xorpd      %%xmm0, %%xmm0    \n\t"\
    "xorpd      %%xmm2, %%xmm2    \n\t"\
    "1:                           \n\t"\
    "movupd   (%3, %0), %%xmm1    \n\t"\
    "movapd     %%xmm1, %%xmm3    \n\t"\
    "mulpd    (%4, %0), %%xmm1    \n\t"\
    "mulpd    (%5, %0), %%xmm3    \n\t"\
    "addpd      %%xmm1, %%xmm0    \n\t"\
    "addpd      %%xmm3, %%xmm2    \n\t"\
    "add           $16, %0        \n\t"\
    " js 1b                       \n\t"\
    "movhlps    %%xmm0, %%xmm1    \n\t"\
    "movhlps    %%xmm2, %%xmm3    \n\t"\
    "addpd      %%xmm1, %%xmm0    \n\t"\
    "addpd      %%xmm3, %%xmm2    \n\t"\
    "movsd      %%xmm0, %1        \n\t"\
    "movsd      %%xmm2, %2        \n\t"\
    : "+r" (len),\
      "=m" (val),\
      "=m" (v2)\
    : "r" (((uint8_t*)(src+sample_index))-len),\
      "r" (((uint8_t*)filter)-len),\
      "r" (((uint8_t*)(filter+c->filter_alloc))-len)\
    XMM_CLOBBERS_ONLY("%xmm0", "%xmm1", "%xmm2", "%xmm3")\
);
