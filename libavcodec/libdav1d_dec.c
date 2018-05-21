#include "libavutil/opt.h"
#include "dav1d.h"
#include "avcodec.h"
#include "internal.h"

typedef struct Libdav1dContext {
    AVClass *class;
    Dav1dContext *c;

    int frame_threads, tile_threads;
    struct {
        uint64_t pos, pts, dts;
        unsigned flags, size;
        int64_t reordered_opaque;
    } *cache;
    int cache_size, cache_fill;
} Libdav1dContext;

static av_cold int libdav1d_dec_init(AVCodecContext *c)
{
    Libdav1dContext *dav1d = c->priv_data;
    Dav1dSettings s;
    int res;

    dav1d_init();
    dav1d_default_settings(&s);
    s.n_tile_threads = dav1d->tile_threads;
    s.n_frame_threads = dav1d->frame_threads;
    if ((res = dav1d_open(&dav1d->c, &s)) < 0)
        return res;
    dav1d->cache = NULL;
    dav1d->cache_size = dav1d->cache_fill = 0;
    c->has_b_frames = 1;

    return 0;
}

static av_cold int libdav1d_dec_close(AVCodecContext *c)
{
    Libdav1dContext *dav1d = c->priv_data;

    dav1d_close(&dav1d->c);
    dav1d->c = NULL;
    av_freep(&dav1d->cache);
    dav1d->cache_fill = dav1d->cache_size = 0;

    return 0;
}

static av_cold void libdav1d_flush(AVCodecContext *c)
{
    Libdav1dContext *dav1d = c->priv_data;
    dav1d_flush(dav1d->c);
    dav1d->cache_fill = 0;
}

static void libdav1d_ref_free_wrapper(void *opaque, uint8_t *data) {
    Dav1dPicture p;
    memset(&p, 0, sizeof(p));
    p.ref = opaque;
    p.data[0] = (void *) 0x1; // this has to be non-NULL
    dav1d_picture_unref(&p);
}

static int libdav1d_dec_decode(AVCodecContext *c, void *frame,
                               int *got_frame_ptr, AVPacket *pkt)
{
    Libdav1dContext *dav1d = c->priv_data;
    Dav1dData *data_ptr, data_mem;
    Dav1dPicture p;
    int res;

    if (pkt) {
        if (dav1d->cache_size == dav1d->cache_fill) {
            dav1d->cache_size += 8;
            dav1d->cache = realloc(dav1d->cache, sizeof(*dav1d->cache) * dav1d->cache_size);
        }
        dav1d->cache[dav1d->cache_fill].pos = pkt->pos;
        dav1d->cache[dav1d->cache_fill].pts = pkt->pts;
        dav1d->cache[dav1d->cache_fill].dts = pkt->dts;
        dav1d->cache[dav1d->cache_fill].size = pkt->size;
        dav1d->cache[dav1d->cache_fill].flags = pkt->flags;
        dav1d->cache[dav1d->cache_fill].reordered_opaque = c->reordered_opaque;
        dav1d->cache_fill++;
        data_ptr = &data_mem;
        dav1d_data_create(data_ptr, pkt->size);
        memcpy(data_ptr->data, pkt->data, pkt->size);
    } else {
        data_ptr = NULL;
    }

    memset(&p, 0, sizeof(p));
    if ((res = dav1d_decode(dav1d->c, data_ptr, &p)) < 0)
        if (res != -EAGAIN)
            return res;

    if (!res) {
        assert(p.data[0] != NULL);
        static const enum AVPixelFormat pix_fmt[][2] = {
            [DAV1D_PIXEL_LAYOUT_I400] = { AV_PIX_FMT_GRAY8,   AV_PIX_FMT_GRAY16 },
            [DAV1D_PIXEL_LAYOUT_I420] = { AV_PIX_FMT_YUV420P, AV_PIX_FMT_YUV420P10 },
            [DAV1D_PIXEL_LAYOUT_I422] = { AV_PIX_FMT_YUV422P, AV_PIX_FMT_YUV422P10 },
            [DAV1D_PIXEL_LAYOUT_I444] = { AV_PIX_FMT_YUV444P, AV_PIX_FMT_YUV444P10 },
        };
        AVFrame *f = av_frame_alloc();
        f->format = c->pix_fmt = pix_fmt[p.p.layout][p.p.bpc == 10];
        f->width = c->width = p.p.w;
        f->height = c->height = p.p.h;
        f->data[0] = p.data[0];
        f->data[1] = p.data[1];
        f->data[2] = p.data[2];
        f->linesize[0] = p.stride[0];
        f->linesize[1] = p.stride[1];
        f->linesize[2] = p.stride[1];
        f->color_primaries = p.p.pri;
        f->color_trc = p.p.trc;
        f->chroma_location = p.p.chr;
        f->colorspace = p.p.mtrx;
        f->color_range = p.p.fullrange ? AVCOL_RANGE_JPEG : AVCOL_RANGE_MPEG;
        AVBufferRef *ref =
            av_buffer_create(NULL, 0,
                             libdav1d_ref_free_wrapper,
                             p.ref, AV_BUFFER_FLAG_READONLY);
        f->buf[0] = av_buffer_ref(ref);
        f->buf[1] = av_buffer_ref(ref);
        f->buf[2] = av_buffer_ref(ref);

        av_buffer_unref(&ref);
        *got_frame_ptr = 1;

        // match timestamps and packet size
        f->reordered_opaque = dav1d->cache[0].reordered_opaque;
        f->pkt_pos = dav1d->cache[0].pos;
        f->pkt_pts = f->best_effort_timestamp = dav1d->cache[0].pts;
        f->pkt_dts = dav1d->cache[0].dts;
        f->pkt_size = dav1d->cache[0].size;
        f->key_frame = !!(dav1d->cache[0].flags & AV_PKT_FLAG_KEY);
        if (--dav1d->cache_fill)
            memcpy(dav1d->cache, &dav1d->cache[1],
                   sizeof(*dav1d->cache) * dav1d->cache_fill);
        f->pict_type = f->key_frame ? AV_PICTURE_TYPE_I : AV_PICTURE_TYPE_P;
        av_frame_move_ref(frame, f);
    } else {
        *got_frame_ptr = 0;
    }

    return pkt->size;
}

#define OFFSET(x) offsetof(Libdav1dContext, x)
#define VD AV_OPT_FLAG_VIDEO_PARAM | AV_OPT_FLAG_DECODING_PARAM
static const AVOption av1_options[] = {
    { "framethreads", "Frame threads", OFFSET(frame_threads),
        AV_OPT_TYPE_INT, { .i64 = 1 }, 1, 256, VD, NULL },
    { "tilethreads", "Tile threads", OFFSET(tile_threads),
        AV_OPT_TYPE_INT, { .i64 = 1 }, 1, 64, VD, NULL },
    { NULL }
};

static const AVClass av1_class = {
    .class_name = "av1",
    .item_name  = av_default_item_name,
    .option     = av1_options,
    .version    = LIBAVUTIL_VERSION_INT,
};

AVCodec ff_libdav1d_decoder = {
    .name           = "libdav1d",
    .long_name      = NULL_IF_CONFIG_SMALL("Dav1d AV1 decoder by VideoLAN"),
    .type           = AVMEDIA_TYPE_VIDEO,
    .id             = AV_CODEC_ID_AV1,
    .priv_data_size = sizeof(Libdav1dContext),
    .init           = libdav1d_dec_init,
    .close          = libdav1d_dec_close,
    .flush          = libdav1d_flush,
    .decode         = libdav1d_dec_decode,
    .capabilities   = AV_CODEC_CAP_DELAY | FF_CODEC_CAP_SETS_PKT_DTS |
                      AV_CODEC_CAP_AUTO_THREADS,
    .priv_class     = &av1_class,
};
