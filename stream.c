/*
PaProxy, re-implementation of pulseaudio client API over other backend
Copyright (C) 2014 Max Kirillov

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#include <pulse/def.h>
#include <pulse/error.h>
#include <pulse/stream.h>
#include <pulse/xmalloc.h>

#include <alsa/asoundlib.h>

#include <stdio.h>

#include "int_mainloop.h"
#include "internals.h"
#include "misc.h"

#define CB_FIELDS(cb_type, cb_name) \
    cb_type cb_name; \
    void* cb_name ## _data;

struct pa_stream {
    int count;
    pa_context* context;

    const pa_sample_spec* pa_ss;
    snd_pcm_stream_t pcm_dir;

    pa_stream_state_t state;

    snd_pcm_t* pcm;

    pap_desc_handler* desc_handler;

    CB_FIELDS(pa_stream_notify_cb_t, state_cb)
    CB_FIELDS(pa_stream_request_cb_t, read_cb)
    CB_FIELDS(pa_stream_request_cb_t, write_cb)
    CB_FIELDS(pa_stream_notify_cb_t, suspended_cb)
    CB_FIELDS(pa_stream_notify_cb_t, moved_cb)
    CB_FIELDS(pa_stream_notify_cb_t, underflow_cb)
    CB_FIELDS(pa_stream_notify_cb_t, overflow_cb)
    CB_FIELDS(pa_stream_notify_cb_t, started_cb)
    CB_FIELDS(pa_stream_event_cb_t, event_cb)
    CB_FIELDS(pa_stream_notify_cb_t, buffer_attr_cb)
};

pa_stream* pa_stream_new_with_proplist(
        pa_context *c                     /**< The context to create this stream in */,
        const char *name                  /**< A name for this stream */,
        const pa_sample_spec *ss          /**< The desired sample format */,
        const pa_channel_map *map         /**< The desired channel map, or NULL for default */,
        pa_proplist *p                    /**< The initial property list */)
{
    char* props = pa_proplist_to_string(p);
    fprintf(stderr, "%s: name = %s\n", __func__, name);
    if (ss)
        fprintf(stderr, " > sample = {%s, %u, %u}\n", pap_format_name(ss->format), (unsigned)ss->rate, (unsigned)ss->channels);
    if (map)
        fprintf(stderr, " > channels = %u\n", (unsigned)map->channels);
    fprintf(stderr, " > proplist = %s\n", props);
    pa_xfree(props);

    pa_stream* ret = pa_xmalloc0(sizeof(pa_stream));

    ret->count = 1;
    ret->context = c;
    ret->pa_ss = ss;
    pa_context_ref(ret->context);

    return ret;
}

void pa_stream_unref(pa_stream* s)
{
    --s->count;
    if (s->count == 0) {
        pa_context_unref(s->context);
        pa_xfree(s);
    }
}

#define CB_DEFINE(cb_type, stem) \
void pa_stream_set_ ## stem ## _callback(pa_stream *p, cb_type cb, void *userdata) \
{ \
    p->stem ## _cb = cb; \
    p->stem ## _cb_data = userdata; \
}

CB_DEFINE(pa_stream_notify_cb_t, state)
CB_DEFINE(pa_stream_request_cb_t, read)
CB_DEFINE(pa_stream_request_cb_t, write)
CB_DEFINE(pa_stream_notify_cb_t, suspended)
CB_DEFINE(pa_stream_notify_cb_t, moved)
CB_DEFINE(pa_stream_notify_cb_t, underflow)
CB_DEFINE(pa_stream_notify_cb_t, overflow)
CB_DEFINE(pa_stream_notify_cb_t, started)
CB_DEFINE(pa_stream_event_cb_t, event)
CB_DEFINE(pa_stream_notify_cb_t, buffer_attr)

static int fds_len_cb(unsigned int* fds_len, void* userdata)
{
    pa_stream* s = userdata;

}

static int prepare_fds_cb(struct pollfd* fds, unsigned int fds_len, void* userdata)
{
    pa_stream* s = userdata;
}

static int handle_cb(int* is_consumed, struct pollfd* fds, unsigned int fds_len, void* userdata)
{
    pa_stream* s = userdata;
}

int pa_stream_connect_playback(
        pa_stream *s                  /**< The stream to connect to a sink */,
        const char *dev               /**< Name of the sink to connect to, or NULL for default */ ,
        const pa_buffer_attr *attr    /**< Buffering attributes, or NULL for default */,
        pa_stream_flags_t flags       /**< Additional flags, or 0 for default */,
        const pa_cvolume *volume      /**< Initial volume, or NULL for default */,
        pa_stream *sync_stream        /**< Synchronize this stream with the specified one, or NULL for a standalone stream */)
{
    fprintf(stderr, "%s: dev = %s, flags = %x\n", __func__, dev, (int)flags);
    pending_action_request(s->context, STREAM_START_CONNECTING, s);

    return 0;
}

void stream_set_state(pa_stream* s, pa_stream_state_t state)
{
    s->state = state;
    if (s->state_cb)
        s->state_cb(s, s->state_cb_data);
}

pa_stream_state_t pa_stream_get_state(pa_stream *p)
{
    return p->state;
}

pa_context* pa_stream_get_context(pa_stream *p)
{
    return p->context;
}

void pap_stream_do_connect(pa_stream* s)
{
    stream_set_state(s, PA_STREAM_CREATING);

    // TODO: setup write handler

    if (!s->desc_handler) {
        int pap_ret;
        if ((pap_ret = pap_mainloop_new_desc_handler(context_get_loop(s->context),
                                                     &s->desc_handler,
                                                     &fds_len_cb,
                                                     &prepare_fds_cb,
                                                     &handle_cb,
                                                     s)) != PA_OK) {
            fprintf(stderr, "%s: pap_mainloop_new_desc_handler failed: %s\n", __func__, pa_strerror(pap_ret));
            stream_set_state(s, PA_STREAM_FAILED);
            return;
        }
    }

    int pcm_ret;
    if ((pcm_ret = snd_pcm_open(&s->pcm, NULL, s->pcm_dir, SND_PCM_NONBLOCK)) != 0) {
        fprintf(stderr, "%s: snd_pcm_open failed: %s\n", __func__, snd_strerror(pcm_ret));
        stream_set_state(s, PA_STREAM_FAILED);
        return;
    }

    if (snd_pcm_state(s->pcm) != SND_PCM_STATE_OPEN) {
        fprintf(stderr, "%s: open unexpected PCM state: %d, need OPEN\n", __func__, (int)snd_pcm_state(s->pcm));
        stream_set_state(s, PA_STREAM_FAILED);
        return;
    }

    if ((pcm_ret = snd_pcm_set_params(s->pcm, pcm_format_from_pa_format(s->pa_ss->format),
                                      SND_PCM_ACCESS_RW_INTERLEAVED, // TODO: pick proper value from pa parameters
                                      s->pa_ss->channels,
                                      s->pa_ss->rate,
                                      1, 500000)) != 0) // TODO: pick proper latency from pa parameters
    {
        fprintf(stderr, "%s: snd_pcm_set_params failed: %s\n", __func__, snd_strerror(pcm_ret));
        stream_set_state(s, PA_STREAM_FAILED);
        return;
    }

    if (snd_pcm_state(s->pcm) != SND_PCM_STATE_RUNNING) {
        fprintf(stderr, "%s: open unexpected PCM state: %d, need OPEN\n", __func__, (int)snd_pcm_state(s->pcm));
        stream_set_state(s, PA_STREAM_FAILED);
        return;
    }

    stream_set_state(s, PA_STREAM_READY);
}
