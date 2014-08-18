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
#include <pulse/stream.h>
#include <pulse/xmalloc.h>

#include <stdio.h>

#include "internals.h"

#define CB_FIELDS(cb_type, cb_name) \
    cb_type cb_name; \
    void* cb_name ## _data;

struct pa_stream {
    int count;
    pa_context* context;

    pa_stream_state_t state;

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
    fprintf(stderr, "%s: name = %s, proplist = %s\n", __func__, name, props);
    pa_xfree(props);

    pa_stream* ret = pa_xmalloc0(sizeof(pa_stream));

    ret->count = 1;
    ret->context = c;
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
