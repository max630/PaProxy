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

#include <pulse/context.h>
#include <pulse/def.h>
#include <pulse/xmalloc.h>

#include <stdio.h>

#include "internals.h"

struct pending_action_data {
    pa_context* context;
    enum pending_action_type action;
    pa_stream* stream;
};

struct pa_context {
    unsigned use_count;
    pa_context_state_t state;
    int last_errno;
    pa_mainloop_api* loop;
    pa_defer_event* pending_action_event;
    struct pending_action_data pending_action_data;
    pa_context_notify_cb_t state_cb;
    void* state_cb_data;
};

static void pending_action_cb(pa_mainloop_api*a, pa_defer_event* e, void *userdata);

pa_context *pa_context_new_with_proplist(pa_mainloop_api *mainloop, const char *name, pa_proplist *proplist)
{
    char* prop_string = pa_proplist_to_string(proplist);
    fprintf(stderr, "%s: name = %s, props = %s\n", __func__, name, prop_string);
    pa_xfree(prop_string);
    pa_context* ret = pa_xmalloc0(sizeof(pa_context));
    ret->use_count = 1;
    ret->state = PA_CONTEXT_UNCONNECTED;
    ret->loop = mainloop;
    
    ret->pending_action_data.context = ret;
    ret->pending_action_data.action = NO_ACTION;
    ret->pending_action_event = mainloop->defer_new(mainloop, &pending_action_cb, &ret->pending_action_data);
    mainloop->defer_enable(ret->pending_action_event, 0);

    return ret;
}

pa_context* pa_context_ref(pa_context *c)
{
    ++c->use_count;
    return c;
}

void pa_context_unref(pa_context *c)
{
    --c->use_count;
    if (c->use_count == 0) {
        c->loop->defer_free(c->pending_action_event);
        pa_xfree(c);
    }
}

int pa_context_errno(pa_context *c)
{
    return c->last_errno;
}

void pa_context_set_state_callback(pa_context *c, pa_context_notify_cb_t cb, void *userdata)
{
    c->state_cb = cb;
    c->state_cb_data = cb;
}

static void set_state(pa_context* c, pa_context_state_t state)
{
    c->state = state;
    if (c->state_cb)
        c->state_cb(c, c->state_cb_data);
}

pa_context_state_t pa_context_get_state(pa_context *c)
{
    return c->state;
}

void pending_action_cb(pa_mainloop_api*a, pa_defer_event* e, void *userdata)
{
    struct pending_action_data* data = (struct pending_action_data*)userdata;

    pa_context* c = data->context;
    switch (data->action) {
    case NO_ACTION:
        fprintf(stderr, "%s: no action is requested but event is not disabled\n", __func__);
        c->loop->defer_enable(e, 0);
        break;
    case START_CONNECTING:
        data->action = FINISH_CONNECTING;
        set_state(c, PA_CONTEXT_CONNECTING);
        break;
    case FINISH_CONNECTING:
        data->action = NO_ACTION;
        c->loop->defer_enable(e, 0);
        set_state(c, PA_CONTEXT_READY);
        break;
    case STREAM_START_CONNECTING:
        data->action = STREAM_FINISH_CONNECTING;
        stream_set_state(data->stream, PA_STREAM_CREATING);
        break;
    case STREAM_FINISH_CONNECTING:
        data->action = NO_ACTION;
        c->loop->defer_enable(e, 0);
        stream_set_state(data->stream, PA_STREAM_READY);
        break;
    }
}

int pa_context_connect(pa_context *c, const char *server, pa_context_flags_t flags, const pa_spawn_api *api)
{
    fprintf(stderr, "%s: server = %s\n", __func__, server);
    pending_action_request(c, START_CONNECTING, NULL);
    return 0;
}

void pending_action_request(pa_context* c, enum pending_action_type action_type, pa_stream* s)
{
    if (c->pending_action_data.action != NO_ACTION) {
        fprintf(stderr, "%s: pending action clash!!!\n", __func__);
        return;
    }
    c->pending_action_data.action = action_type;
    c->pending_action_data.stream = s;
    c->loop->defer_enable(c->pending_action_event, 1);
}
