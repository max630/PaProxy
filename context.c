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

struct pa_context {
    unsigned use_count;
    int last_errno;
    pa_context_notify_cb_t state_cb;
    void* state_cb_data;
};

pa_context *pa_context_new_with_proplist(pa_mainloop_api *mainloop, const char *name, pa_proplist *proplist)
{
    pa_context* ret = pa_xmalloc0(sizeof(pa_context));
    ret->use_count = 1;
}

void pa_context_unref(pa_context *c)
{
    --c->use_count;
    if (c->use_count == 0) {
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

int pa_context_connect(pa_context *c, const char *server, pa_context_flags_t flags, const pa_spawn_api *api)
{
    fprintf(stderr, "%s: server = %s\n", __func__, server);
    return 0;
}
