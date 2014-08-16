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

struct pa_stream {
    int count;
    pa_context* context;
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
