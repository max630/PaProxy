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
#include <pulse/mainloop.h>
#include <pulse/mainloop-api.h>
#include <pulse/xmalloc.h>

#include "test.h"

static int test_create()
{
    pa_mainloop* m = pa_mainloop_new();

    pa_mainloop_free(m);
    return 0;
}

static int test_api_defer(pa_mainloop_api* a)
{
    struct data_s {
        pa_defer_event* event;
        int count;
    };
    void cb(pa_mainloop_api*a, pa_defer_event* e, void *data_p)
    {
        struct data_s* data = data_p;
        if (data->count != 0)
            a->quit(a, data->count);
        data->count++;
    }
    struct data_s* data = pa_xmalloc0(sizeof(struct data_s));
    data->event = a->defer_new(a, &cb, data);
}

static int test_defer()
{
    pa_mainloop* m = pa_mainloop_new();
    test_api_defer(pa_mainloop_get_api(m));
    int ret = 0;
    pa_mainloop_run(m, &ret);
    ASSERT_X(ret == 1, "ret: %d", ret);
    pa_mainloop_free(m);
    return 0;
}

int main()
{
    SUB(test_create);
    SUB(test_defer);
    
    return test_summary();
}
