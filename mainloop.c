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

#include <string.h>

#include "default_macros.h"
#include "misc.h"

#define PAP_POLL_DEFAULT_NULL() \
    pap_report_undefined(__func__); \
    if (mainloop(a)) mainloop(a)->last_errno = PA_ERR_NOTIMPLEMENTED; \
    return NULL;

#define PAP_POLL_DEFAULT_VOID_EV() \
    pap_report_undefined(__func__); \
    return;

#define PAP_POLL_DEFAULT_VOID() \
    pap_report_undefined(__func__); \
    if (mainloop(a)) mainloop(a)->last_errno = PA_ERR_NOTIMPLEMENTED; \
    return;

struct pa_defer_event {
    pa_mainloop* mainloop;
    size_t idx;
};

struct defer_data {
    int enabled;
    pa_defer_event_cb_t cb;
    void* userdata;
    pa_defer_event_destroy_cb_t destroy_cb;
    pa_defer_event* event;
};

struct pa_mainloop {
    int last_errno;
    int stopped;
    int retval;
    struct defer_data* defers;
    size_t defers_len;
    size_t defers_size;
    pa_mainloop_api api;
};

static void destroy_defer(pa_mainloop* m, size_t i)
{
    assert(i < m->defers_len);
    if (m->defers[i].destroy_cb)
        m->defers[i].destroy_cb(pa_mainloop_get_api(m), m->defers[i].event, m->defers[i].userdata);
    if (m->defers[i].event)
        pa_xfree(m->defers[i].event);
    memset(&m->defers[i], sizeof(m->defers[i]), 0);
    if (i == m->defers_len - 1)
        m->defers_len--;
}

void pa_mainloop_free(pa_mainloop* m)
{
    for (size_t i = 0; i < m->defers_len; ++i) {
        destroy_defer(m, i);
    }
    pa_xfree(m->defers);
    pa_xfree(m);
}

static pa_mainloop* mainloop(pa_mainloop_api*a)
{
    if(a) return (pa_mainloop*) a->userdata;
}

static pa_io_event* pap_poll_io_new(pa_mainloop_api*a, int fd, pa_io_event_flags_t events, pa_io_event_cb_t cb, void *userdata)
{ PAP_POLL_DEFAULT_NULL(); }
static void pap_poll_io_enable(pa_io_event* e, pa_io_event_flags_t events)
{ PAP_POLL_DEFAULT_VOID_EV(); }
static void pap_poll_io_free(pa_io_event* e)
{ PAP_POLL_DEFAULT_VOID_EV(); }
static void pap_poll_io_set_destroy(pa_io_event *e, pa_io_event_destroy_cb_t cb)
{ PAP_POLL_DEFAULT_VOID_EV(); }
static pa_time_event* pap_poll_time_new(pa_mainloop_api*a, const struct timeval *tv, pa_time_event_cb_t cb, void *userdata)
{ PAP_POLL_DEFAULT_NULL(); }
static void pap_poll_time_restart(pa_time_event* e, const struct timeval *tv)
{ PAP_POLL_DEFAULT_VOID_EV(); }
static void pap_poll_time_free(pa_time_event* e)
{ PAP_POLL_DEFAULT_VOID_EV(); }
static void pap_poll_time_set_destroy(pa_time_event *e, pa_time_event_destroy_cb_t cb)
{ PAP_POLL_DEFAULT_VOID_EV(); }

static pa_defer_event* pap_poll_defer_new(pa_mainloop_api*a, pa_defer_event_cb_t cb, void *userdata)
{
    pa_mainloop* m = mainloop(a);
    size_t idx = (size_t)(-1);
    for (size_t i = 0; i < m->defers_len; ++i) {
        if (m->defers[i].cb == 0) {
            idx = i;
            break;
        }
    }
    if (idx == (size_t)(-1)) {
        // no free slot
        if (m->defers_len >= m->defers_size) PAP_GROW(m->defers, m->defers_size);
        idx = m->defers_len;
        m->defers_len++;
    }
    m->defers[idx].enabled = 1;
    m->defers[idx].cb = cb;
    m->defers[idx].userdata = userdata;
    m->defers[idx].destroy_cb = NULL;
    m->defers[idx].event = pa_xmalloc0(sizeof(*m->defers[idx].event));
    m->defers[idx].event->mainloop = m;
    m->defers[idx].event->idx = idx;

    return m->defers[idx].event;
}

static void pap_poll_defer_enable(pa_defer_event* e, int b)
{
    pa_mainloop* m = e->mainloop;

    m->defers[e->idx].enabled = b;
}


static void pap_poll_defer_free(pa_defer_event* e)
{
    pa_mainloop* m = e->mainloop;

    destroy_defer(m, e->idx);
}

static void pap_poll_defer_set_destroy(pa_defer_event *e, pa_defer_event_destroy_cb_t cb)
{ PAP_POLL_DEFAULT_VOID_EV(); }
static void pap_poll_quit(pa_mainloop_api*a, int retval)
{ PAP_POLL_DEFAULT_VOID(); }

pa_mainloop *pa_mainloop_new(void)
{
    pa_mainloop* ret = pa_xmalloc0(sizeof(pa_mainloop));
    ret->api.userdata = ret;
    ret->api.io_new = &pap_poll_io_new;
    ret->api.io_enable = &pap_poll_io_enable;
    ret->api.io_free = &pap_poll_io_free;
    ret->api.io_set_destroy = &pap_poll_io_set_destroy;
    ret->api.time_new = &pap_poll_time_new;
    ret->api.time_restart = &pap_poll_time_restart;
    ret->api.time_free = &pap_poll_time_free;
    ret->api.time_set_destroy = &pap_poll_time_set_destroy;
    ret->api.defer_new = &pap_poll_defer_new;
    ret->api.defer_enable = &pap_poll_defer_enable;
    ret->api.defer_free = &pap_poll_defer_free;
    ret->api.defer_set_destroy = &pap_poll_defer_set_destroy;
    ret->api.quit = &pap_poll_quit;
    return ret;
}

pa_mainloop_api* pa_mainloop_get_api(pa_mainloop*m)
{
    return &m->api;
}
