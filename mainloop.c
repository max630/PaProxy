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
#include <pulse/mainloop.h>
#include <pulse/mainloop-api.h>
#include <pulse/xmalloc.h>

#include <errno.h>
#include <stdio.h>
#include <string.h>

#include "default_macros.h"
#include "int_mainloop.h"
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

struct pap_desc_handler
{
    pa_mainloop* loop;
    int (*fds_len_cb)(unsigned int* fds_len, void* userdata);
    int (*prepare_fds_cb)(struct pollfd* fds, unsigned int fds_len, void* userdata);
    int (*handle_cb)(int* is_consumed, struct pollfd* fds, unsigned int fds_len, void* userdata);
    void* userdata;
};

struct pa_mainloop {
    int last_errno;
    int stopped;
    int retval;
    struct defer_data* defers;
    size_t defers_len;
    size_t defers_size;
    pa_mainloop_api api;

    pap_desc_handler* desc_handlers;
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
    pa_xfree(m->desc_handlers);
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
{
    pa_mainloop* m = e->mainloop;

    m->defers[e->idx].destroy_cb = cb;
}

static void pap_poll_quit(pa_mainloop_api*a, int retval)
{
    pa_mainloop* m = mainloop(a);

    m->stopped = 1;
    m->retval = retval;
}

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

int pa_mainloop_run(pa_mainloop *m, int *retval)
{
    while (!m->stopped) {
        for (size_t i = 0; i < m->defers_len; ++i) {
            if (m->defers[i].cb && m->defers[i].enabled) {
                m->defers[i].cb(&m->api, m->defers[i].event, m->defers[i].userdata);
            }
        }
        
        if (m->desc_handlers) {
            unsigned int fds_len = 0;
            int pa_ret;
            if ((pa_ret = m->desc_handlers->fds_len_cb(&fds_len, m->desc_handlers->userdata)) != 0) {
                fprintf(stderr, "Pa Proxy: %s: fds_len_cb failed: %s\n", __func__, pa_strerror(pa_ret));
                goto desc_handlers_err;
            }

            struct pollfd* fds = pa_xmalloc0(sizeof(struct pollfd*) * fds_len);
            if (!fds) {
                fprintf(stderr, "Pa Proxy: %s: allocate fds failed\n", __func__);
                goto desc_handlers_err;
            }

            if ((pa_ret = m->desc_handlers->prepare_fds_cb(fds, fds_len, m->desc_handlers->userdata)) != 0) {
                fprintf(stderr, "Pa Proxy: %s: prepare_fds_cb failed: %s\n", __func__, pa_strerror(pa_ret));
                pa_xfree(fds);
                goto desc_handlers_err;
            }

            switch (poll(fds, fds_len, -1)) {
            case 0:
                break;
            case -1:
                fprintf(stderr, "Pa Proxy: %s: poll failed: %s\n", __func__, strerror(errno));
                break;
            default:
                {
                    int is_consumed = 0;
                    if ((pa_ret = m->desc_handlers->handle_cb(&is_consumed, fds, fds_len, m->desc_handlers->userdata)) != 0)
                        fprintf(stderr, "Pa Proxy: %s: handle_cb failed: %s\n", __func__, pa_strerror(pa_ret));
                }
                break;
            }
            pa_xfree(fds);
        }
    desc_handlers_err:
        ;
    }

    if (retval)
        *retval = m->retval;
}

int pap_mainloop_new_desc_handler(
        pa_mainloop_api* api,
        pap_desc_handler** handler,
        int (*fds_len_cb)(unsigned int* fds_len, void* userdata),
        int (*prepare_fds_cb)(struct pollfd* fds, unsigned int fds_len, void* userdata),
        int (*handle_cb)(int* is_consumed, struct pollfd* fds, unsigned int fds_len, void* userdata),
        void* userdata)
{
    if (api->io_new != &pap_poll_io_new) {
        fprintf(stderr, "Pa Proxy: %s: descriptor handler requested for unsupported mainloop\n", __func__);
        return -PA_ERR_NOTIMPLEMENTED;
    }
    pa_mainloop* m = mainloop(api);
    if (m->desc_handlers != NULL) {
        fprintf(stderr, "Pa Proxy: %s: only one descriptor handler is impleented so far\n", __func__);
        return -PA_ERR_NOTIMPLEMENTED;
    }
    m->desc_handlers = pa_xmalloc0(sizeof(*m->desc_handlers));
    m->desc_handlers->fds_len_cb = fds_len_cb;
    m->desc_handlers->loop = m;
    m->desc_handlers->prepare_fds_cb = prepare_fds_cb;
    m->desc_handlers->handle_cb = handle_cb;
    m->desc_handlers->userdata = userdata;
    *handler = m->desc_handlers;
    return 0;
}

void pap_mainloop_free_desc_handler(pap_desc_handler* handler)
{
    pa_mainloop* m = handler->loop;
    pa_xfree(m->desc_handlers);
    m->desc_handlers = NULL;
}
