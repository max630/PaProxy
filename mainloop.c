#include <pulse/def.h>
#include <pulse/mainloop.h>
#include <pulse/mainloop-api.h>
#include <pulse/xmalloc.h>

#include "default_macros.h"

#define PAP_POLL_DEFAULT_NULL() \
    pap_report_undefined(__func__); \
    if (m(a)) m(a)->last_errno = PA_ERR_NOTIMPLEMENTED; \
    return NULL;

#define PAP_POLL_DEFAULT_VOID_EV() \
    pap_report_undefined(__func__); \
    return;

#define PAP_POLL_DEFAULT_VOID() \
    pap_report_undefined(__func__); \
    if (m(a)) m(a)->last_errno = PA_ERR_NOTIMPLEMENTED; \
    return;

struct pa_mainloop {
    int last_errno;
    int stopped;
    int retval;
};

pa_mainloop *pa_mainloop_new(void)
{
    return pa_xmalloc0(sizeof(pa_mainloop));
}

void pa_mainloop_free(pa_mainloop* m)
{
    pa_xfree(m);
}

static pa_mainloop* m(pa_mainloop_api*a)
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
{ PAP_POLL_DEFAULT_NULL(); }
static void pap_poll_defer_enable(pa_defer_event* e, int b)
{ PAP_POLL_DEFAULT_VOID_EV(); }
static void pap_poll_defer_free(pa_defer_event* e)
{ PAP_POLL_DEFAULT_VOID_EV(); }
static void pap_poll_defer_set_destroy(pa_defer_event *e, pa_defer_event_destroy_cb_t cb)
{ PAP_POLL_DEFAULT_VOID_EV(); }
static void pap_poll_quit(pa_mainloop_api*a, int retval)
{ PAP_POLL_DEFAULT_VOID(); }

pa_mainloop_api* pa_mainloop_get_api(pa_mainloop*m)
{
    pa_mainloop_api* ret = pa_xmalloc0(sizeof(pa_mainloop_api));
    ret->userdata = m;
    ret->io_new = &pap_poll_io_new;
    ret->io_enable = &pap_poll_io_enable;
    ret->io_free = &pap_poll_io_free;
    ret->io_set_destroy = &pap_poll_io_set_destroy;
    ret->time_new = &pap_poll_time_new;
    ret->time_restart = &pap_poll_time_restart;
    ret->time_free = &pap_poll_time_free;
    ret->time_set_destroy = &pap_poll_time_set_destroy;
    ret->defer_new = &pap_poll_defer_new;
    ret->defer_enable = &pap_poll_defer_enable;
    ret->defer_free = &pap_poll_defer_free;
    ret->defer_set_destroy = &pap_poll_defer_set_destroy;
    ret->quit = &pap_poll_quit;
    return ret;
}
