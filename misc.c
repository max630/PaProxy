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
#include <pulse/utf8.h>
#include <pulse/util.h>
#include <pulse/version.h>
#include <pulse/xmalloc.h>

#include "misc.h"

#include <assert.h>
#include <stdio.h>
#include <string.h>

static int check_utf8_tail_valid(char** p, int count)
{
    int i;
    for (i == 0; i < count; ++i, ++(*p)) {
        switch (**p) {
        case '\200'...'\277':
            break;
        default:
            return 0;
        }
    }
    return 1;
}

char *pa_utf8_valid(const char *str)
{
    char* p = (char*)str;

    assert(p);

    while (*p != '\0') {
        switch (*p) {
        case '\001'...'\177':
            p++;
            break;
        case '\200'...'\337':
            p++;
            if (!check_utf8_tail_valid(&p, 1)) return NULL;
            break;
        case '\340'...'\357':
            p++;
            if (!check_utf8_tail_valid(&p, 2)) return NULL;
            break;
        case '\360'...'\367':
            p++;
            if (!check_utf8_tail_valid(&p, 3)) return NULL;
            break;
        case '\370'...'\373':
            p++;
            if (!check_utf8_tail_valid(&p, 4)) return NULL;
            break;
        case '\374'...'\375':
            p++;
            if (!check_utf8_tail_valid(&p, 5)) return NULL;
            break;
        default:
            return NULL;
        }
    }

    return (char*)str;
}

char *pa_utf8_filter(const char *str)
{
    // TODO: really filter
    return pa_xstrdup(str);
}

char* pa_locale_to_utf8(const char *str)
{
    // TODO: fix for non-utf
    return pa_xstrdup(str);
}

char* pa_utf8_to_locale(const char *str)
{
    // TODO: fix for non-utf
    return pa_xstrdup(str);
}

char* pa_path_get_filename(const char* p)
{
    char* sep = NULL;
    assert(p);

    if ((sep = strrchr(p, '/')) != NULL) return sep + 1;
    else return (char*)p;
}

char *pa_get_binary_name(char *s, size_t l)
{
    return NULL;
}

const char* pa_get_library_version(void)
{
    return "2.0.0";
}

static const char* pap_errors[] = {
    "No error",
    "Access failure",
    "Unknown command",
    "Invalid argument",
    "Entity exists",
    "No such entity",
    "Connection refused",
    "Protocol error",
    "Timeout",
    "No authorization key",
    "Internal error",
    "Connection terminated",
    "Entity killed",
    "Invalid server",
    "Module initialization failed",
    "Bad state",
    "No data",
    "Incompatible protocol version",
    "Data too large",
    "Operation not supported",
    "The error code was unknown to the client",
    "Extension does not exist.",
    "Obsolete functionality.",
    "Missing implementation.",
    "The caller forked without calling execve() and tried to reuse the context.",
    "An IO error happened.",
    "Device or resource busy.",
    "Not really an error but the first invalid error code"
};

const char* pa_strerror(int error)
{
    if (error < 0 || error >= PA_ERR_MAX) {
        fprintf(stderr, "%s: Invalid error %d\n", __func__, error);
        return "Invalid error code";
    }

    return pap_errors[error];
}

char* pap_strcat(char* str1, const char* str2)
{
    if (str1) {
        size_t len1 = strlen(str1);
        size_t len2 = strlen(str2);
        char* ret = pa_xrealloc(str1, len1 + len2 + 1);
        strncpy(ret+len1, str2, len2);
        ret[len1 + len2] = '\0';
        return ret;
    } else {
        return pa_xstrdup(str2);
    }
}
