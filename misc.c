/*
PaProxy, re-implementation of pulseaudio client API over other backend
Copyright (C) 2014 Max Kirillov

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA
*/

#include <pulse/utf8.h>
#include <pulse/util.h>
#include <pulse/version.h>
#include <pulse/xmalloc.h>

#include <assert.h>
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
