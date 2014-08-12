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

#include <pulse/proplist.h>
#include <pulse/xmalloc.h>

#include <assert.h>
#include <string.h>

struct pap_entry
{
    char* key;
    char* value;
};

static void destroy_entry(struct pap_entry* e)
{
    pa_xfree(e->key);
    pa_xfree(e->value);
    e->key = e->value = NULL;
}

struct pa_proplist
{
    struct pap_entry* entries;
    size_t entries_len;
    size_t entries_allocated;
};

pa_proplist* pa_proplist_new(void)
{
    void* mem = pa_xmalloc0(sizeof(pa_proplist));
    return (pa_proplist*)mem;
}

void pa_proplist_free(pa_proplist* p)
{
    for (size_t i = 0; i < p->entries_len; ++i) {
        destroy_entry(&p->entries[i]);
    }
    pa_xfree(p->entries);
    pa_xfree(p);
}

static struct pap_entry* lookup(pa_proplist* p, const char* key)
{
    for (size_t i = 0; i < p->entries_len; ++i) {
        if (strcmp(p->entries[i].key, key) == 0)
            return &p->entries[i];
    }
    return NULL;
}

static void push_back(pa_proplist* p, const char* key, const char* value)
{
    if (p->entries_len >= p->entries_allocated) {
        size_t new_allocated = (p->entries_allocated > 0) ?  10 : (p->entries_allocated * 2);
        p->entries = pa_xrealloc(p->entries, new_allocated * sizeof(struct pap_entry));
        assert(p->entries);
        p->entries_allocated = new_allocated;
    }
    p->entries[p->entries_len].key = pa_xstrdup(key);
    p->entries[p->entries_len].value = pa_xstrdup(value);
    p->entries_len++;
}

int pa_proplist_sets(pa_proplist *p, const char *key, const char *value)
{
    // TODO:
    // 1. verify utf8
    // 2. assert != NULL all (outptu from pa_.. also)
    struct pap_entry* found = NULL;
    if (p->entries != NULL && (found = lookup(p, key)) != NULL) {
        pa_xfree(found->value);
        found->value = pa_xstrdup(value);
    } else {
        push_back(p, key, value);
    }
}
