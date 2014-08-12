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
#include <search.h>
#include <string.h>

#include "default_macros.h"

struct pap_tree_entry
{
    char* key;
    char* value;
};

static int compare_entry(const void* e1, const void* e2)
{
    assert(e1);
    assert(e2);
    return strcmp(((const struct pap_tree_entry*)e1)->key, ((const struct pap_tree_entry*)e2)->key);
};

static void destroy_entry(void* e)
{
    pa_xfree(((struct pap_tree_entry*)e)->key);
    pa_xfree(((struct pap_tree_entry*)e)->value);
    pa_xfree(e);
}

struct pa_proplist
{
    void* tree;
};

pa_proplist* pa_proplist_new(void)
{
    void* mem = pa_xmalloc0(sizeof(pa_proplist));
    return (pa_proplist*)mem;
}

void pa_proplist_free(pa_proplist* p)
{
    tdestroy(p->tree, &destroy_entry);
    pa_xfree(p);
}

int pa_proplist_sets(pa_proplist *p, const char *key, const char *value)
{
    // TODO:
    // 1. verify utf8
    // 2. assert != NULL all (outptu from pa_.. also)
    struct pap_tree_entry sample = { .key = (char*)key, .value = NULL };
    struct pap_tree_entry* existing = tfind(&sample, &p->tree, &compare_entry);
    if (existing != NULL) {
        pa_xfree(existing->value);
        existing->value = pa_xstrdup(value);
    } else {
        struct pap_tree_entry* new_entry = pa_xmalloc(sizeof(struct pap_tree_entry));
        new_entry->key = pa_xstrdup(key);
        new_entry->value = pa_xstrdup(value);
        tsearch(new_entry, &p->tree, &compare_entry);
    }
    return 0;
}

void pa_proplist_update(pa_proplist *p, pa_update_mode_t mode, const pa_proplist *other)
{
    int ifReplace = 0;
    switch (mode) {
    case PA_UPDATE_SET:
        pap_report_undefined("pa_proplist_update(PA_UPDATE_SET)");
        pa_proplist_update(p, PA_UPDATE_REPLACE, other);
        break;
    case PA_UPDATE_REPLACE:
        ifReplace = 1;
        break;
    case PA_UPDATE_MERGE:
        ifReplace = 0;
        break;
    }
}
