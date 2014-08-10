#include <pulse/proplist.h>
#include <pulse/xmalloc.h>

struct pa_entry
{
    char* key;
};

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
    pa_xfree(p);
}
