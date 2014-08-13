#include <pulse/proplist.h>

#include <stdio.h>

static int fail_count = 0;

#define ASSERT(expr) do { \
    if (!(expr)) { \
        fprintf(stderr, "Failed: %s\n", #expr); \
        fail_count++; \
        return 1; \
    } \
} while(0)


int main()
{
    pa_proplist* pl = NULL;
    ASSERT((pl = pa_proplist_new()) != NULL);

    ASSERT(pa_proplist_sets(pl, "prop1", "aaaa"));
    ASSERT(pa_proplist_sets(pl, "prop2", "bbbb"));

    pa_proplist_free(pl);
}
