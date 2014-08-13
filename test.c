#include <pulse/def.h>
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

static int test_summary()
{
    printf("Failures: %d\n", fail_count);
    return (fail_count > 0) ? 1 : 0;
}

int main()
{
    pa_proplist* pl = NULL;
    ASSERT((pl = pa_proplist_new()) != NULL);

    ASSERT(pa_proplist_sets(pl, "prop1", "aaaa") == PA_OK);
    ASSERT(pa_proplist_sets(pl, "prop2", "bbbb") == PA_OK);

    pa_proplist_free(pl);

    return test_summary();
}
