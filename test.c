#include <pulse/def.h>
#include <pulse/proplist.h>

#include <stdio.h>
#include <string.h>

static int fail_count = 0;

#define ASSERT(expr) do { \
    if (!(expr)) { \
        fprintf(stderr, "Failed: %s\n", #expr); \
        fail_count++; \
        return 1; \
    } \
} while(0)

#define EXPECT(expr) do { \
    if (!(expr)) { \
        fprintf(stderr, "Failed: %s\n", #expr); \
        fail_count++; \
    } \
} while(0)

#define SUB(name) EXPECT(name() == 0)

static int test_summary()
{
    printf("Failures: %d\n", fail_count);
    return (fail_count > 0) ? 1 : 0;
}

static int test_smoke()
{
    pa_proplist* pl = NULL;
    ASSERT((pl = pa_proplist_new()) != NULL);

    ASSERT(pa_proplist_sets(pl, "prop1", "aaaa") == PA_OK);
    ASSERT(pa_proplist_sets(pl, "prop2", "bbbb") == PA_OK);

    ASSERT(pa_proplist_size(pl) == 2);
    ASSERT(strcmp(pa_proplist_gets(pl, "prop1"), "aaaa") == 0);
    ASSERT(strcmp(pa_proplist_gets(pl, "prop2"), "bbbb") == 0);
    ASSERT(pa_proplist_gets(pl, "prop3") == NULL);

    pa_proplist_free(pl);

    return 0;
}

static int test_update_set()
{
    pa_proplist* pl = NULL;
    ASSERT((pl = pa_proplist_new()) != NULL);
    ASSERT(pa_proplist_sets(pl, "prop1", "aaaa") == PA_OK);
    ASSERT(pa_proplist_sets(pl, "prop2", "bbbb") == PA_OK);

    pa_proplist* pl2 = NULL;
    ASSERT((pl2 = pa_proplist_new()) != NULL);
    ASSERT(pa_proplist_sets(pl2, "prop2", "cccc") == PA_OK);
    ASSERT(pa_proplist_sets(pl2, "prop3", "dddd") == PA_OK);

    pa_proplist_update(pl, PA_UPDATE_SET, pl2);
    ASSERT(pa_proplist_size(pl) == 2);
    ASSERT(strcmp(pa_proplist_gets(pl, "prop2"), "cccc") == 0);
    ASSERT(strcmp(pa_proplist_gets(pl, "prop3"), "dddd") == 0);

    return 0;
}

static int test_update_replace()
{
    pa_proplist* pl = NULL;
    ASSERT((pl = pa_proplist_new()) != NULL);
    ASSERT(pa_proplist_sets(pl, "prop1", "aaaa") == PA_OK);
    ASSERT(pa_proplist_sets(pl, "prop2", "bbbb") == PA_OK);

    pa_proplist* pl2 = NULL;
    ASSERT((pl2 = pa_proplist_new()) != NULL);
    ASSERT(pa_proplist_sets(pl2, "prop2", "cccc") == PA_OK);
    ASSERT(pa_proplist_sets(pl2, "prop3", "dddd") == PA_OK);

    pa_proplist_update(pl, PA_UPDATE_REPLACE, pl2);
    ASSERT(pa_proplist_size(pl) == 3);
    ASSERT(strcmp(pa_proplist_gets(pl, "prop1"), "aaaa") == 0);
    ASSERT(strcmp(pa_proplist_gets(pl, "prop2"), "cccc") == 0);
    ASSERT(strcmp(pa_proplist_gets(pl, "prop3"), "dddd") == 0);

    return 0;
}

static int test_update_merge()
{
    pa_proplist* pl = NULL;
    ASSERT((pl = pa_proplist_new()) != NULL);
    ASSERT(pa_proplist_sets(pl, "prop1", "aaaa") == PA_OK);
    ASSERT(pa_proplist_sets(pl, "prop2", "bbbb") == PA_OK);

    pa_proplist* pl2 = NULL;
    ASSERT((pl2 = pa_proplist_new()) != NULL);
    ASSERT(pa_proplist_sets(pl2, "prop2", "cccc") == PA_OK);
    ASSERT(pa_proplist_sets(pl2, "prop3", "dddd") == PA_OK);

    pa_proplist_update(pl, PA_UPDATE_MERGE, pl2);
    ASSERT(pa_proplist_size(pl) == 3);
    ASSERT(strcmp(pa_proplist_gets(pl, "prop1"), "aaaa") == 0);
    ASSERT(strcmp(pa_proplist_gets(pl, "prop2"), "bbbb") == 0);
    ASSERT(strcmp(pa_proplist_gets(pl, "prop3"), "dddd") == 0);

    return 0;
}

int main()
{
    SUB(test_smoke);
    SUB(test_update_set);
    SUB(test_update_merge);
    SUB(test_update_replace);
    return test_summary();
}
