#include <pulse/utf8.h>
#include <pulse/util.h>

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

char* pa_path_get_filename(const char* p)
{
    char* sep = NULL;
    assert(p);

    if ((sep = strrchr(p, '/')) != NULL) return sep + 1;
    else return (char*)p;
}
