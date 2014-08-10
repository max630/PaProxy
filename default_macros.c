#include "default_macros.h"

#include <stdio.h>

/* TODO: report each function only once
 */

void pap_report_undefined(const char* func_name)
{
    fprintf(stderr, "Pa Proxy: unimplemented function '%s'\n", func_name);
}

