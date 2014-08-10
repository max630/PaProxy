#include <introspect.h>
#include <format.h>

#include "default_macros.h"

/* Special cases, not worthing fixing the generator
 */

/* there were no "cb" name - apparently it is an error */
pa_operation * pa_context_add_autoload(pa_context * c,
                                       const char * name,
                                       pa_autoload_type_t type,
                                       const char * module,
                                       const char * argument,
                                       pa_context_index_cb_t cb,
                                       void * userdata)
{
    PAP_DEFAULT_NULL();
}

void pa_format_info_free2(pa_format_info *f, void *userdata) {
    pa_format_info_free(f);
}
