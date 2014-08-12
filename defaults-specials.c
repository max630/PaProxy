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

#include <pulse/introspect.h>
#include <pulse/format.h>

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
