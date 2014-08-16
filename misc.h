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

#pragma once

char* pap_strcat(char* str1, const char* str2);

#define PAP_GROW(str, str_size) do { \
    size_t new_allocated = ((str_size) > 0) ? ((str_size) * 2) : 10; \
    (str) = pa_xrealloc((str), new_allocated * sizeof(str[0])); \
    assert((str)); \
    (str_size) = new_allocated; \
} while (0)
