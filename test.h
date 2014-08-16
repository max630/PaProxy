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

#include <stdio.h>

extern int fail_count __attribute__ ((weak));
int fail_count = 0;

#define ASSERT(expr) do { \
    if (!(expr)) { \
        fprintf(stderr, "%s:%d: Failed: %s\n", __FILE__, __LINE__, #expr); \
        fail_count++; \
        return 1; \
    } \
} while(0)

#define ASSERT_X(expr, format, ...) do { \
    if (!(expr)) { \
        fprintf(stderr, "%s:%d: Failed: %s\n", __FILE__, __LINE__, #expr); \
        fprintf(stderr, " > " format "\n", ## __VA_ARGS__); \
        fail_count++; \
        return 1; \
    } \
} while(0)

#define EXPECT(expr) do { \
    if (!(expr)) { \
        fprintf(stderr, "%s:%d: Failed: %s\n", __FILE__, __LINE__, #expr); \
        fail_count++; \
    } \
} while(0)

#define SUB(name) EXPECT(name() == 0)

#define test_summary() ( \
    printf("Failures: %d\n", fail_count), \
    ((fail_count > 0) ? 1 : 0) \
)
