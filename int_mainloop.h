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

#include <pulse/mainloop-api.h>

#include <poll.h>

typedef struct pap_desc_handler pap_desc_handler;

int pap_mainloop_new_desc_handler(
        pa_mainloop_api* api,
        pap_desc_handler** handler,
        int (*fds_len_cb)(unsigned int* fds_len, void* userdata),
        int (*prepare_fds_cb)(struct pollfd* fds, unsigned int fds_len, void* userdata),
        int (*handle_cb)(int* is_consumed, struct pollfd* fds, unsigned int fds_len, void* userdata),
        void* userdata);

void pap_mainloop_free_desc_handler(pap_desc_handler* handler);
