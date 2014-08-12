/*
PaProxy, re-implementation of pulseaudio client API over other backend
Copyright (c) 2014 Max Kirillov

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

void pap_report_undefined(const char* func_name);

#define PAP_DEFAULT_NULL() \
    pap_report_undefined(__func__); \
    return 0;

#define PAP_DEFAULT_ZERO() \
    pap_report_undefined(__func__); \
    return 0;

#define PAP_DEFAULT_VOID() \
    pap_report_undefined(__func__);

#define PAP_DEFAULT_CHANNEL_POSITION_MASK() \
    pap_report_undefined(__func__); \
    return 0; /* what should be here? */

#define PAP_DEFAULT_CHANNEL_POSITION() \
    pap_report_undefined(__func__); \
    return PA_CHANNEL_POSITION_INVALID;

#define PAP_DEFAULT_CONTEXT_STATE() \
    pap_report_undefined(__func__); \
    return PA_CONTEXT_FAILED;

#define PAP_DEFAULT_VOLUME() \
    pap_report_undefined(__func__); \
    return PA_VOLUME_MUTED;

#define PAP_DEFAULT_ENCODING() \
    pap_report_undefined(__func__); \
    return PA_ENCODING_INVALID;

#define PAP_DEFAULT_PROP_TYPE() \
    pap_report_undefined(__func__); \
    return PA_PROP_TYPE_INVALID;

#define PAP_DEFAULT_OPERATION_STATE() \
    pap_report_undefined(__func__); \
    return PA_OPERATION_CANCELLED;

#define PAP_DEFAULT_SAMPLE_FORMAT() \
    pap_report_undefined(__func__); \
    return PA_SAMPLE_INVALID;

#define PAP_DEFAULT_STREAM_STATE() \
    pap_report_undefined(__func__); \
    return PA_STREAM_FAILED;
