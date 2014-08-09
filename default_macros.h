#pragma once

#include <channelmap.h>

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
