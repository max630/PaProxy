lib_LTLIBRARIES = libpulse.la

AM_CFLAGS = -Wimplicit -Werror

libpulse_la_SOURCES = xmalloc.c defaults.c defaults-specials.c default_macros.c
libpulse_la_LDFLAGS = $(AM_LDFLAGS) -Wl,-version-script=map-libpulse

ACLOCAL_AMFLAGS = -I m4

AM_CPPFLAGS = -I/usr/include/pulse
