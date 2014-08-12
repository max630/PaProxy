# PaProxy, re-implementation of pulseaudio client API over other backend
# Copyright (C) 2014 Max Kirillov
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
# 
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

lib_LTLIBRARIES = libpulse.la

AM_CFLAGS = -Wimplicit -Werror -g
AM_CPPFLAGS = -D_GNU_SOURCE

libpulse_la_SOURCES = misc.c channels.c proplist.c sample.c xmalloc.c defaults.c defaults-specials.c default_macros.c
libpulse_la_LDFLAGS = $(AM_LDFLAGS) -Wl,-version-script=map-libpulse

ACLOCAL_AMFLAGS = -I m4

if ENABLE_MAKEDEFAULTS
defaults.c: MakeDefault.hs $(filter-out defaults.c,$(wildcard *.c))
	./MakeDefault.hs

maintainer-clean-local:
	rm -f map-libpulse defaults.c
else !ENABLE_MAKEDEFAULTS
endif
