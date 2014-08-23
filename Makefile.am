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
check_PROGRAMS = t_proplist t_mainloop

AM_CFLAGS = -Wimplicit -Werror -g --std=gnu99
AM_CPPFLAGS = -D_GNU_SOURCE

libpulse_impl_sources = misc.c channels.c proplist.c sample.c xmalloc.c \
		        mainloop.c context.c stream.c \
			defaults-specials.c

libpulse_la_SOURCES = $(libpulse_impl_sources) defaults.c default_macros.c
libpulse_LDADD = $(AM_LDADD) -lasound
libpulse_la_LDFLAGS = $(AM_LDFLAGS) -Wl,-version-script=map-libpulse

t_proplist_SOURCES = t_proplist.c
t_proplist_LDADD = -lpulse

t_mainloop_SOURCES = t_mainloop.c
t_mainloop_LDADD = -lpulse

ACLOCAL_AMFLAGS = -I m4

if ENABLE_MAKEDEFAULTS
defaults.c: MakeDefault.hs $(libpulse_impl_sources)
	./MakeDefault.hs

maintainer-clean-local:
	rm -f map-libpulse defaults.c
else !ENABLE_MAKEDEFAULTS
endif

test: $(check_PROGRAMS)
	fail=no; for p in $(check_PROGRAMS); do \
	 ./$$p || fail=yes; \
	done ; test $$fail = no
