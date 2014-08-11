# PaProxy, re-implementation of pulseaudio client API over other backend
# Copyright (C) 2014 Max Kirillov
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA

AC_INIT([pulseproxy-alsa], [0.1])
AM_INIT_AUTOMAKE
AC_PROG_CC
AC_PROG_LIBTOOL
AC_CONFIG_MACRO_DIR([m4])

AC_ARG_ENABLE([makedefaults], [  --enable-makedefaults Add rule for generating defaults.c],
[case "$enableval" in
 yes) makedefaults=true;;
 no) makedefaults=false;;
 *) AC_MSG_ERROR([bad value ${enableval} for --enable-makedefaults])
esac], [makedefaults=false])
AM_CONDITIONAL([ENABLE_MAKEDEFAULTS], [test x$makedefaults = xtrue])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
