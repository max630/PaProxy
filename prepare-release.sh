#!/bin/sh

aclocal
libtoolize -c
automake -a -c
autoconf
./MakeDefault.hs

if [ "$1" = ci ]; then
    git add Makefile.in aclocal.m4 config.guess config.sub configure defaults.c \
            depcomp install-sh ltmain.sh map-libpulse missing
fi
