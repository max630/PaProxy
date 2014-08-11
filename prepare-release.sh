#!/bin/sh

aclocal
libtoolize -c
automake -a -c
autoconf
./MakeDefault.hs

if [ $1 = ci ]; then
    git add Makefile.in config.guess config.sub configure defaults.c \
            depcomp install-sh libtool ltmain.sh missing
fi
