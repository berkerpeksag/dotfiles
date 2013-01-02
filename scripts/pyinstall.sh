#!/bin/bash

PYVERSION=$1
if [ -z $PYVERSION]; then
    PYVERSION=3.3.0
fi

PYDIR="Python-$PYVERSION"
PYFILE="$PYDIR.tgz"
PYDOWNLOADURL="http://python.org/ftp/python/$PYVERSION/$PYFILE"
WGET="wget -c"
WGETCOMMAND=$WGET" "$PYDOWNLOADURL

if [ `$WGETCOMMAND` ]; then
    echo "[ERROR] $PYDIR download error."
fi

tar -xf $PYFILE
cd $PYDIR
./configure
make
sudo make install
