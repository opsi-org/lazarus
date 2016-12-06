#!/bin/bash

# set PRG to your executable file name
PRG=winst
LIBPATH=/usr/lib/kylix3

# setup directory
STARTDIR=$(pwd)
APPDIR="${0%/*}"
cd "$APPDIR"
APPDIR=$(pwd)
cd "$STARTDIR"

# setup environment and execute
export LD_LIBRARY_PATH=$LIBPATH:$LD_LIBRARY_PATH

$APPDIR/$PRG $*
