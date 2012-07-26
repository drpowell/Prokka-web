#!/bin/sh

DIR=$1
TOZIP=$2
ZIPFILE=$3

cd "$DIR"
zip -r "$ZIPFILE" $TOZIP

