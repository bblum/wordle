#!/bin/bash

WORD=$1
TMPFILE=`mktemp input.XXXX`
echo "$WORD" > "$TMPFILE" # includes newline
sendkeys -a "Google Chrome" --delay 0.001 -f "$TMPFILE" -i 0
rm $TMPFILE
