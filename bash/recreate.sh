#!/bin/bash
FILES=$1 # original structure
TMPL=$2  # where to create empty files
[[ -d $FILES ]] || exit 1
[[ -d $TMPL ]] || mkdir -p $TMPL || exit 1

cd $TMPL
gfind "$FILES" -type d -printf "$TMPL%p\0" | sort -z | xargs -0 -L 1000 mkdir -p
gfind "$FILES" -type f -printf "$TMPL%p\0" | xargs -0 -L 1000 touch