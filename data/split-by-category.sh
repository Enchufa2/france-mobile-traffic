#!/bin/bash

DIR=$(python3 -c "import sys, os.path; print(os.path.commonprefix(sys.argv[1:]))" "$@")
mkdir -p $DIR
zcat $@ | awk -vDIR="$DIR" -F';' '{print $1, $2, $4, $5 | "gzip > "DIR"/"$3".csv.gz"}'
