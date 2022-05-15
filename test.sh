#!/bin/env bash
set -e

for file in tests/cases/*.lyte
do
    cd cli
    cargo run -- ../$file >& /dev/null
    if [ $? -eq 0 ]; then
      echo $file PASSED 
    else
      echo $file FAILED 
    fi
    cd ..
done
