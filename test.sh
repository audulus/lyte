#!/bin/env bash

for file in tests/cases/*.lyte
do
    cd cli
    cargo run -- ../$file >& /dev/null
    if [ $? -eq 0 ]; then
      echo ✅ $file
    else
      echo ❌ $file
    fi
    cd ..
done
