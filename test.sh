#!/bin/env bash

for file in tests/cases/*.lyte
do
    cd cli
    cargo run -- ../$file >& /dev/null
    result=$?

    if grep -q "expect failure" "../$file" ; then
        if [ $result -eq 0 ]; then
            echo ❌ $file expected failure
        else
            echo ✅ $file expected failure
        fi
    else
        if [ $result -eq 0 ]; then
            echo ✅ $file
        else
            echo ❌ $file
        fi
    fi
    cd ..
done
