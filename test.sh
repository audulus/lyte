#!/bin/env bash
set -e

for file in tests/cases/*.lyte
do
    echo $file
    cd cli
    cargo run -- ../$file
    cd ..
done
