#!/bin/env bash

for file in tests/cases/*.lyte
do
    echo $file
    cd cli
    cargo run -- ../$file
    cd ..
done
