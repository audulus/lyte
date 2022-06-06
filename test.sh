#!/bin/env bash

cargo test

cd cli
cargo run -- --test ../tests/cases/
