#!/bin/env bash

LLVM_SYS_180_PREFIX=/opt/homebrew/opt/llvm@18 \
LIBRARY_PATH="/opt/homebrew/lib:$LIBRARY_PATH" \
cargo test --workspace --features llvm

