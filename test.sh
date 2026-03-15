#!/bin/env bash

# Detect LLVM 18 prefix (ARM Mac vs Intel Mac)
if [ -d "/opt/homebrew/opt/llvm@18" ]; then
    LLVM_PREFIX="/opt/homebrew/opt/llvm@18"
    LIB_DIR="/opt/homebrew/lib"
elif [ -d "/usr/local/opt/llvm@18" ]; then
    LLVM_PREFIX="/usr/local/opt/llvm@18"
    LIB_DIR="/usr/local/lib"
else
    echo "Error: LLVM 18 not found. Install with: brew install llvm@18"
    exit 1
fi

LLVM_SYS_181_PREFIX="$LLVM_PREFIX" \
LIBRARY_PATH="$LIB_DIR:$LIBRARY_PATH" \
cargo test --workspace --features llvm

