#!/bin/env bash

# Detect LLVM 18 prefix
if [ -d "/opt/homebrew/opt/llvm@18" ]; then
    # ARM Mac (Homebrew)
    LLVM_PREFIX="/opt/homebrew/opt/llvm@18"
    LIB_DIR="/opt/homebrew/lib"
elif [ -d "/usr/local/opt/llvm@18" ]; then
    # Intel Mac (Homebrew)
    LLVM_PREFIX="/usr/local/opt/llvm@18"
    LIB_DIR="/usr/local/lib"
elif [ -d "/usr/lib/llvm-18" ]; then
    # Linux (apt: llvm-18-dev)
    LLVM_PREFIX="/usr/lib/llvm-18"
    LIB_DIR="/usr/lib/llvm-18/lib"
elif llvm-config-18 --prefix &>/dev/null; then
    # Linux (llvm-config-18 on PATH)
    LLVM_PREFIX="$(llvm-config-18 --prefix)"
    LIB_DIR="$(llvm-config-18 --libdir)"
elif llvm-config --version 2>/dev/null | grep -q '^18\.'; then
    # Linux (unversioned llvm-config is v18)
    LLVM_PREFIX="$(llvm-config --prefix)"
    LIB_DIR="$(llvm-config --libdir)"
else
    echo "Error: LLVM 18 not found."
    echo "  macOS:  brew install llvm@18"
    echo "  Ubuntu: apt install llvm-18-dev"
    exit 1
fi

LLVM_SYS_181_PREFIX="$LLVM_PREFIX" \
LIBRARY_PATH="$LIB_DIR:$LIBRARY_PATH" \
cargo test --workspace --features llvm

