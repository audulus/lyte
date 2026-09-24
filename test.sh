#!/bin/env bash

# Detect LLVM 19 prefix
if [ "$(uname -s)" = "Darwin" ]; then
    # macOS: the official LLVM release binaries, prepared by ci/prepare-llvm.sh
    # (downloaded and converted on first run, cached after that).
    LLVM_PREFIX="$("$(dirname "$0")/ci/prepare-llvm.sh" "$(uname -m)")"
    LIB_DIR="$LLVM_PREFIX/lib"
elif [ -d "/usr/lib/llvm-19" ]; then
    # Linux (apt: llvm-19-dev)
    LLVM_PREFIX="/usr/lib/llvm-19"
    LIB_DIR="/usr/lib/llvm-19/lib"
elif llvm-config-19 --prefix &>/dev/null; then
    # Linux (llvm-config-19 on PATH)
    LLVM_PREFIX="$(llvm-config-19 --prefix)"
    LIB_DIR="$(llvm-config-19 --libdir)"
elif llvm-config --version 2>/dev/null | grep -q '^19\.'; then
    # Linux (unversioned llvm-config is v19)
    LLVM_PREFIX="$(llvm-config --prefix)"
    LIB_DIR="$(llvm-config --libdir)"
else
    echo "Error: LLVM 19 not found."
    echo "  Ubuntu: apt install llvm-19-dev libpolly-19-dev libzstd-dev"
    exit 1
fi

LLVM_SYS_191_PREFIX="$LLVM_PREFIX" \
LIBRARY_PATH="$LIB_DIR:$LIBRARY_PATH" \
cargo test --workspace --features llvm

