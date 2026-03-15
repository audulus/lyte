#!/usr/bin/env bash

# Detect LLVM 18 prefix (ARM Mac vs Intel Mac vs Linux)
if [ -d "/opt/homebrew/opt/llvm@18" ]; then
    LLVM_PREFIX="/opt/homebrew/opt/llvm@18"
    LIB_DIR="/opt/homebrew/lib"
elif [ -d "/usr/local/opt/llvm@18" ]; then
    LLVM_PREFIX="/usr/local/opt/llvm@18"
    LIB_DIR="/usr/local/lib"
elif [ -d "/usr/lib/llvm-18" ]; then
    LLVM_PREFIX="/usr/lib/llvm-18"
    LIB_DIR="/usr/lib/llvm-18/lib"
else
    echo "Error: LLVM 18 not found. Install with: brew install llvm@18"
    exit 1
fi

RUNS=${1:-10}

echo "Building with LLVM..."
LLVM_SYS_181_PREFIX="$LLVM_PREFIX" \
LIBRARY_PATH="$LIB_DIR:$LIBRARY_PATH" \
cargo build -p lyte-cli --features llvm --quiet || exit 1

FAILED=0
for i in $(seq 1 "$RUNS"); do
    echo -n "Run $i: "
    if ! ./target/debug/lyte tests/cases/slices/empty_string.lyte --backend llvm; then
        FAILED=$((FAILED + 1))
    fi
done

echo ""
if [ "$FAILED" -eq 0 ]; then
    echo "All $RUNS runs passed."
else
    echo "$FAILED/$RUNS runs failed."
    exit 1
fi
