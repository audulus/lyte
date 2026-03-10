#!/bin/bash
# Biquad filter DSP benchmark: Lyte vs Lua
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR/.."

echo "=== Biquad Filter DSP Benchmark ==="
echo "Processing 10M samples of 440Hz sine through a 1kHz lowpass"
echo ""

# Build lyte in release mode
echo "Building lyte (release)..."
cargo build -p lyte-cli --release --quiet 2>/dev/null
LYTE=./target/release/lyte

# Helper: extract seconds from "exec: 1.234s" or "jit exec: 0.012s" or "vm exec: 0.779s"
extract_exec() {
    grep -oE '[0-9]+\.[0-9]+s' | head -1 | sed 's/s$//'
}

echo "Building C benchmark..."
cc -O2 -o benchmark/biquad_c benchmark/biquad.c -lm

echo "Running benchmarks..."

C_TIME=$(benchmark/biquad_c 2>&1 | extract_exec)
LYTE_JIT=$($LYTE benchmark/biquad.lyte -c --timing 2>&1 | grep "jit exec:" | extract_exec)
LYTE_VM=$($LYTE benchmark/biquad.lyte -r --timing 2>&1 | grep "vm exec:" | extract_exec)

if ! command -v lua &>/dev/null; then
    echo "Error: lua not found. Install with: brew install lua" >&2
    exit 1
fi
if ! command -v luajit &>/dev/null; then
    echo "Error: luajit not found. Install with: brew install luajit" >&2
    exit 1
fi

LUA=$(lua benchmark/biquad.lua 2>&1 | extract_exec)
LUAJIT_JIT=$(luajit benchmark/biquad.lua 2>&1 | extract_exec)
LUAJIT_INT=$(luajit -joff benchmark/biquad.lua 2>&1 | extract_exec)

echo ""
printf "%-24s %10s %8s\n" "Runtime" "Time (s)" "vs Lua"
printf "%-24s %10s %8s\n" "------------------------" "--------" "------"

row() {
    local name=$1 time=$2 baseline=$3
    if [ -z "$time" ]; then
        printf "%-24s %10s %8s\n" "$name" "N/A" "N/A"
        return
    fi
    local ratio
    ratio=$(awk -v b="$baseline" -v t="$time" 'BEGIN { printf "%.2f", b / t }')
    printf "%-24s %10.3f %7sx\n" "$name" "$time" "$ratio"
}

row "Lua 5.5"           "$LUA"        "$LUA"
row "LuaJIT (interp)"   "$LUAJIT_INT" "$LUA"
row "Lyte VM"           "$LYTE_VM"    "$LUA"
row "LuaJIT (JIT)"      "$LUAJIT_JIT" "$LUA"
row "Lyte JIT"          "$LYTE_JIT"   "$LUA"
row "C (-O2)"           "$C_TIME"     "$LUA"
