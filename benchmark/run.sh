#!/bin/bash
# Biquad filter DSP benchmark: Lyte vs Lua
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR/.."

RUNS=${1:-5}

echo "=== Biquad Filter DSP Benchmark ==="
echo "Processing 10M samples of 440Hz sine through a 1kHz lowpass"
echo "Averaging $RUNS runs per benchmark"
echo ""

# Build lyte in release mode
echo "Building lyte (release)..."
cargo build -p lyte-cli --release --quiet 2>/dev/null
LYTE=./target/release/lyte

echo "Building C benchmark..."
cc -O2 -o benchmark/biquad_c benchmark/biquad.c -lm

if ! command -v lua &>/dev/null; then
    echo "Error: lua not found. Install with: brew install lua" >&2
    exit 1
fi
if ! command -v luajit &>/dev/null; then
    echo "Error: luajit not found. Install with: brew install luajit" >&2
    exit 1
fi

# Helper: extract seconds from "exec: 1.234s" or "jit exec: 0.012s" or "vm exec: 0.779s"
extract_exec() {
    grep -oE '[0-9]+\.[0-9]+s' | head -1 | sed 's/s$//'
}

# Run a command $RUNS times, extract exec time, return the average
avg() {
    local sum=0
    for ((r = 0; r < RUNS; r++)); do
        local t
        t=$(eval "$@" 2>&1 | extract_exec)
        sum=$(awk -v s="$sum" -v t="$t" 'BEGIN { printf "%.6f", s + t }')
    done
    awk -v s="$sum" -v n="$RUNS" 'BEGIN { printf "%.3f", s / n }'
}

echo "Running benchmarks..."

C_TIME=$(avg benchmark/biquad_c)
LYTE_JIT=$(avg "$LYTE benchmark/biquad.lyte -c --timing 2>&1 | grep 'jit exec:'")
LYTE_VM=$(avg "$LYTE benchmark/biquad.lyte -r --timing 2>&1 | grep 'vm exec:'")
LUA=$(avg lua benchmark/biquad.lua)
LUAJIT_JIT=$(avg luajit benchmark/biquad.lua)
LUAJIT_INT=$(avg luajit -joff benchmark/biquad.lua)

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
