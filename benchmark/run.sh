#!/bin/bash
# Lyte benchmark suite: Lyte vs C vs Lua
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR/.."

RUNS=${1:-5}

echo "=== Lyte Benchmark Suite ==="
echo "Averaging $RUNS runs per benchmark"
echo ""

# Build lyte in release mode (with LLVM if available)
echo "Building lyte (release)..."
export LLVM_SYS_180_PREFIX="$(brew --prefix llvm@18 2>/dev/null)"
export LIBRARY_PATH="${LIBRARY_PATH:+$LIBRARY_PATH:}$(brew --prefix zstd 2>/dev/null)/lib"
if [ -n "$LLVM_SYS_180_PREFIX" ] && cargo build -p lyte-cli --release --features llvm --quiet 2>/dev/null; then
    HAS_LLVM=1
else
    echo "  (LLVM feature not available, building without it)"
    cargo build -p lyte-cli --release --quiet 2>/dev/null
    HAS_LLVM=0
fi
# Resolve actual target dir — honors a `.cargo/config.toml` target-dir
# override so the script doesn't rely on `./target/release/lyte` existing.
TARGET_DIR="$(cargo metadata --format-version=1 --no-deps 2>/dev/null \
    | sed -n 's/.*"target_directory":"\([^"]*\)".*/\1/p')"
LYTE="${TARGET_DIR:-./target}/release/lyte"

echo "Building C benchmarks..."
cc -O2 -o benchmark/biquad_c_o2 benchmark/biquad.c -lm
cc -O3 -o benchmark/biquad_c_o3 benchmark/biquad.c -lm
cc -O2 -o benchmark/sort_c_o2 benchmark/sort.c -lm
cc -O3 -o benchmark/sort_c_o3 benchmark/sort.c -lm
cc -O2 -o benchmark/fft_c_o2 benchmark/fft.c -lm
cc -O3 -o benchmark/fft_c_o3 benchmark/fft.c -lm

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

# Tracks whether any runtime failed (missing output, crash, etc.) so the
# script can exit non-zero and break CI instead of silently printing N/A.
FAIL=0

# Run a command $RUNS times, extract exec time, return the average.
# First arg is a human-readable label used in error reporting.
# If any run produces no timing output, mark FAIL and return empty so the
# row renders as N/A.
avg() {
    local label=$1
    shift
    local sum=0
    for ((r = 0; r < RUNS; r++)); do
        local t
        t=$(eval "$@" 2>&1 | extract_exec)
        if [ -z "$t" ]; then
            echo "  !! $label failed on run $r" >&2
            FAIL=1
            return
        fi
        sum=$(awk -v s="$sum" -v t="$t" 'BEGIN { printf "%.6f", s + t }')
    done
    awk -v s="$sum" -v n="$RUNS" 'BEGIN { printf "%.3f", s / n }'
}

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

run_benchmark() {
    local NAME=$1
    local LYTE_FILE=$2
    local C_O2=$3
    local C_O3=$4
    local LUA_FILE=$5
    local DESC=$6

    echo ""
    echo "--- $NAME: $DESC ---"
    echo ""

    local C_O2_TIME C_O3_TIME LYTE_JIT LYTE_VM LYTE_ASM LYTE_STACK LYTE_LLVM LUA LUAJIT_JIT LUAJIT_INT

    echo "Running benchmarks..."
    C_O2_TIME=$(avg "C -O2 ($NAME)" "$C_O2")
    C_O3_TIME=$(avg "C -O3 ($NAME)" "$C_O3")
    LYTE_JIT=$(avg "Lyte Cranelift ($NAME)" "$LYTE --backend jit $LYTE_FILE --timing 2>&1 | grep 'jit exec:'")
    LYTE_VM=$(avg "Lyte VM ($NAME)" "$LYTE --backend vm $LYTE_FILE --timing 2>&1 | grep 'vm exec:'")
    if [ "$(uname -m)" = "arm64" ] || [ "$(uname -m)" = "aarch64" ]; then
        LYTE_ASM=$(avg "Lyte ASM ($NAME)" "$LYTE --backend asm $LYTE_FILE --timing 2>&1 | grep 'asm exec:'")
    else
        LYTE_ASM=""
    fi
    if $LYTE --backend stack /dev/null 2>&1 | grep -q "requires Clang"; then
        LYTE_STACK=""
    else
        LYTE_STACK=$(avg "Lyte Stack VM ($NAME)" "$LYTE --backend stack $LYTE_FILE --timing 2>&1 | grep 'stack exec:'")
    fi
    if [ "$HAS_LLVM" = "1" ]; then
        LYTE_LLVM=$(avg "Lyte LLVM ($NAME)" "$LYTE --backend llvm $LYTE_FILE --timing 2>&1 | grep 'llvm exec:'")
    else
        LYTE_LLVM=""
    fi
    LUA=$(avg "Lua 5.5 ($NAME)" lua "$LUA_FILE")
    LUAJIT_JIT=$(avg "LuaJIT JIT ($NAME)" luajit "$LUA_FILE")
    LUAJIT_INT=$(avg "LuaJIT interp ($NAME)" luajit -joff "$LUA_FILE")

    printf "%-24s %10s %8s\n" "Runtime" "Time (s)" "vs Lua"
    printf "%-24s %10s %8s\n" "------------------------" "--------" "------"

    row "Lua 5.5"           "$LUA"        "$LUA"
    row "LuaJIT (interp)"   "$LUAJIT_INT" "$LUA"
    row "Lyte VM"           "$LYTE_VM"    "$LUA"
    row "Lyte Stack VM"     "$LYTE_STACK" "$LUA"
    row "Lyte VM (ARM64)"   "$LYTE_ASM"   "$LUA"
    row "LuaJIT (JIT)"      "$LUAJIT_JIT" "$LUA"
    row "Lyte Cranelift"    "$LYTE_JIT"   "$LUA"
    row "Lyte LLVM"         "$LYTE_LLVM"  "$LUA"
    row "C (-O2)"           "$C_O2_TIME"  "$LUA"
    row "C (-O3)"           "$C_O3_TIME"  "$LUA"
}

run_benchmark "Biquad" \
    "benchmark/biquad.lyte" \
    "benchmark/biquad_c_o2" \
    "benchmark/biquad_c_o3" \
    "benchmark/biquad.lua" \
    "10M samples through a 1kHz lowpass"

run_benchmark "Sort" \
    "benchmark/sort.lyte" \
    "benchmark/sort_c_o2" \
    "benchmark/sort_c_o3" \
    "benchmark/sort.lua" \
    "Quicksort 10K elements x 50 iterations"

run_benchmark "FFT" \
    "benchmark/fft.lyte" \
    "benchmark/fft_c_o2" \
    "benchmark/fft_c_o3" \
    "benchmark/fft.lua" \
    "1024-point FFT x 2000 iterations"

if [ "$FAIL" = "1" ]; then
    echo ""
    echo "!! One or more benchmarks failed — see errors above" >&2
    exit 1
fi
