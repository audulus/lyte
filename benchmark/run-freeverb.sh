#!/bin/bash
# Freeverb FFI performance benchmark.
#
# Builds liblyte.dylib with --features llvm, builds the FFI harness in
# benchmark/freeverb_bench.c, and runs it against tests/cases/freeverb.lyte
# RUNS times. Reports the median compile + exec time.
#
# This exercises lyte_compiler_compile (the codepath the xcframework hosts
# use) — same path that hid the no_recursion-not-threaded regression. The
# CLI does not touch this codepath, so this benchmark is the smallest thing
# that would have caught that bug.
#
# Usage: ./benchmark/run-freeverb.sh [RUNS] [ITERATIONS]
#   RUNS       number of harness runs to median over (default 5)
#   ITERATIONS process() calls per run (default 10000)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO_ROOT"

RUNS="${1:-5}"
ITERATIONS="${2:-10000}"

LYTE_FILE="tests/cases/freeverb.lyte"
HARNESS_SRC="benchmark/freeverb_bench.c"

LLVM_PREFIX="$(brew --prefix llvm@18 2>/dev/null || true)"
if [ -z "$LLVM_PREFIX" ] || [ ! -d "$LLVM_PREFIX" ]; then
    echo "Error: llvm@18 not found. Install with: brew install llvm@18" >&2
    exit 1
fi
export LLVM_SYS_180_PREFIX="$LLVM_PREFIX"
export LIBRARY_PATH="${LIBRARY_PATH:+$LIBRARY_PATH:}$LLVM_PREFIX/lib"

echo "Building liblyte.dylib (release, --features llvm)..."
cargo build --release --features llvm --lib --quiet

# Resolve actual target dir — honors a `.cargo/config.toml` target-dir override.
TARGET_DIR="$(cargo metadata --format-version=1 --no-deps 2>/dev/null \
    | sed -n 's/.*"target_directory":"\([^"]*\)".*/\1/p')"
TARGET_DIR="${TARGET_DIR:-./target}"
DYLIB_DIR="$TARGET_DIR/release"
if [ ! -f "$DYLIB_DIR/liblyte.dylib" ]; then
    echo "Error: $DYLIB_DIR/liblyte.dylib not found after build" >&2
    exit 1
fi

HEADER_DIR="Sources/CLyte/include"
HARNESS_BIN="$DYLIB_DIR/freeverb_bench"

echo "Building FFI harness..."
cc -O2 -Wall -I "$HEADER_DIR" \
    "$HARNESS_SRC" \
    -L "$DYLIB_DIR" -llyte \
    -Wl,-rpath,"$DYLIB_DIR" \
    -o "$HARNESS_BIN"

echo "Running freeverb FFI bench: $RUNS runs of $ITERATIONS process() calls"
echo ""

declare -a compile_samples=()
declare -a exec_samples=()
for r in $(seq 1 "$RUNS"); do
    out=$(FREEVERB_ITERATIONS="$ITERATIONS" "$HARNESS_BIN" "$LYTE_FILE")
    echo "  run $r: $out"
    c=$(printf '%s\n' "$out" | sed -n 's/.*compile=\([0-9.]*\)s.*/\1/p')
    e=$(printf '%s\n' "$out" | sed -n 's/.*exec=\([0-9.]*\)s.*/\1/p')
    if [ -z "$c" ] || [ -z "$e" ]; then
        echo "Error: failed to parse run $r output" >&2
        exit 1
    fi
    compile_samples+=("$c")
    exec_samples+=("$e")
done

# Bash median (RUNS is small, sort is fine).
median() {
    local sorted=($(printf '%s\n' "$@" | sort -n))
    local n=${#sorted[@]}
    printf '%s' "${sorted[$((n / 2))]}"
}

cm=$(median "${compile_samples[@]}")
em=$(median "${exec_samples[@]}")

echo ""
printf 'median compile=%ss  median exec=%ss\n' "$cm" "$em"
