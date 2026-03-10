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

echo "--- Lyte (JIT) ---"
$LYTE benchmark/biquad.lyte -c --timing 2>&1 | grep -E "compile:|exec:"
echo ""

echo "--- Lyte (VM) ---"
$LYTE benchmark/biquad.lyte -r --timing 2>&1 | grep -E "compile:|exec:"
echo ""

echo "--- Lua $(lua -v 2>&1 | head -1) ---"
time lua benchmark/biquad.lua
echo ""

echo "--- LuaJIT JIT $(luajit -v 2>&1 | head -1) ---"
time luajit benchmark/biquad.lua
echo ""

echo "--- LuaJIT Interpreter (no JIT) ---"
time luajit -joff benchmark/biquad.lua
echo ""
