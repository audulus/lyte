#!/bin/bash
# Build CLyte.xcframework for macOS and iOS using static libraries.
#
# Prerequisites:
#   rustup target add aarch64-apple-darwin x86_64-apple-darwin aarch64-apple-ios aarch64-apple-ios-sim
#
# Usage: ./build-xcframework.sh

set -euo pipefail

# Set deployment targets to suppress linker version mismatch warnings.
export MACOSX_DEPLOYMENT_TARGET="15.0"
export IPHONEOS_DEPLOYMENT_TARGET="16.0"

# Use a target dir without spaces (autotools/libffi can't handle spaces in paths).
export CARGO_TARGET_DIR="/tmp/lyte-build"

FRAMEWORK_NAME="CLyte"
XCFRAMEWORK="$FRAMEWORK_NAME.xcframework"
HEADER_DIR="Sources/CLyte/include"

MACOS_ARM_TARGET="aarch64-apple-darwin"
MACOS_X86_TARGET="x86_64-apple-darwin"
IOS_TARGET="aarch64-apple-ios"
IOS_SIM_TARGET="aarch64-apple-ios-sim"

BUILD_DIR=".build/xcframework"

rm -rf $XCFRAMEWORK
rm -f $XCFRAMEWORK.zip

# Ensure targets are installed
rustup target add "$MACOS_ARM_TARGET" "$MACOS_X86_TARGET" "$IOS_TARGET" "$IOS_SIM_TARGET"

# Auto-detect LLVM 18 from Homebrew if not already set
if [ -z "${LLVM_SYS_180_PREFIX:-}" ]; then
    LLVM_SYS_180_PREFIX="$(brew --prefix llvm@18 2>/dev/null || true)"
    if [ -z "$LLVM_SYS_180_PREFIX" ] || [ ! -d "$LLVM_SYS_180_PREFIX" ]; then
        echo "Error: LLVM 18 not found. Install with: brew install llvm@18" >&2
        exit 1
    fi
    export LLVM_SYS_180_PREFIX
    echo "Using LLVM from $LLVM_SYS_180_PREFIX"
fi

# Ensure linker can find LLVM's dependencies (zstd, zlib, etc.)
export LIBRARY_PATH="${LLVM_SYS_180_PREFIX}/lib:$(brew --prefix zstd)/lib:$(brew --prefix)/lib${LIBRARY_PATH:+:$LIBRARY_PATH}"

echo "Building for macOS ($MACOS_ARM_TARGET) with LLVM..."
cargo rustc --release --features llvm --target "$MACOS_ARM_TARGET" --crate-type staticlib

echo "Building for macOS ($MACOS_X86_TARGET) (VM only)..."
cargo rustc --release --target "$MACOS_X86_TARGET" --crate-type staticlib

echo "Building for iOS ($IOS_TARGET) (VM only)..."
cargo rustc --release --target "$IOS_TARGET" --crate-type staticlib

echo "Building for iOS Simulator ($IOS_SIM_TARGET) (VM only)..."
cargo rustc --release --target "$IOS_SIM_TARGET" --crate-type staticlib

# Remove old xcframework and build dir
rm -rf "$XCFRAMEWORK" "$BUILD_DIR"
mkdir -p "$BUILD_DIR"

# Helper: find the libffi.a built by libffi-sys for a given target.
find_libffi() {
    find "$CARGO_TARGET_DIR/$1/release/build" -path "*/libffi-sys-*/out/libffi-root/lib/libffi.a" 2>/dev/null | head -1
}

# Merge static dependencies into each target library so the xcframework is self-contained.
echo "Merging static dependencies..."

# macOS ARM64: merge LLVM deps (zstd, ffi from brew) + Rust-built libffi
ZSTD_LIB="$(brew --prefix zstd)/lib/libzstd.a"
BREW_FFI_LIB="$(brew --prefix libffi)/lib/libffi.a"
libtool -static -o "$BUILD_DIR/liblyte-arm64.a" \
    "$CARGO_TARGET_DIR/$MACOS_ARM_TARGET/release/liblyte.a" \
    "$ZSTD_LIB" "$BREW_FFI_LIB"

# macOS x86_64: merge cross-compiled libffi
X86_FFI=$(find_libffi "$MACOS_X86_TARGET")
libtool -static -o "$BUILD_DIR/liblyte-x86_64.a" \
    "$CARGO_TARGET_DIR/$MACOS_X86_TARGET/release/liblyte.a" \
    "$X86_FFI"

# iOS: merge cross-compiled libffi
IOS_FFI=$(find_libffi "$IOS_TARGET")
libtool -static -o "$BUILD_DIR/liblyte-ios.a" \
    "$CARGO_TARGET_DIR/$IOS_TARGET/release/liblyte.a" \
    "$IOS_FFI"

# iOS Simulator: merge cross-compiled libffi
SIM_FFI=$(find_libffi "$IOS_SIM_TARGET")
libtool -static -o "$BUILD_DIR/liblyte-ios-sim.a" \
    "$CARGO_TARGET_DIR/$IOS_SIM_TARGET/release/liblyte.a" \
    "$SIM_FFI"

echo "Creating macOS universal static library..."
lipo -create \
    "$BUILD_DIR/liblyte-arm64.a" \
    "$BUILD_DIR/liblyte-x86_64.a" \
    -output "$BUILD_DIR/liblyte-macos.a"

echo "Creating XCFramework..."
xcodebuild -create-xcframework \
    -library "$BUILD_DIR/liblyte-macos.a" \
    -headers "$HEADER_DIR" \
    -library "$BUILD_DIR/liblyte-ios.a" \
    -headers "$HEADER_DIR" \
    -library "$BUILD_DIR/liblyte-ios-sim.a" \
    -headers "$HEADER_DIR" \
    -output "$XCFRAMEWORK"

echo "Created $XCFRAMEWORK"

echo "Zipping $XCFRAMEWORK..."
zip -r "$XCFRAMEWORK.zip" "$XCFRAMEWORK"
echo "Created $XCFRAMEWORK.zip"

CHECKSUM=$(swift package compute-checksum "$XCFRAMEWORK.zip")
echo "Checksum: $CHECKSUM"
