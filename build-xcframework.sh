#!/bin/bash
# Build CLyte.xcframework for macOS and iOS using static libraries.
#
# Prerequisites:
#   rustup target add aarch64-apple-darwin x86_64-apple-darwin aarch64-apple-ios aarch64-apple-ios-sim
#
# Usage: ./build-xcframework.sh

set -euo pipefail

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

echo "Creating macOS universal static library..."
lipo -create \
    "target/$MACOS_ARM_TARGET/release/liblyte.a" \
    "target/$MACOS_X86_TARGET/release/liblyte.a" \
    -output "$BUILD_DIR/liblyte-macos.a"

echo "Creating XCFramework..."
xcodebuild -create-xcframework \
    -library "$BUILD_DIR/liblyte-macos.a" \
    -headers "$HEADER_DIR" \
    -library "target/$IOS_TARGET/release/liblyte.a" \
    -headers "$HEADER_DIR" \
    -library "target/$IOS_SIM_TARGET/release/liblyte.a" \
    -headers "$HEADER_DIR" \
    -output "$XCFRAMEWORK"

echo "Created $XCFRAMEWORK"

echo "Zipping $XCFRAMEWORK..."
zip -r "$XCFRAMEWORK.zip" "$XCFRAMEWORK"
echo "Created $XCFRAMEWORK.zip"

echo "Updating checksum in Package.swift..."
CHECKSUM=$(swift package compute-checksum "$XCFRAMEWORK.zip")
sed -i '' "s/checksum: \"[a-f0-9]*\"/checksum: \"$CHECKSUM\"/" Package.swift
echo "Updated checksum to $CHECKSUM"
