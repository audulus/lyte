#!/bin/bash
# Build CLyte.xcframework for macOS and iOS.
#
# Prerequisites:
#   rustup target add aarch64-apple-darwin aarch64-apple-ios aarch64-apple-ios-sim
#
# Usage: ./build-xcframework.sh

set -euo pipefail

XCFRAMEWORK="CLyte.xcframework"
HEADER_DIR="Sources/CLyte/include"

MACOS_ARM_TARGET="aarch64-apple-darwin"
MACOS_X86_TARGET="x86_64-apple-darwin"
IOS_TARGET="aarch64-apple-ios"
IOS_SIM_TARGET="aarch64-apple-ios-sim"

BUILD_DIR=".build/xcframework"

# Ensure targets are installed
rustup target add "$MACOS_ARM_TARGET" "$MACOS_X86_TARGET" "$IOS_TARGET" "$IOS_SIM_TARGET"

echo "Building for macOS ($MACOS_ARM_TARGET)..."
cargo build --release --target "$MACOS_ARM_TARGET"

echo "Building for macOS ($MACOS_X86_TARGET)..."
cargo build --release --target "$MACOS_X86_TARGET"

echo "Building for iOS ($IOS_TARGET)..."
cargo build --release --target "$IOS_TARGET"

echo "Building for iOS Simulator ($IOS_SIM_TARGET)..."
cargo build --release --target "$IOS_SIM_TARGET"

# Remove old xcframework and build dir
rm -rf "$XCFRAMEWORK" "$BUILD_DIR"
mkdir -p "$BUILD_DIR"

# Create universal macOS binary
echo "Creating universal macOS binary..."
lipo -create \
    "target/$MACOS_ARM_TARGET/release/liblyte.a" \
    "target/$MACOS_X86_TARGET/release/liblyte.a" \
    -output "$BUILD_DIR/liblyte-macos.a"

echo "Creating XCFramework..."
xcodebuild -create-xcframework \
    -library "$BUILD_DIR/liblyte-macos.a" -headers "$HEADER_DIR" \
    -library "target/$IOS_TARGET/release/liblyte.a" -headers "$HEADER_DIR" \
    -library "target/$IOS_SIM_TARGET/release/liblyte.a" -headers "$HEADER_DIR" \
    -output "$XCFRAMEWORK"

echo "Created $XCFRAMEWORK"
