#!/bin/bash
# Build CLyte.xcframework for macOS and iOS using static libraries.
#
# Prerequisites:
#   rustup target add aarch64-apple-darwin x86_64-apple-darwin aarch64-apple-ios aarch64-apple-ios-sim
#   Xcode command line tools, Rosetta (for the x86_64 LLVM tools), and
#   network access to fetch the official LLVM release binaries on first run
#   (see ci/prepare-llvm.sh).
#
# Usage: ./build-xcframework.sh

set -euo pipefail

cd "$(dirname "$0")"

# Defensive: a globally-set LIBRARY_PATH leaks into host build scripts and
# can break cross-arch builds (e.g. host build scripts linking against the
# wrong arch). This script uses per-target CARGO_TARGET_<TRIPLE>_RUSTFLAGS
# to scope library search paths instead, so we clear any inherited value.
unset LIBRARY_PATH

# Deployment targets for the Rust code, and (via ci/prepare-llvm.sh) for the
# LLVM and zstd objects merged into the macOS slices. Checked below.
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

# LLVM for the macOS slices comes from the official LLVM release binaries,
# turned into a native static prefix (with a zstd built from source) by
# ci/prepare-llvm.sh; see that script for the details. The x86_64 prefix
# is used under Rosetta, since its llvm-config is an x86_64 binary.
ARM64_LLVM="$(ci/prepare-llvm.sh arm64)"
X86_LLVM="$(ci/prepare-llvm.sh x86_64)"
echo "Using arm64 LLVM from $ARM64_LLVM"
echo "Using x86_64 LLVM from $X86_LLVM"

# Library search paths are passed per-target via CARGO_TARGET_<TRIPLE>_RUSTFLAGS
# so they only affect the final-crate link and not the host build scripts.
ARM64_LINK_FLAGS="-L native=$ARM64_LLVM/lib"
X86_LINK_FLAGS="-L native=$X86_LLVM/lib"

echo "Building for macOS ($MACOS_ARM_TARGET) with LLVM..."
env \
    LLVM_SYS_191_PREFIX="$ARM64_LLVM" \
    CARGO_TARGET_AARCH64_APPLE_DARWIN_RUSTFLAGS="$ARM64_LINK_FLAGS" \
    cargo rustc --release --features llvm --target "$MACOS_ARM_TARGET" --crate-type staticlib

echo "Building for macOS ($MACOS_X86_TARGET) with LLVM..."
env \
    LLVM_SYS_191_PREFIX="$X86_LLVM" \
    CARGO_TARGET_X86_64_APPLE_DARWIN_RUSTFLAGS="$X86_LINK_FLAGS" \
    cargo rustc --release --features llvm --target "$MACOS_X86_TARGET" --crate-type staticlib

echo "Building for iOS ($IOS_TARGET) (stack VM)..."
cargo rustc --release --target "$IOS_TARGET" --crate-type staticlib

echo "Building for iOS Simulator ($IOS_SIM_TARGET) (stack VM)..."
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

# macOS: LLVM's zstd dependency is linked as a plain -lzstd, so rustc does not
# bundle it into the staticlib; merge it from the LLVM prefix. LLVM's other
# system libraries (z, xml2, m) come from the macOS SDK and are linked by the
# Swift package. libffi is bundled by the libffi-sys crate.
libtool -static -o "$BUILD_DIR/liblyte-arm64.a" \
    "$CARGO_TARGET_DIR/$MACOS_ARM_TARGET/release/liblyte.a" \
    "$ARM64_LLVM/lib/libzstd.a"

libtool -static -o "$BUILD_DIR/liblyte-x86_64.a" \
    "$CARGO_TARGET_DIR/$MACOS_X86_TARGET/release/liblyte.a" \
    "$X86_LLVM/lib/libzstd.a"

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

# Guard: every object merged into a slice must have a minimum OS no newer
# than the deployment target. The Rust code honours MACOSX_DEPLOYMENT_TARGET
# and ci/prepare-llvm.sh compiles LLVM and zstd for it, but a prebuilt
# library from elsewhere (as the Homebrew bottles used to be) carries the
# minimum OS of whatever built it. An object above the deployment target
# makes every consumer that links at that target warn "was built for newer
# 'macOS' version", and can reference symbols the older OS lacks.
# Args: <archive> <platform name as otool prints it> <max allowed version>
check_min_os() {
    local archive="$1" platform="$2" max="$3"
    local objs
    # otool prints one LC_BUILD_VERSION (or LC_VERSION_MIN_*) block per
    # object; the object name precedes each block as "archive(object):".
    objs=$(otool -l "$archive" | awk -v max="$max" -v plat="$platform" '
        function newer(a, b,   x, y, n, i) {
            n = split(a, x, "."); split(b, y, ".")
            for (i = 1; i <= n; i++) {
                if ((x[i]+0) > (y[i]+0)) return 1
                if ((x[i]+0) < (y[i]+0)) return 0
            }
            return 0
        }
        /^[^ ].*\):$/ { obj = $0; sub(/:$/, "", obj); sub(/^.*\(/, "", obj); sub(/\)$/, "", obj) }
        /^ *platform / { p = $2 }
        /^ *minos / { if (newer($2, max)) print obj " minos " $2 }
        /^ *version / && p == "" { if (newer($2, max)) print obj " version " $2 }
        /^ *cmd / { p = "" }
    ')
    if [ -n "$objs" ]; then
        echo "Error: $archive contains objects built for $platform newer than $max:" >&2
        echo "$objs" | head -20 >&2
        local n
        n=$(echo "$objs" | wc -l | tr -d ' ')
        echo "($n objects total)" >&2
        echo "Every merged library must be built for $platform $max or older." >&2
        exit 1
    fi
}

echo "Checking minimum OS versions..."
check_min_os "$BUILD_DIR/liblyte-arm64.a" macOS "$MACOSX_DEPLOYMENT_TARGET"
check_min_os "$BUILD_DIR/liblyte-x86_64.a" macOS "$MACOSX_DEPLOYMENT_TARGET"
check_min_os "$BUILD_DIR/liblyte-ios.a" iOS "$IPHONEOS_DEPLOYMENT_TARGET"
check_min_os "$BUILD_DIR/liblyte-ios-sim.a" iOS "$IPHONEOS_DEPLOYMENT_TARGET"

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
