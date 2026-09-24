#!/bin/bash
# Build CLyte.xcframework for macOS and iOS using static libraries.
#
# Prerequisites:
#   rustup target add aarch64-apple-darwin x86_64-apple-darwin aarch64-apple-ios aarch64-apple-ios-sim
#
# Usage: ./build-xcframework.sh

set -euo pipefail

# Defensive: a globally-set LIBRARY_PATH leaks into host build scripts and
# can break cross-arch builds (e.g. host build scripts linking against the
# wrong arch). This script uses per-target CARGO_TARGET_<TRIPLE>_RUSTFLAGS
# to scope library search paths instead, so we clear any inherited value.
unset LIBRARY_PATH

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

# Homebrew prefixes: arm64 brew at /opt/homebrew, x86_64 brew at /usr/local.
# Both are needed because llvm-sys/inkwell require a native llvm-config per
# target arch, and the linker needs arch-matched libzstd / libffi static libs.
ARM64_BREW="$(/opt/homebrew/bin/brew --prefix 2>/dev/null || true)"
X86_BREW="$(arch -x86_64 /usr/local/bin/brew --prefix 2>/dev/null || true)"

if [ -z "$ARM64_BREW" ] || [ ! -d "$ARM64_BREW/opt/llvm@18" ]; then
    echo "Error: arm64 LLVM 18 not found. Install with: brew install llvm@18" >&2
    exit 1
fi
if [ -z "$X86_BREW" ] || [ ! -d "$X86_BREW/opt/llvm@18" ]; then
    echo "Error: x86_64 LLVM 18 not found. Install with:" >&2
    echo "    arch -x86_64 /usr/local/bin/brew install llvm@18 zstd libffi" >&2
    exit 1
fi

ARM64_LLVM="$ARM64_BREW/opt/llvm@18"
X86_LLVM="$X86_BREW/opt/llvm@18"
echo "Using arm64 LLVM from $ARM64_LLVM"
echo "Using x86_64 LLVM from $X86_LLVM"

# Library search paths are passed per-target via CARGO_TARGET_<TRIPLE>_RUSTFLAGS
# so they only affect the final-crate link and not the host build scripts
# (which still need arm64 system libs from the host toolchain).
ARM64_LINK_FLAGS="-L native=$ARM64_LLVM/lib -L native=$ARM64_BREW/opt/zstd/lib -L native=$ARM64_BREW/lib"
X86_LINK_FLAGS="-L native=$X86_LLVM/lib -L native=$X86_BREW/opt/zstd/lib -L native=$X86_BREW/opt/libffi/lib -L native=$X86_BREW/lib"

echo "Building for macOS ($MACOS_ARM_TARGET) with LLVM..."
env \
    LLVM_SYS_180_PREFIX="$ARM64_LLVM" \
    CARGO_TARGET_AARCH64_APPLE_DARWIN_RUSTFLAGS="$ARM64_LINK_FLAGS" \
    cargo rustc --release --features llvm --target "$MACOS_ARM_TARGET" --crate-type staticlib

echo "Building for macOS ($MACOS_X86_TARGET) with LLVM..."
env \
    LLVM_SYS_180_PREFIX="$X86_LLVM" \
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

# macOS ARM64: merge LLVM deps (zstd, ffi) from the arm64 brew.
libtool -static -o "$BUILD_DIR/liblyte-arm64.a" \
    "$CARGO_TARGET_DIR/$MACOS_ARM_TARGET/release/liblyte.a" \
    "$ARM64_BREW/opt/zstd/lib/libzstd.a" \
    "$ARM64_BREW/opt/libffi/lib/libffi.a"

# macOS x86_64: merge LLVM deps (zstd, ffi) from the x86_64 brew.
libtool -static -o "$BUILD_DIR/liblyte-x86_64.a" \
    "$CARGO_TARGET_DIR/$MACOS_X86_TARGET/release/liblyte.a" \
    "$X86_BREW/opt/zstd/lib/libzstd.a" \
    "$X86_BREW/opt/libffi/lib/libffi.a"

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
# than the deployment target. The Rust code honours MACOSX_DEPLOYMENT_TARGET,
# but the Homebrew llvm@18 / zstd / libffi archives are prebuilt bottles whose
# minimum OS is whatever macOS built them. A bottle from a newer macOS makes
# every consumer that links at the deployment target warn "was built for
# newer 'macOS' version", and can reference symbols the older OS lacks.
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
        echo "Homebrew bottles are built for the macOS that installed them; build on macOS $max or older." >&2
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
