#!/bin/bash
# Build CLyte.xcframework for macOS and iOS using dynamic frameworks.
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

# Write Info.plist to a given path
write_info_plist() {
    local plist_path="$1"
    cat > "$plist_path" <<PLIST
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>CFBundleDevelopmentRegion</key>
    <string>en</string>
    <key>CFBundleExecutable</key>
    <string>$FRAMEWORK_NAME</string>
    <key>CFBundleIdentifier</key>
    <string>com.audulus.CLyte</string>
    <key>CFBundleInfoDictionaryVersion</key>
    <string>6.0</string>
    <key>CFBundleName</key>
    <string>$FRAMEWORK_NAME</string>
    <key>CFBundlePackageType</key>
    <string>FMWK</string>
    <key>CFBundleShortVersionString</key>
    <string>0.1.0</string>
    <key>CFBundleVersion</key>
    <string>1</string>
</dict>
</plist>
PLIST
}

# Write module.modulemap to a given path
write_modulemap() {
    local modulemap_path="$1"
    cat > "$modulemap_path" <<MODULEMAP
framework module $FRAMEWORK_NAME {
    header "lyte.h"
    export *
}
MODULEMAP
}

# Create a .framework bundle from a dylib.
# Usage: create_framework <dylib_path> <platform>
# macOS uses deep bundle structure (Versions/A/...), iOS uses shallow.
create_framework() {
    local dylib_path="$1"
    local platform="$2"
    local framework_dir="$BUILD_DIR/$platform/$FRAMEWORK_NAME.framework"

    if [[ "$platform" == "macos" ]]; then
        # Deep bundle structure for macOS
        local version_dir="$framework_dir/Versions/A"
        mkdir -p "$version_dir/Headers" "$version_dir/Modules" "$version_dir/Resources"

        cp "$dylib_path" "$version_dir/$FRAMEWORK_NAME"
        install_name_tool -id "@rpath/$FRAMEWORK_NAME.framework/Versions/A/$FRAMEWORK_NAME" "$version_dir/$FRAMEWORK_NAME"
        cp "$HEADER_DIR"/*.h "$version_dir/Headers/"
        write_modulemap "$version_dir/Modules/module.modulemap"
        write_info_plist "$version_dir/Resources/Info.plist"

        # Create Versions/Current symlink
        (cd "$framework_dir/Versions" && ln -sf A Current)

        # Create top-level symlinks
        (cd "$framework_dir" && ln -sf Versions/Current/$FRAMEWORK_NAME $FRAMEWORK_NAME)
        (cd "$framework_dir" && ln -sf Versions/Current/Headers Headers)
        (cd "$framework_dir" && ln -sf Versions/Current/Modules Modules)
        (cd "$framework_dir" && ln -sf Versions/Current/Resources Resources)
    else
        # Shallow bundle structure for iOS / iOS Simulator
        mkdir -p "$framework_dir/Headers" "$framework_dir/Modules"

        cp "$dylib_path" "$framework_dir/$FRAMEWORK_NAME"
        install_name_tool -id "@rpath/$FRAMEWORK_NAME.framework/$FRAMEWORK_NAME" "$framework_dir/$FRAMEWORK_NAME"
        cp "$HEADER_DIR"/*.h "$framework_dir/Headers/"
        write_modulemap "$framework_dir/Modules/module.modulemap"
        write_info_plist "$framework_dir/Info.plist"
    fi

    echo "$framework_dir"
}

echo "Creating macOS universal dylib..."
lipo -create \
    "target/$MACOS_ARM_TARGET/release/liblyte.dylib" \
    "target/$MACOS_X86_TARGET/release/liblyte.dylib" \
    -output "$BUILD_DIR/liblyte-macos.dylib"

echo "Creating framework bundles..."
MACOS_FW=$(create_framework "$BUILD_DIR/liblyte-macos.dylib" "macos")
IOS_FW=$(create_framework "target/$IOS_TARGET/release/liblyte.dylib" "ios")
IOS_SIM_FW=$(create_framework "target/$IOS_SIM_TARGET/release/liblyte.dylib" "ios-simulator")

echo "Creating XCFramework..."
xcodebuild -create-xcframework \
    -framework "$MACOS_FW" \
    -framework "$IOS_FW" \
    -framework "$IOS_SIM_FW" \
    -output "$XCFRAMEWORK"

echo "Created $XCFRAMEWORK"

echo "Zipping $XCFRAMEWORK..."
# -y preserves symlinks
zip -r -y "$XCFRAMEWORK.zip" "$XCFRAMEWORK"
echo "Created $XCFRAMEWORK.zip"
