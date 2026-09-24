#!/bin/bash
# Prepare a native LLVM prefix for macOS from the official LLVM release
# binaries, suitable for llvm-sys (LLVM_SYS_191_PREFIX) and for static
# linking into the xcframework. Prints the prefix path on stdout.
#
# Usage: ci/prepare-llvm.sh <arm64|x86_64>
#
# Environment:
#   LYTE_LLVM_CACHE           where downloads and prefixes live
#                             (default: ~/Library/Caches/lyte/llvm)
#   MACOSX_DEPLOYMENT_TARGET  minimum macOS for the generated code (default 15.0)
#
# Why this exists instead of pointing llvm-sys at the tarball directly:
#
#  1. The official macOS release archives contain LTO bitcode, not machine
#     code: every member of every libLLVM*.a is an LLVM bitcode file. Linking
#     them as-is would push LTO of all of LLVM onto every consumer of the
#     xcframework at their link step, and require Apple's linker to read
#     upstream bitcode. So each bitcode object is compiled to a native object
#     with the release's own clang, at -O2 and the deployment target, and the
#     archives are repacked. The result is an ordinary static LLVM.
#
#  2. The release was linked against a Homebrew zstd, and its llvm-config
#     reports that dependency as an absolute path (/opt/homebrew/lib/libzstd.a
#     or /usr/local/lib/libzstd.a), which llvm-sys refuses unless the file
#     exists. The prefix gets a libzstd.a built from source for the target
#     arch, and an llvm-config wrapper that reports -lzstd instead.
#
# Previously LLVM came from Homebrew bottles, which carry the minimum OS of
# whatever macOS built them and, since September 2026, cannot be installed
# for x86_64 at all.

set -euo pipefail

LLVM_VERSION="19.1.7"
ZSTD_VERSION="1.5.7"

# From the sigstore attestations published alongside the release assets
# (LLVM-*.tar.xz.jsonl) and zstd-*.tar.gz.sha256 respectively.
LLVM_ARM64_SHA256="d93bf12952d89fe4ec7501c40475718b722407da6a8d651f05c995863468e570"
LLVM_X64_SHA256="49405e75fbe7ad6f8139a33f59ec8c5112b75b3027405c7b92d19f4c6f02c78a"
ZSTD_SHA256="eb33e51f49a15e023950cd7825ca74a4a2b43db8354825ac24fc1b7ee09e6fa3"

ARCH="${1:-}"
case "$ARCH" in
    arm64)  ASSET="LLVM-$LLVM_VERSION-macOS-ARM64"; LLVM_SHA256="$LLVM_ARM64_SHA256" ;;
    x86_64) ASSET="LLVM-$LLVM_VERSION-macOS-X64";   LLVM_SHA256="$LLVM_X64_SHA256" ;;
    *) echo "Usage: $0 <arm64|x86_64>" >&2; exit 1 ;;
esac

MINOS="${MACOSX_DEPLOYMENT_TARGET:-15.0}"
CACHE="${LYTE_LLVM_CACHE:-$HOME/Library/Caches/lyte/llvm}"
TARBALL="$CACHE/$ASSET.tar.xz"
EXTRACTED="$CACHE/$ASSET"
NATIVE="$CACHE/$ASSET-native-$MINOS"
JOBS="$(sysctl -n hw.ncpu 2>/dev/null || echo 4)"

log() { echo "prepare-llvm[$ARCH]: $*" >&2; }

if [ -f "$NATIVE/.complete" ]; then
    echo "$NATIVE"
    exit 0
fi

mkdir -p "$CACHE"

verify_sha256() {
    local file="$1" expected="$2" actual
    actual="$(shasum -a 256 "$file" | awk '{print $1}')"
    if [ "$actual" != "$expected" ]; then
        log "checksum mismatch for $file"
        log "  expected $expected"
        log "  actual   $actual"
        rm -f "$file"
        exit 1
    fi
}

fetch() {
    local url="$1" file="$2" sha="$3"
    if [ ! -f "$file" ]; then
        log "downloading $(basename "$file")"
        curl -fsSL --retry 3 -o "$file.part" "$url"
        mv "$file.part" "$file"
    fi
    verify_sha256 "$file" "$sha"
}

# --- 1. Official LLVM release ---------------------------------------------

fetch "https://github.com/llvm/llvm-project/releases/download/llvmorg-$LLVM_VERSION/$ASSET.tar.xz" \
      "$TARBALL" "$LLVM_SHA256"

if [ ! -f "$EXTRACTED/.complete" ]; then
    log "extracting $ASSET (llvm-config, clang, headers, static archives)"
    rm -rf "$EXTRACTED"
    # Only the archives llvm-sys will link (libLLVM*.a plus Polly, which the
    # release build links in), not the gigabyte of clang/lld/lldb libraries
    # alongside them. bsdtar matches member arguments as glob patterns.
    tar -xJf "$TARBALL" -C "$CACHE" \
        "$ASSET/bin/llvm-config" "$ASSET/bin/clang" "$ASSET/bin/clang-$(echo "$LLVM_VERSION" | cut -d. -f1)" \
        "$ASSET/include" "$ASSET/lib/libLLVM*.a" "$ASSET/lib/libPolly*.a"
    touch "$EXTRACTED/.complete"
fi

CLANG="$EXTRACTED/bin/clang"
"$CLANG" --version >/dev/null
# llvm-config checks that every archive it names exists, so this also fails
# loudly if a future release links in something not extracted above.
STATIC_LIBS="$("$EXTRACTED/bin/llvm-config" --link-static --libnames)"

# --- 2. zstd from source ---------------------------------------------------

ZSTD_TARBALL="$CACHE/zstd-$ZSTD_VERSION.tar.gz"
fetch "https://github.com/facebook/zstd/releases/download/v$ZSTD_VERSION/zstd-$ZSTD_VERSION.tar.gz" \
      "$ZSTD_TARBALL" "$ZSTD_SHA256"

rm -rf "$NATIVE"
mkdir -p "$NATIVE/bin" "$NATIVE/lib"

WORK="$(mktemp -d "${TMPDIR:-/tmp}/prepare-llvm.XXXXXX")"
trap 'rm -rf "$WORK"' EXIT

log "building zstd $ZSTD_VERSION for $ARCH"
tar -xzf "$ZSTD_TARBALL" -C "$WORK" "zstd-$ZSTD_VERSION/lib"
ZSTD_SRC="$WORK/zstd-$ZSTD_VERSION/lib"
mkdir -p "$WORK/zstd-obj"
# The system clang has the SDK wired up; -arch cross-compiles for x86_64.
# ZSTD_DISABLE_ASM avoids the x86_64-only assembly so both arches build the
# same way; LLVM only needs the plain compress/decompress API.
find "$ZSTD_SRC/common" "$ZSTD_SRC/compress" "$ZSTD_SRC/decompress" -name '*.c' -print0 \
  | xargs -0 -P "$JOBS" -I{} sh -c \
      'xcrun clang -arch "$1" -mmacosx-version-min="$2" -O3 -DZSTD_DISABLE_ASM -DZSTD_MULTITHREAD=0 -I"$3" -I"$3/common" -c "$4" -o "$5/$(basename "$4" .c).o"' \
      _ "$ARCH" "$MINOS" "$ZSTD_SRC" {} "$WORK/zstd-obj"
xcrun libtool -static -o "$NATIVE/lib/libzstd.a" "$WORK/zstd-obj"/*.o

# --- 3. Bitcode -> native objects -----------------------------------------

log "compiling LLVM bitcode to native $ARCH objects (minos $MINOS, $JOBS jobs)"
mkdir -p "$WORK/objs"
LIST="$WORK/list.txt"
: > "$LIST"
for lib in $STATIC_LIBS; do
    archive="$EXTRACTED/lib/$lib"
    name="${lib%.a}"
    mkdir -p "$WORK/objs/$name"
    (cd "$WORK/objs/$name" && ar x "$archive")
    rm -f "$WORK/objs/$name/__.SYMDEF"*
    for obj in "$WORK/objs/$name"/*.o; do
        echo "$obj" >> "$LIST"
    done
done

# Each bitcode object is compiled in place. -O2 matters: the IR was already
# optimized by the release build, but code generation quality (register
# allocation, scheduling) still follows the level given here.
convert_one() {
    local obj="$1"
    mv "$obj" "$obj.bc"
    "$CLANG" -x ir -c -O2 -target "$ARCH-apple-macosx$MINOS" -Wno-override-module \
        "$obj.bc" -o "$obj"
    rm -f "$obj.bc"
}
export -f convert_one
export CLANG ARCH MINOS
if ! xargs -P "$JOBS" -n 1 bash -c 'convert_one "$0"' < "$LIST"; then
    log "bitcode compilation failed"
    exit 1
fi

log "repacking archives"
for dir in "$WORK"/objs/*/; do
    name="$(basename "$dir")"
    xcrun libtool -static -no_warning_for_no_symbols -o "$NATIVE/lib/$name.a" "$dir"/*.o
done

# --- 4. Prefix layout for llvm-sys ----------------------------------------

# llvm-config derives its prefix from its own location, so a copy under
# $NATIVE/bin reports $NATIVE/lib and $NATIVE/include on its own. The wrapper
# only fixes --system-libs (see the header comment).
cp "$EXTRACTED/bin/llvm-config" "$NATIVE/bin/llvm-config.real"
cat > "$NATIVE/bin/llvm-config" <<'WRAP'
#!/bin/bash
"$(dirname "$0")/llvm-config.real" "$@" | sed -E 's#(/opt/homebrew|/usr/local)/lib/libzstd\.a#-lzstd#g'
exit "${PIPESTATUS[0]}"
WRAP
chmod +x "$NATIVE/bin/llvm-config"
# llvm-sys compiles a small C wrapper against the C API, which needs only
# llvm-c/ and llvm/Config/; the prefix stays self-contained (no symlinks
# into the extracted tree), so CI can cache it on its own.
mkdir -p "$NATIVE/include/llvm"
cp -R "$EXTRACTED/include/llvm-c" "$NATIVE/include/llvm-c"
cp -R "$EXTRACTED/include/llvm/Config" "$NATIVE/include/llvm/Config"

"$NATIVE/bin/llvm-config" --version >/dev/null
touch "$NATIVE/.complete"
log "ready: $NATIVE"
echo "$NATIVE"
