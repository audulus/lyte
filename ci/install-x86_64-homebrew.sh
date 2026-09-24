#!/bin/bash
# Install an x86_64 Homebrew under /usr/local on an Apple Silicon machine.
#
# The xcframework's macOS x86_64 slice links against a native x86_64 LLVM,
# so the build needs an Intel Homebrew alongside the arm64 one at
# /opt/homebrew. Homebrew's install.sh refuses to run on Intel macOS since
# September 2026 ("Homebrew on macOS is only supported on Apple Silicon
# processors!"), including under Rosetta, but brew itself still runs there
# and Intel bottles are still published. So this does by hand what
# install.sh used to do: clone the brew repo into /usr/local/Homebrew,
# create the prefix directories, and link /usr/local/bin/brew.
#
# /usr/local is the standard Intel prefix, which keeps bottles usable
# without relocation. Requires passwordless sudo (GitHub runners have it).
#
# Usage: ci/install-x86_64-homebrew.sh [formula...]

set -euo pipefail

if [ "$(uname -m)" != "arm64" ]; then
    echo "Error: expected to run on an arm64 host" >&2
    exit 1
fi
if ! arch -x86_64 /usr/bin/true 2>/dev/null; then
    echo "Error: Rosetta is not available" >&2
    exit 1
fi

PREFIX=/usr/local
REPO="$PREFIX/Homebrew"

if [ ! -x "$REPO/bin/brew" ]; then
    sudo mkdir -p "$REPO"
    sudo chown "$(id -u):$(id -g)" "$REPO"
    git clone --depth 1 https://github.com/Homebrew/brew "$REPO"
fi

# The directories install.sh creates and makes writable for the user.
for d in bin etc include lib sbin share var opt Cellar Caskroom Frameworks \
         var/homebrew var/homebrew/linked share/zsh share/zsh/site-functions; do
    sudo mkdir -p "$PREFIX/$d"
    sudo chown "$(id -u):$(id -g)" "$PREFIX/$d"
done

# On arm64 runner images /usr/local/bin/brew may already point at the arm64
# brew; replace it, as install.sh did, so /usr/local/bin/brew is the Intel one.
ln -sfn "$REPO/bin/brew" "$PREFIX/bin/brew"

export HOMEBREW_NO_AUTO_UPDATE=1 HOMEBREW_NO_ANALYTICS=1 HOMEBREW_NO_ENV_HINTS=1
arch -x86_64 "$PREFIX/bin/brew" config | grep -E "^(HOMEBREW_PREFIX|macOS|Rosetta 2):" || true

if [ $# -gt 0 ]; then
    arch -x86_64 "$PREFIX/bin/brew" install "$@"
fi
