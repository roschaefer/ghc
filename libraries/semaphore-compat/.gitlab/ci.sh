#!/usr/bin/env bash
set -x
set -eo pipefail

: "${HACKAGE_STATE:=HEAD}"

runner_temp=$(mktemp -d)
# shellcheck disable=SC2064
trap "rm -rf ${runner_temp}" EXIT

export GHCUP_INSTALL_BASE_PREFIX=$runner_temp/foobarbaz
export BOOTSTRAP_HASKELL_NONINTERACTIVE=1
export BOOTSTRAP_HASKELL_MINIMAL=1
export BOOTSTRAP_HASKELL_ADJUST_BASHRC=1

curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# On Windows/MSYS2, ghcup installs to "ghcup/" (no dot prefix)
source "$GHCUP_INSTALL_BASE_PREFIX/.ghcup/env" 2>/dev/null \
  || source "$GHCUP_INSTALL_BASE_PREFIX/ghcup/env" 2>/dev/null \
  || export PATH="$GHCUP_INSTALL_BASE_PREFIX/ghcup/bin:$PATH"

ghcup --version
which ghcup | grep foobarbaz

ghcup install ghc --set $VERSION
ghcup install cabal

cabal update "hackage.haskell.org,${HACKAGE_STATE}"
cabal build
cabal test --test-show-details=streaming
