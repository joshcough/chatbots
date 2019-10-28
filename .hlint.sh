#!/bin/bash

#
# Usage: ./hlint.sh [service ...]
#
# Examples:
#   ./hlint.sh                 # all services
#   ./hlint.sh apiary          # one service
#   ./hlint.sh trellis apiary  # multiple services
#

set -eu

HLINT_VERSION=2.1.10
ROOT=.hlint-$HLINT_VERSION
HLINT=$ROOT/hlint-$HLINT_VERSION/hlint

function get_os () {
    case "$OSTYPE" in
    darwin*) echo "osx" ;;
    *)       echo "linux" ;;
    esac
}

function download () {
    if [[ ! -d "$ROOT" ]]; then
        echo "=======> Download hlint-$HLINT_VERSION..."
        mkdir -p "$ROOT"

        local hlint_release
        local hlint_url
        hlint_release="v$HLINT_VERSION/hlint-$HLINT_VERSION-x86_64-$(get_os).tar.gz"
        hlint_url="https://github.com/ndmitchell/hlint/releases/download/$hlint_release"
        curl --progress-bar --location --output "$ROOT/hlint.tar.gz" "$hlint_url"
        tar -xzf "$ROOT/hlint.tar.gz" -C "$ROOT"
        rm "$ROOT/hlint.tar.gz"
    fi
}

# Returns list of all of our Haskell services, e.g.
# apiary bean ...
function get_all_services () {
    find * \( -name '*.cabal' -o -name 'package.yaml' \) \
      ! -path '*.stack-work/*' -exec dirname {} ';' | sort | uniq
}

function run_hlint () {
  if [[ -f "$1"/.hlint.yaml ]]; then
    echo "=======> Lint $1 ($1/.hlint.yaml...)"
    "$HLINT" "$1" --hint="$1"/.hlint.yaml
  else
    echo "=======> Lint $1..."
    "$HLINT" "$1"
  fi
}

EXIT_CODE=0
function error_handler () {
    EXIT_CODE=1
}

# Main
download
echo "=======> $($HLINT --version)"

SERVICES=${*:-$(get_all_services)}

trap error_handler ERR
for service in $SERVICES; do
    set +e
    run_hlint "$service"
    echo ""
    set -e
done

exit $EXIT_CODE
