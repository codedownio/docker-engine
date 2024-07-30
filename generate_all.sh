#!/usr/bin/env sh

SCRIPTDIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
cd "$SCRIPTDIR"

set -eo pipefail

rm -rf ./v*

nix run .#generate1_36
nix run .#generate1_37
nix run .#generate1_38
nix run .#generate1_39
nix run .#generate1_40
nix run .#generate1_41
nix run .#generate1_42
nix run .#generate1_43
nix run .#generate1_44
nix run .#generate1_45
nix run .#generate1_46
