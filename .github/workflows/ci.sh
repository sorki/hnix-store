#!/usr/bin/env bash
# Script by @fisx

set -eo pipefail
cd "$( dirname "${BASH_SOURCE[0]}" )"


# based on https://github.com/vmchale/github-actions-dhall
which dhall-to-yaml || cabal install dhall-yaml
echo "regenerating ci.yaml..."
dhall-to-yaml --file ubuntu-ci.dhall > ubuntu-ci.yaml
echo "regenerating macos-ci.yaml..."
dhall-to-yaml --file macos-ci.dhall > macos-ci.yaml
