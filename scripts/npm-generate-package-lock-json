#!/usr/bin/env bash
################################################################################
# Generate a package-lock.json file from a recalcitrant npm package that doesn't
# include its own package-lock.json (most of them don't - a questionable
# community choice).
#
# Takes a URL to the package tarball, and its name.  Example:
# npm-generate-package-lock-json https://registry.npmjs.org/@github/copilot/-/copilot-0.0.342.tgz copilot
# This generates a copilot-package-lock.json which can be used to provide
# overrides for package versions.
################################################################################

set -euo pipefail

# TODO: Add the 100 or so lines to parse args.
url="${1:-}"
name="${2:-}"

dir="$(mktemp -d)"
pushd "$dir"
curl --silent --location --output npm-pkg.tgz "$url"
tar --extract --file npm-pkg.tgz
ls -al
pushd package
ls -al
npm install --package-lock-only --ignore-scripts --no-audit --no-fund
ls -al
popd
popd

cp "$dir/package/package-lock.json" "${name}-package-lock.json"

rm -rf "$dir"
