#! /usr/bin/env bash

set -e

echo "Installing AWS tools..."
if [ $(uname) = 'Darwin' ]; then
    brew tap versent/homebrew-taps
    ./install-package.sh saml2aws
    echo "Done installing AWS tools."
else
    2> echo "Other platforms are not supported. Doing nothing for AWS tools."
fi

