#! /usr/bin/env bash

set -e
echo "installing gems"
GEMS="jekyll bundler"
# 10.12+ needs a sudoer to install gems. All of my systems are on 10.12+ so just
# sudo by default.
echo "Need sudoer access to install gems."
sudo gem install $GEMS

echo "github-pages needs a specific version to be compatible with older ruby"
sudo gem install github-pages -v 156
