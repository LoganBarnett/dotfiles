#! /usr/bin/env bash

set -e

./install-package.sh stack

stack upgrade
