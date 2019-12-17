#! /usr/bin/env bash

set -e
node ./bootstrap-code-injector.js
cd ../code-injector
yarn grunt
