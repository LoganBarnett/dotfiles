#! /usr/bin/env bash

echo "[DOTFILES] Installing MongoDB..."

brew tap mongodb/brew

brew install mongodb-community

echo 'To start daemonized, use "brew services start mongodb-community".'
echo 'To start manually, use "mongod --config /usr/local/etc/mongod.conf"'

echo "[DOTFILES] MongoDB installed!"