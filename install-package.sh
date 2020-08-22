#!/usr/bin/env bash

if [ $(uname) = 'Darwin' ]; then
  brew install $1 || brew upgrade $1
else
  apt install -y $1 || apt upgrade -y $1
fi
