#! /usr/bin/env bash

mkdir -p ~/.ssh
chmod 700 ~/.ssh
# tell it not to overwrite the file if it exists using echo
echo -e 'n\n' | ssh-keygen -t rsa -f ~/.ssh/id_rsa -N ""
