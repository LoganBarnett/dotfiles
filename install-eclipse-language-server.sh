#! /usr/bin/env bash

# At time of writing, there is no official Homebrew package for this. There is
# an unofficial one though here:
# https://github.com/nossralf/homebrew-jdt-language-server/blob/master/jdt-language-server.rb

set -x

install_dir=/usr/local/opt/eclipse-language-server
mkdir -p $install_dir
cd $install_dir

wget -qO- \
    http://download.eclipse.org/jdtls/snapshots/jdt-language-server-0.20.0-201805230915.tar.gz \
    | bsdtar -xvf- \
    #
