#! /usr/bin/env bash

echo "Installing fonts..."
set -e
# set -x

rm -rf scratch
mkdir -p scratch
mkdir -p fonts
cd scratch

curl -s https://api.github.com/repos/IBM/plex/releases/latest \
  | jq -r '.assets[] | select(.name == "OpenType.zip").browser_download_url' \
  | wget -qi -

unzip OpenType.zip
# Files need to live in the Fonts directory, not just be symlinked. Once copied
# there will be a delay and then they will be automatically consumed.
echo "Fonts will cause a delay as the OS processes them."
find OpenType -name "*.otf" -exec mv -f {} ~/Library/Fonts \;
cd -


echo "Installing fonts done."