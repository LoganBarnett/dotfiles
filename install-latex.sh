#! /usr/bin/env bash

set -e

echo "Installing latex support..."
# mactex adds latex support. pandoc requires latex to export to PDF amongst
# other things. It also installs dvipng needed for inline latex previews.
brew cask install mactex
# This makes pdflatex available for pandoc to use.
ln -snf /Library/TeX/texbin/pdflatex /usr/local/bin/pdflatex
ln -snf /Library/TeX/texbin/dvipng /usr/local/bin/dvipng
echo "[DONE] Installed latex support!"
