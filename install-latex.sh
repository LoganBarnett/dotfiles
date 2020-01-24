#! /usr/bin/env bash

set -e

echo "Installing latex support..."
# basictex adds latex support. pandoc requires latex to export to PDF amongst
# other things.
brew cask install basictex
# This makes pdflatex available for pandoc to use.
ln -snf /Library/TeX/texbin/pdflatex /usr/local/bin/pdflatex
echo "[DONE] Installed latex support!"
